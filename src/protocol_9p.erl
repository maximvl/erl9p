%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2013, maxvel
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2013 by maxvel <>
%%%-------------------------------------------------------------------
-module(protocol_9p).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("9p.hrl").

%% API
-export([start_link/4, async_reply/4]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(PVERSION, <<"9P2000">>).
-define(MAX_SIZE, 8192).

-record(state, {socket        :: ranch:socket(),
                transport     :: ranch:transport(),
                handler       :: module(),
                handler_state :: any(),
                max_size      :: pos_integer(),
                buffer        :: iodata(),
                msg_size      :: undefined | pos_integer(),
                session       :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
  gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, Socket, Transport, Opts]) ->
  Handler = proplists:get_value(handler, Opts, server_impl),
  MSize = proplists:get_value(max_size, Opts, ?MAX_SIZE),
  {HState2, MSize2} = case Handler:init(Opts) of
                        {ok, HState} ->
                          {HState, ?MAX_SIZE};
                        {ok, HState, MSize} ->
                          {HState, MSize}
                      end,
  gen_server:cast(self(), {do_init, Ref}),
  {ok, #state{socket=Socket,
              transport=Transport,
              handler=Handler,
              handler_state=HState2,
              max_size=MSize2,
              buffer= <<>>,
              msg_size=undefined,
              session=false}}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast({do_init, Ref}, #state{transport=T,
                                   socket=S}=State) ->
  ok = ranch:accept_ack(Ref),
  T:setopts(S, [{recbuf, ?MAX_SIZE}, {sndbuf, ?MAX_SIZE}, {active, true}]),
  {noreply, State};

handle_cast({reply9p, Reply}, #state{socket=S,
                                     transport=T}=State) ->
  T:send(S, Reply),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_Trans, S, Data}, #state{socket=S, buffer=Buff}=State) ->
  FullData = <<Buff/binary, Data/binary>>,
  NState = maybe_process_message(State#state{buffer=FullData}),
  {noreply, NState};

handle_info({tcp_closed, S}, #state{socket=S}=State) ->
  {stop, {shutdown, tcp_closed}, State};

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(Reason, #state{transport=T, socket=S, handler=H, handler_state=HS}) ->
  H:terminate(Reason, HS),
  error_logger:error_report(["9p terminating", {reason, Reason}]),
  T:close(S).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_process_message(#state{}) -> #state{}.
maybe_process_message(#state{msg_size=undefined,
                             buffer=Buff}=State) ->
  case Buff of
    <<MSize:32/little-integer, Rest/binary>> ->
      maybe_process_message(State#state{msg_size=MSize-4,
                                        buffer=Rest});
    _ ->
      State
  end;
maybe_process_message(#state{msg_size=MSize,
                             buffer=Buff}=State) when
    byte_size(Buff) < MSize ->
  State;
maybe_process_message(#state{msg_size=MSize,
                             buffer=Buff}=State) when
    byte_size(Buff) == MSize ->
  <<Type:8/little-integer, Tag:2/binary, Msg/binary>> = Buff,
  Payload = {Type, Tag, Msg},
  process_message(Payload, State#state{msg_size=undefined, buffer= <<>>});
maybe_process_message(#state{msg_size=MSize,
                             buffer=Buff}=State) when
    byte_size(Buff) > MSize ->
  <<Msg1:MSize/binary, Rest/binary>> = Buff,
  NState = maybe_process_message(State#state{buffer=Msg1}),
  maybe_process_message(NState#state{buffer=Rest,
                                     msg_size=undefined}).

%% first message should be session start
-spec process_message(Msg::tuple(), #state{}) -> #state{}.
process_message({?Tversion, Tag, Data}, #state{session=false,
                                               max_size=MSize,
                                               transport=T,
                                               socket=S,
                                               handler=H,
                                               handler_state=Hs}=State) ->
  {CSize, ClientVers} = lib9p:parse_message(?Tversion, Data),
  NHs = H:session_init(ClientVers, Hs),
  MinSize = min(MSize, CSize),
  Resp = lib9p:pack_message(?Rversion, Tag, {MinSize, ?PVERSION}),
  T:send(S, Resp),
  T:setopts(S, [{packet_size, MinSize}]),
  State#state{session=true, max_size=MinSize, handler_state=NHs};

%% restart session
process_message({?Tversion, Tag, Data}, #state{session=true}=State) ->
  process_message({?Tversion, Tag, Data}, State#state{session=false});

process_message({Type, Tag, Data}, #state{session=true,
                                          transport=T,
                                          socket=S,
                                          handler=H,
                                          handler_state=Hs}=State) ->
  case handle_message(Type, Tag, Data, H, Hs) of
    {reply, Reply, NHs} ->
      T:send(S, Reply),
      State#state{handler_state=NHs};
    {noreply, NHs} ->
      State#state{handler_state=NHs}
  end;

process_message(_, State) ->
  erlang:error("session was not started with TVersion", State).

-spec handle_message(Type::byte(), Tag::binary(),
                     Data::binary(), Handler::module(),
                     HState::any()) ->
                        {reply, Resp::iodata(), NHState::any()} | {noreply, NHState::any()}.
handle_message(Type, Tag, Data, Handler, HState) ->
  case lib9p:parse_message(Type, Data) of
    false ->
      Msg = lib9p:pack_message(?Rerror, Tag, <<"cant parse message">>),
      {reply, Msg, HState};
    Parsed ->
      case Handler:handle_9p(Type, Parsed, HState) of
        {reply, Reply, NHState} ->
          {reply, lib9p:pack_message(Type+1, Tag, Reply), NHState};
        {error, Emsg, NHState} ->
          {reply, lib9p:pack_message(?Rerror, Tag, Emsg), NHState};
        {noreply, NHState} ->
          {noreply, NHState}
      end
  end.

async_reply(Pid, error, Tag, Msg) ->
  Resp = lib9p:pack_message(?Rerror, Tag, Msg),
  gen_server:cast(Pid, {reply9p, Resp});

async_reply(Pid, Type, Tag, Reply) ->
  Resp = lib9p:pack_message(Type+1, Tag, Reply),
  gen_server:cast(Pid, {reply9p, Resp}).
