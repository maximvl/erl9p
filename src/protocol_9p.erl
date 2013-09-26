%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2013, maxvel
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2013 by maxvel <>
%%%-------------------------------------------------------------------
-module(protocol_9p).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("9p.hrl").

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(PVERSION, <<"9P2000">>).
-define(MSG_SIZE, 8192).

-record(state, {socket        :: ranch:socket(),
                transport     :: ranch:transport(),
                handler       :: module(),
                handler_state :: any(),
                msg_size      :: pos_integer(),
                session       :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Ref, Socket, Transport, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, Socket, Transport, Opts]) ->
  {ok, Handler} = proplists:get_value(handler, Opts),
  MSize = proplists:get_value(msg_size, Opts, ?MSG_SIZE),
  {HState2, MSize2} = case Handler:init(Opts) of
                        {ok, HState} ->
                          {HState, ?MSG_SIZE};
                        {ok, HState, MSize} ->
                          {HState, MSize}
                      end,
  ok = ranch:accept_ack(Ref),
  Transport:setopts(Socket, [{recbuf, ?MSG_SIZE}, {sndbuf, ?MSG_SIZE}]),
  {ok, #state{socket=Socket,
              transport=Transport,
              handler=Handler,
              handler_state=HState2,
              msg_size=MSize2,
              session=false}}.

handle_call(Request, From, #state{handler=H, handler_state=Hs}=State) ->
  {Reply, NHs} = H:handle_call(Request, From, Hs),
  {reply, Reply, State#state{handler_state=NHs}}.

handle_cast(Msg, #state{handler=H, handler_state=Hs}=State) ->
  NHs = H:handle_cast(Msg, Hs),
  {noreply, State#state{handler_state=NHs}}.

handle_info({tcp, S, Data}, #state{session=true,
                                   transport=T,
                                   socket=S,
                                   handler=H,
                                   handler_state=Hs}=State) ->
  case make_message(Data) of
    {ok, Type, Tag, Data} ->
      {Reply, NHs} = handle_message(Type, Tag, Data, H, Hs),
      T:send(S, Reply),
      {noreply, State#state{handler_state=NHs}};
    _ ->
      T:close(S),
      {stop, normal}
  end;

%% TVersion message must start session
handle_info({tcp, S, Data}, #state{transport=T,
                                   socket=S,
                                   msg_size=MSize}=State) ->
  case make_message(Data) of
    {ok, ?TVersion, Tag, Data} ->
      {CSize, _V} = lib9p:parse_message(?TVersion, Data),
      MinSize = min(MSize, CSize),
      Resp = lib9p:pack_message(?RVersion, Tag, {MinSize, ?PVERSION}),
      T:send(S, Resp),
      T:setopts(S, [{packet_size, MinSize}]),
      {noreply, State#state{msg_size=MinSize, session=true}};
    _ ->
      T:close(S),
      {stop, normal}
  end;

handle_info(Info, #state{handler=H, handler_state=Hs}=State) ->
  NHs = H:handle_info(Info, Hs),
  {noreply, State#state{handler_state=NHs}}.

terminate(Reason, #state{transport=T,
                         socket=S,
                         handler=H,
                         handler_state=Hs}) ->
  H:terminate(Reason, Hs),
  T:close(S).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_message(Msg::binary()) -> {ok, Type::integer(),
                                      Tag::binary(),
                                      Data::binary()} | false.
make_message(<<Size:32/little-integer,
               Type:8/little-integer,
               Tag:2/binary,
               Data/binary>>) when byte_size(Data) == Size-7 ->
  {ok, Type, Tag, Data};

make_message(_) ->
  false.

-spec handle_message(Type::integer(), Tag::binary(),
                     Data::binary(), Handler::module(),
                     HState::any()) -> {Resp::iodata(), NHState::any()}.
handle_message(Type, Tag, Data, Handler, HState) ->
  case lib9p:parse_message(Type, Data) of
    false ->
      Msg = lib9p:pack_message(?RError, Tag, <<"cant parse message">>),
      {Msg, HState};
    Parsed ->
      case Handler:handle_9p(Type, Parsed, HState) of
        {ok, Reply, NHState} ->
          {lib9p:pack_message(Type+1, Tag, Reply), NHState};
        {error, Emsg, NHState} ->
          {lib9p:pack_message(?RError, Tag, Emsg), NHState}
      end
  end.
