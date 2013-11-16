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

-include("9p.hrl").

%% API
-export([start_link/4, init/4]).

-define(SERVER, ?MODULE).

-define(PVERSION, <<"9P2000">>).
-define(MAX_SIZE, 8192).

-record(state, {socket        :: ranch:socket(),
                transport     :: ranch:transport(),
                handler       :: module(),
                handler_state :: any(),
                max_size      :: pos_integer(),
                msg_size      :: pos_integer(),
                session       :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
  Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Ref, Socket, Transport, Opts) ->
  Handler = proplists:get_value(handler, Opts, server_impl),
  MSize = proplists:get_value(max_size, Opts, ?MAX_SIZE),
  {HState2, MSize2} = case Handler:init(Opts) of
                        {ok, HState} ->
                          {HState, ?MAX_SIZE};
                        {ok, HState, MSize} ->
                          {HState, MSize}
                      end,
  ok = ranch:accept_ack(Ref),
  Transport:setopts(Socket, [{recbuf, ?MAX_SIZE}, {sndbuf, ?MAX_SIZE}, {active, true}]),
  try wait_request(#state{socket=Socket,
                          transport=Transport,
                          handler=Handler,
                          handler_state=HState2,
                          max_size=MSize2,
                          session=false})
  catch E:V ->
         error_logger:error_report([{E, V},
                                    {trace, erlang:get_stacktrace()}])
  end.

wait_request(State) ->
  wait_request(<<>>, State).

wait_request(<<>>, #state{socket=S}=State) ->
  receive
    {_Transport, S, Data} ->
      <<MSize:32/little-integer,Rest/binary>> = Data,
      wait_request(Rest, State#state{msg_size=MSize-4});
    Other ->
      terminate(Other, State)
  end;

wait_request(Buffer, #state{socket=S,
                            msg_size=MSize}=State) when
    byte_size(Buffer) < MSize ->
  receive
    {_Transport, S, Data} ->
      AllData = <<Buffer/binary, Data/binary>>,
      wait_request(AllData, State);
    Other ->
      terminate(Other, State)
  end;

%% we have complete message here
wait_request(Buffer, #state{msg_size=MSize}=State) when
    byte_size(Buffer) == MSize ->
  parse_request(Buffer, State).

parse_request(Data, #state{msg_size=MSize}=State) ->
  case make_message(MSize, Data) of
    {ok, Msg} ->
      parse_message(Msg, State);
    false ->
      terminate({"cant make message from binary", Data}, State)
  end.

%% first message should be session start
parse_message({?Tversion, Tag, Data}, #state{session=false,
                                             max_size=MSize,
                                             transport=T,
                                             socket=S}=State) ->
  {CSize, _V} = lib9p:parse_message(?Tversion, Data),
  MinSize = min(MSize, CSize),
  Resp = lib9p:pack_message(?Rversion, Tag, {MinSize, ?PVERSION}),
  T:send(S, Resp),
  T:setopts(S, [{packet_size, MinSize}]),
  wait_request(State#state{session = true, max_size = MinSize});

parse_message({Type, Tag, Data}, #state{session=true,
                                        transport=T,
                                        socket=S,
                                        handler=H,
                                        handler_state=Hs}=State) ->
  {Reply, NHs} = handle_message(Type, Tag, Data, H, Hs),
  T:send(S, Reply),
  wait_request(State#state{handler_state=NHs});

parse_message(_, State) ->
  terminate("session was not started with TVersion", State).

terminate(Reason, #state{transport=T, socket=S, handler=H, handler_state=HS}) ->
  H:terminate(Reason, HS),
  error_logger:error_report(["9p terminating", {reason, Reason}]),
  T:close(S).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_message(Size::pos_integer(), Msg::binary()) -> {ok, Type::integer(),
                                                           Tag::binary(),
                                                           Data::binary()} | false.
make_message(Size, <<Type:8/little-integer,
                     Tag:2/binary,
                     Data/binary>>) when byte_size(Data) == Size-3 ->
  {ok, {Type, Tag, Data}};

make_message(_, _) ->
  false.

-spec handle_message(Type::integer(), Tag::binary(),
                     Data::binary(), Handler::module(),
                     HState::any()) -> {Resp::iodata(), NHState::any()}.
handle_message(Type, Tag, Data, Handler, HState) ->
  case lib9p:parse_message(Type, Data) of
    false ->
      Msg = lib9p:pack_message(?Rerror, Tag, <<"cant parse message">>),
      {Msg, HState};
    Parsed ->
      case Handler:handle_9p(Type, Parsed, HState) of
        {ok, Reply, NHState} ->
          {lib9p:pack_message(Type+1, Tag, Reply), NHState};
        {error, Emsg, NHState} ->
          {lib9p:pack_message(?Rerror, Tag, Emsg), NHState}
      end
  end.
