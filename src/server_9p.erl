-module(server_9p).

-callback init([any()]) ->
  {ok, State :: any()} | {ok, State :: any(), MsgSize :: integer()}.

-callback handle_9p(Type::integer(), Message::any()) ->
  {ok | error, Data :: iodata(), State :: any()}.

-callback handle_call(Request::any(), From::any(), State::any()) ->
  {Reply::any(), NewState::any()}.

-callback handle_cast(Msg::any(), State::any()) ->
  NewState::any().

-callback handle_info(Info::any(), State::any()) ->
  NewState::any().

-callback terminate(Reason::any(), State::any()) ->
  ok.
