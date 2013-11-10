-module(server_9p).

-callback init([any()]) ->
  {ok, State :: any()} | {ok, State :: any(), MsgSize :: integer()}.

-callback handle_9p(Type::integer(), Message::any(), State::any()) ->
  {ok | error, Data :: iodata(), State :: any()}.

-callback terminate(Reason::any(), State::any()) ->
  ok.
