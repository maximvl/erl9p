-module(server_9p).

-callback init([any()]) ->
  {ok, State :: any()} | {ok, State :: any(), MsgSize :: integer()}.

-callback session_init(Version::binary(), State::any()) ->
  State::any().

-callback handle_9p(Type::integer(), Message::any(), State::any()) ->
  {reply, Data :: iodata(), State :: any} | {noreply, State :: any()}.

-callback terminate(Reason::any(), State::any()) ->
  State::any().
