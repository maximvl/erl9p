-module(erl9p_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ranch:start_listener(
              tcp_echo, 100,
              ranch_tcp, [{port, 5555}, {active, true}],
              echo_protocol, []),
  erl9p_sup:start_link().

stop(_State) ->
  ok.
