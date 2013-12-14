-module(erl9p_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ranch:start_listener(
              erl9p, 10,
              ranch_tcp,
              [{port, 564}, {active, true}],
              protocol_9p, []),
  erl9p_sup:start_link().

stop(_State) ->
  ok.
