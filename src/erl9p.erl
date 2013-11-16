-module(erl9p).

-export([start/0, stop/0]).

start() ->
  application:start(ranch),
  application:start(erl9p).

stop() ->
  application:stop(erl9p).
