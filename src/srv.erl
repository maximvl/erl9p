-module(srv).

-behaviour(server_9p).
-include("9p.hrl").

-export([init/1]).

init(_Props) ->
  {ok, []}.
