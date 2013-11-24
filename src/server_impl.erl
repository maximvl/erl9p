%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2013, maxvel
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2013 by maxvel <>
%%%-------------------------------------------------------------------
-module(server_impl).

-behaviour(server_9p).

%% gen_9p callbacks
-export([init/1, session_init/2, handle_9p/3, terminate/2]).

-include("9p.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
  {ok, #state{}}.

session_init(V, State) ->
  io:format("new session: ~p~n", [V]),
  State.

handle_9p(?Tauth, _Msg, State) ->
  io:format("auth: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tflush, _Msg, State) ->
  io:format("flush: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tattach, _Msg, State) ->
  io:format("attach: ~p~n", [_Msg]),
  Qid = {?FileType, 1, 1},
  {reply, Qid, State};

handle_9p(?Tclunk, _Msg, State) ->
  io:format("clunk: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Twalk, _Msg, State) ->
  io:format("walk: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Topen, _Msg, State) ->
  io:format("open: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tcreate, _Msg, State) ->
  io:format("create: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tread, _Msg, State) ->
  io:format("read: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Twrite, _Msg, State) ->
  io:format("write: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tremove, _Msg, State) ->
  io:format("remove: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tstat, _Msg, State) ->
  io:format("stat: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Twstat, _Msg, State) ->
  io:format("wstat: ~p~n", [_Msg]),
  {reply, <<>>, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
