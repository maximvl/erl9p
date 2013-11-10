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
-export([init/1, handle_9p/3, terminate/2]).

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

handle_9p(?Tauth, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tflush, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tattach, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tclunk, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Twalk, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Topen, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tcreate, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tread, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Twrite, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tremove, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Tstat, _Msg, State) ->
  {ok, <<>>, State};
handle_9p(?Twstat, _Msg, State) ->
  {ok, <<>>, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
