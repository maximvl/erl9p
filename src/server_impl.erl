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

-record(state, {ns :: namespace:namespace(),
                fids :: dict()}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
  Mode = ?DirMode bor 8#777,
  Root = file9p:make(?DirType, <<>>, Mode, <<"root">>, <<"root">>),
  Ns = namespace:make(<<"">>, Root),
  {ok, #state{ns=Ns, fids=dict:new()}}.

session_init(V, State) ->
  io:format("new session: ~p~n", [V]),
  State.

handle_9p(?Tauth, _Msg, State) ->
  io:format("auth: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tflush, _Msg, State) ->
  io:format("flush: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tattach, {Fid, _AFid, _Uname, _Aname}=Msg,
          #state{ns=Ns, fids=Fids}=State) ->
  io:format("attach: ~p~n", [Msg]),
  Root = namespace:get_root(Ns),
  Fids2 = dict:store(Fid, file9p:path(Root), Fids),
  {reply, file9p:qid(Root), State#state{fids=Fids2}};

handle_9p(?Tclunk, Fid, #state{fids=Fids}=State) ->
  io:format("clunk: ~p~n", [Fid]),
  Fids2 = dict:erase(Fid, Fids),
  {reply, <<>>, State#state{fids=Fids2}};

handle_9p(?Twalk, {Fid, NFid, Names}, #state{fids=Fids,
                                             ns=Ns}=State) ->
  {ok, Qid} = dict:find(Fid, Fids),
  {ok, Cur} = namespace:get(Ns, Qid),
  {ok, File} = namespace:find_child_with_name(Ns, Cur, Names),
  Fids2 = dict:store(NFid, file9p:path(File), Fids),
  io:format("walk: ~p -> ~p : ~p~n", [Fid, NFid, Names]),
  {reply, <<>>, State#state{fids=Fids2}};

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

handle_9p(?Tstat, Fid, #state{ns=Ns, fids=Fids}=State) ->
  io:format("stat: ~p~n", [Fid]),
  Path = dict:fetch(Fid, Fids),
  {ok, File} = namespace:get(Ns, Path),
  {reply, file9p:stat(File), State};

handle_9p(?Twstat, _Msg, State) ->
  io:format("wstat: ~p~n", [_Msg]),
  {reply, <<>>, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
