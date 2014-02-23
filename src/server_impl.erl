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

-record(state, {ns    :: namespace:namespace(),
                fids  :: dict(),
                user  :: binary()}).

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
  {ok, #state{ns=Ns, fids=dict:new(),
              user= <<"nobody">>}}.

session_init(V, State) ->
  io:format("new session: ~p~n", [V]),
  State.

handle_9p(?Tauth, _Msg, State) ->
  io:format("auth: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tflush, _Msg, State) ->
  io:format("flush: ~p~n", [_Msg]),
  {reply, <<>>, State};

handle_9p(?Tattach, {Fid, _AFid, Uname, _Aname}=Msg,
          #state{ns=Ns, fids=Fids}=State) ->
  io:format("attach: ~p~n", [Msg]),
  Root = namespace:get_root(Ns),
  Fids2 = dict:store(Fid, file9p:path(Root), Fids),
  %% TODO user->groups mapping module
  {reply, file9p:qid(Root), State#state{fids=Fids2, user=Uname}};

handle_9p(?Tclunk, Fid, #state{fids=Fids}=State) ->
  io:format("clunk: ~p~n", [Fid]),
  Fids2 = dict:erase(Fid, Fids),
  {reply, <<>>, State#state{fids=Fids2}};

%% clone fid
handle_9p(?Twalk, {Fid, NFid, []}, #state{fids=Fids}=State) ->
  io:format("walk: ~p -> ~p ~n", [Fid, NFid]),
  File = fid2file(Fid, State),
  Fids2 = dict:store(NFid, file9p:path(File), Fids),
  {reply, [], State#state{fids=Fids2}};

handle_9p(?Twalk, {Fid, NFid, Names}, #state{fids=Fids,
                                             ns=Ns,
                                             user=User}=State) ->
  io:format("walk: ~p -> ~p : ~p~n", [Fid, NFid, Names]),
  Cur = fid2file(Fid, State),
  case walk_names(Names, Cur, Ns, User, []) of
    {ok, Qids, LastPath} ->
      Fids2 = dict:store(NFid, LastPath, Fids),
      {reply, Qids, State#state{fids=Fids2}};
    {error, Qids} when is_list(Qids) ->
      {reply, Qids, State};
    {error, E} when is_binary(E) ->
      {error, E, State}
  end;

handle_9p(?Topen, {Fid, Mode}, #state{user=U}=State) ->
  io:format("open: ~p~n", [{Fid, Mode}]),
  File = fid2file(Fid, State),
  <<IntMode/integer>> = Mode,
  case file9p:accessible(File, U, [], IntMode) of
    true ->
      %% TODO set/get iounit
      {reply, {file9p:qid(File), ?IOUnit}, State};
    false ->
      {error, <<"No permissions">>, State}
  end;

handle_9p(?Tcreate, {Fid, Name, Perm, Mode}=Msg, State) ->
  io:format("create: ~p~n", [Msg]),
  %% TODO correct permissions for new file/dir
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

fid2file(Fid, #state{ns=Ns, fids=Fids}) ->
  {ok, Path} = dict:find(Fid, Fids),
  {ok, File} = namespace:get(Ns, Path),
  File.

-spec walk_names([binary()], file9p:file9p(),
                 namespace:namespace(),
                 binary(), [binary()]) ->
                    {ok, [qid()], qid_path()} |
                    {error, [qid()]} |
                    {error, binary()}.
walk_names([Name|Tail], Curr, Ns, User, Groups) ->
  case file9p:executable(Curr, User, Groups) of
    true ->
      CurPath = file9p:path(Curr),
      case namespace:find_child_with_name(Ns, CurPath, Name) of
        {ok, File} ->
          Qid = file9p:qid(File),
          case walk_names1(Tail, File, Ns, User, Groups) of
            {ok, []} ->
              {ok, [Qid], file9p:path(File)};
            {ok, Qids, Path} ->
              {ok, [Qid | Qids], Path};
            {error, Qids} when is_list(Qids) ->
              {error, [Qid | Qids]};
            {error, E} when is_binary(E) ->
              {error, [Qid]}
          end;
        error ->
          {error, <<"not found">>}
      end;
    false ->
      {error, <<"no permission">>}
  end.

-spec walk_names1([binary()], file9p:file9p(),
                  namespace:namespace(),
                  binary(), [binary()]) ->
                     {ok, []} |
                     {ok, [qid()], qid_path()} |
                     {error, [qid()]} |
                     {error, binary()}.
walk_names1([], _, _, _, _) ->
  {ok, []};

%% types hack
walk_names1(Names, Curr, Ns, User, Groups) ->
  walk_names(Names, Curr, Ns, User, Groups).
