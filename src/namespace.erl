-module(namespace).

-export([make/2,
         add/3,
         del/3,
         get/2,
         get_children/2,
         contains/2,
         all_files/1]).

-include("9p.hrl").

-record(namespace, {name    :: binary(),
                    content :: dict()}).

-type namespace() :: #namespace{}.

-record(node, {file     :: file9p:file9p(),
               children :: set() | undefined}).

-spec make(Name::binary(), Root::file9p:file9p()) -> namespace().
make(Name, RootDir) ->
  true = file9p:is_directory(RootDir),
  <<"/">> = file9p:name(RootDir),
  Path = file9p:path(RootDir),
  Node = make_node(RootDir),
  Content = dict:store(Path, Node, dict:new()),
  #namespace{name=Name, content=Content}.

-spec add(NS::namespace(), DirPath::qid_path(), File::file9p:file9p()) ->
             {ok, namespace()} | error.
add(#namespace{content=C}=NS, DirPath, File) ->
  case dict:find(DirPath, C) of
    {ok, #node{file=Dir, children=Ch}=DirNode} ->
      true = file9p:is_directory(Dir),
      FilePath = file9p:path(File),
      FileNode = make_node(File),
      C2 = dict:store(FilePath, FileNode, C),
      Node2 = DirNode#node{children=sets:add_element(FilePath, Ch)},
      C3 = dict:store(DirPath, Node2, C2),
      NS#namespace{content=C3};
    _ ->
      error
  end.

-spec del(NS::namespace(), DirPath::qid_path(), FilePath::qid_path()) ->
             namespace().
del(#namespace{content=C}=NS, DirPath, File) ->
  case dict:find(DirPath, C) of
    {ok, #node{file=Dir, children=Ch}=DirNode} ->
      true = file9p:is_directory(Dir),
      FilePath = file9p:path(File),
      C2 = dict:erase(FilePath, C),
      Node2 = DirNode#node{children=sets:del_element(FilePath, Ch)},
      C3 = dict:store(DirPath, Node2, C2),
      NS#namespace{content=C3};
    _ ->
      NS
  end.

-spec get(NS::namespace(), Path::qid_path()) ->
             {ok, file9p:file9p()} | error.
get(#namespace{content=C}, Path) ->
  dict:find(Path, C).

-spec contains(NS::namespace(), Path::qid_path()) -> boolean().
contains(#namespace{content=C}, Path) ->
  dict:is_key(Path, C).

-spec get_children(NS::namespace(), Path::qid_path()) -> [qid_path()] | undefined | error.
get_children(#namespace{content=C}, Path) ->
  case dict:find(Path, C) of
    {ok, #node{children=Ch}} when Ch /= undefined ->
      sets:to_list(Ch);
    {ok, #node{children=undefined}} ->
      undefined;
    _ ->
      error
  end.

-spec all_files(NS::namespace()) -> [{qid_path(), file9p:file9p()}].
all_files(#namespace{content=C}) ->
  dict:to_list(C).

-spec make_node(file9p:file9p()) -> #node{}.
make_node(File) ->
  case file9p:is_directory(File) of
    true ->
      #node{file=File, children=sets:new()};
    false ->
      #node{file=File, children=undefined}
  end.
