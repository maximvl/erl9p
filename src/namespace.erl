-module(namespace).

-export([make/2,
         add/3,
         del/3,
         get/2,
         get_root/1,
         get_children/2,
         find_child_with_name/3,
         contains/2,
         all_files/1]).

-include("9p.hrl").

-record(namespace, {name    :: binary(),
                    root    :: qid_path(),
                    content :: dict()}).

-type namespace() :: #namespace{}.
-export_type([namespace/0]).

-type file9p() :: file9p:file9p().

-record(node, {file     :: file9p(),
               children :: set() | undefined}).

-spec make(Name::binary(), Root::file9p()) -> namespace().
make(Name, RootDir) ->
  true = file9p:is_directory(RootDir),
  <<>> = file9p:name(RootDir),
  Path = file9p:path(RootDir),
  Node = make_node(RootDir),
  Content = dict:store(Path, Node, dict:new()),
  #namespace{name=Name, content=Content, root=Path}.

-spec add(NS::namespace(), DirPath::qid_path(), File::file9p()) ->
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

-spec del(NS::namespace(), DirPath::qid_path(), FilePath::file9p()) ->
             namespace().
del(#namespace{content=C, root=Root}=NS, DirPath, File) ->
  case Root == file9p:path(File) of
    true ->
      %% TODO is it possible to delete root of namespace?
      NS;
    false ->
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
      end
  end.

-spec get(NS::namespace(), Path::qid_path()) ->
             {ok, file9p()} | not_found.
get(#namespace{content=C}, Path) ->
  case dict:find(Path, C) of
    {ok, Node} ->
      {ok, Node#node.file};
    _ ->
      not_found
  end.

-spec get_root(NS::namespace()) -> file9p().
get_root(#namespace{content=C, root=Root}) ->
  RNode = dict:fetch(Root, C),
  RNode#node.file.

-spec find_child_with_name(namespace(), file9p(), binary()) ->
                              {ok, file9p()} | error.
find_child_with_name(#namespace{content=C}, DirPath, Name) ->
  case dict:find(DirPath, C) of
    {ok, #node{children=Ch, file=Dir}} ->
      true = file9p:is_directory(Dir),
      ChildList = sets:to_list(Ch),
      find_child(C, ChildList, Name);
    _ ->
      error
  end.

-spec contains(NS::namespace(), Path::qid_path()) -> boolean().
contains(#namespace{content=C}, Path) ->
  dict:is_key(Path, C).

-spec get_children(NS::namespace(), Path::qid_path()) ->
                      [qid_path()] | undefined | error.
get_children(#namespace{content=C}, Path) ->
  case dict:find(Path, C) of
    {ok, #node{children=Ch}} when Ch /= undefined ->
      sets:to_list(Ch);
    {ok, #node{children=undefined}} ->
      undefined;
    _ ->
      error
  end.

-spec all_files(NS::namespace()) -> [{qid_path(), file9p()}].
all_files(#namespace{content=C}) ->
  dict:to_list(C).

-spec make_node(file9p()) -> #node{}.
make_node(File) ->
  case file9p:is_directory(File) of
    true ->
      #node{file=File, children=sets:new()};
    false ->
      #node{file=File, children=undefined}
  end.

-spec find_child(dict(), [qid_path()], binary()) ->
                    {ok, file9p()} | not_found.
find_child(_, [], _) ->
  not_found;

find_child(Cont, [Path|Rest], Name) ->
  case dict:find(Path, Cont) of
    {ok, #node{file=F}} ->
      case Name == file9p:name(F) of
        true ->
          {ok, F};
        false ->
          find_child(Cont, Rest, Name)
      end;
    _ ->
      find_child(Cont, Rest, Name)
  end.
