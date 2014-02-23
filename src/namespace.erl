-module(namespace).

-export([make/2,
         add/3,
         del/2,
         get_file/2,
         get_root/1,
         get_children/2,
         get_parent/2,
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
               parent   :: qid_path(),
               children :: set() | undefined}). % set of qid_path's

-spec make(Name::binary(), Root::file9p()) -> namespace().
make(Name, RootDir) ->
  true = file9p:is_directory(RootDir),
  <<>> = file9p:name(RootDir),
  Path = file9p:path(RootDir),
  Node = make_node(RootDir, Path),              % root is parent for itself
  Content = dict:store(Path, Node, dict:new()),
  #namespace{name=Name, content=Content, root=Path}.

-spec add(NS::namespace(), Dir::file9p() | qid_path(), File::file9p()) ->
             {ok, namespace()} | error.
add(#namespace{content=C}=NS, Dir, File) ->
  case get_node(NS, Dir) of
    {ok, #node{file=DirF, children=Ch}=DirNode} ->
      true = file9p:is_directory(DirF),
      FilePath = file9p:path(File),
      DirPath = file9p:path(DirF),
      FileNode = make_node(File, DirPath),
      C2 = dict:store(FilePath, FileNode, C),
      Node2 = DirNode#node{children=sets:add_element(FilePath, Ch)},
      C3 = dict:store(DirPath, Node2, C2),
      NS#namespace{content=C3};
    _ ->
      error
  end.

-spec del(NS::namespace(), File::file9p() | qid_path()) -> namespace() | error | {error, directory_not_empty}.
del(#namespace{content=C, root=Root}=NS, File) ->
  case get_node(NS, File) of
    {ok, #node{parent=Parent, children=Ch, file=FileF}} ->
      FilePath = file9p:path(FileF),
      case FilePath == Root of
        true ->
          %% TODO is it possible to delete root of namespace?
          error;
        false ->
          case Ch == undefined orelse sets:size(Ch) == 0 of
            true ->
              PNode = dict:fetch(Parent, C),
              Pch = PNode#node.children,

              Pch2 = sets:del_element(FilePath, Pch),
              C2 = dict:erase(FilePath, C),
              C3 = dict:store(Parent, Parent#node{children=Pch2}, C2),
              NS#namespace{content=C3};
            false ->
              %% TODO add recursive deletion?
              {error, directory_not_empty}
          end
      end;
    _ ->
      error
  end.

-spec get_node(NS::namespace(), file9p() | qid_path()) ->
                  {ok, node()} | error.
get_node(NS, File) ->
  case file9p:is_file9p(File) of
    true ->
      get_node1(NS, file9p:path(File));
    false ->
      get_node1(NS, File)
  end.

-spec get_node1(namespace(), qid_path()) -> {ok, node()} | error.
get_node1(#namespace{content=C}, Path) ->
  dict:find(Path, C).

-spec get_file(NS::namespace(), Path::qid_path()) ->
                  {ok, file9p()} | not_found.
get_file(NS, Path) ->
  case get_node1(NS, Path) of
    {ok, #node{file=File}} ->
      {ok, File};
    _ ->
      not_found
  end.

-spec get_root(NS::namespace()) -> file9p().
get_root(#namespace{content=C, root=Root}) ->
  RNode = dict:fetch(Root, C),
  RNode#node.file.

-spec find_child_with_name(namespace(), file9p() | qid_path(), binary()) ->
                              {ok, file9p()} | error.
find_child_with_name(NS, Dir, Name) ->
  case get_node(NS, Dir) of
    {ok, #node{children=Ch, file=Dir}} ->
      case file9p:is_directory(Dir) of
        true ->
          ChildList = sets:to_list(Ch),
          find_child(NS#namespace.content, ChildList, Name);
        false ->
          {error, not_a_dir}
      end;
    _ ->
      error
  end.

-spec contains(NS::namespace(), File::file9p() | qid_path()) -> boolean().
contains(#namespace{content=C}, File) ->
  case file9p:is_file9p(File) of
    true ->
      dict:is_key(file9p:path(File), C);
    false ->
      dict:is_key(File, C)
  end.

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

-spec get_parent(NS::namespace(), File::file9p() | qid_path()) -> {ok, file9p()} | error.
get_parent(NS, File) ->
  case get_node(NS, File) of
    {ok, #node{parent=Parent}} ->
      {ok, #node{file=ParentF}} = get_node1(NS, Parent),
      {ok, ParentF};
    _ ->
      error
  end.

-spec all_files(NS::namespace()) -> [{qid_path(), file9p()}].
all_files(#namespace{content=C}) ->
  dict:to_list(C).

-spec make_node(file9p(), qid_path()) -> #node{}.
make_node(File, Parent) ->
  case file9p:is_directory(File) of
    true ->
      #node{file=File, parent=Parent, children=sets:new()};
    false ->
      #node{file=File, parent=Parent, children=undefined}
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
