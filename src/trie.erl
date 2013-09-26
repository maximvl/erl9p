%% Simple trie to represent symbolic 9p filesystem.

-module(trie).

-export([new/1, find/2, add/2, add/3, del/2, print/1]).

-type qid()  :: {Type::integer(), Version::integer(), Path::integer()}.

-record(node, {name  :: binary(),
               qid   :: qid(),
               nodes :: [#node{}]}).

-spec new(Name::binary()) -> #node{}.
new(Name) ->
  #node{name=Name,
        qid=make_qid(),
        nodes=[]}.

-spec find(Root::#node{}, Path::[binary()]) -> false | #node{}.
find(R, []) ->
  R;

find(#node{nodes=Nodes}, [Name|T]) ->
  case lists:keyfind(Name, #node.name, Nodes) of
    false ->
      false;
    NextRoot ->
      find(NextRoot, T)
  end.

-spec add(Root::#node{}, Node::#node{}) -> #node{}.
add(Root, Node) ->
  add(Root, Node, []).

-spec add(Root::#node{}, Node::#node{}, Path::[binary()]) -> #node{}.
add(Root, Node, Path) when not is_record(Node, node) ->
  add(Root, trie:new(Node), Path);

add(Root, Node, []) ->
  add(Root, Node, [Root#node.name]);

add(#node{name=Name, nodes=Nodes}=Root, Child, [Name]) ->
  Root#node{nodes=[Child | Nodes]};

add(#node{nodes=Nodes}=Root, Child, [Name|T]) ->
  case lists:keyfind(Name, #node.name, Nodes) of
    false ->
      NewNode = add(new(Name), Child, T),
      Root#node{nodes = [NewNode | Nodes]};
    NextRoot ->
      NewNode = add(NextRoot, Child, T),
      NewNodes = lists:delete(NextRoot, Nodes),
      Root#node{nodes = [NewNode | NewNodes]}
  end.

-spec del(Root::#node{}, Path::[binary()]) -> #node{}.
del(_, []) ->
  ok;

del(#node{nodes=Nodes}=Root, [Name|T]) ->
  case lists:keyfind(Name, #node.name, Nodes) of
    false ->
      Root;
    NextRoot ->
      NewNodes = lists:delete(NextRoot, Nodes),
      case del(NextRoot, T) of
        ok ->
          Root#node{nodes = NewNodes};
        NewNode ->
          Root#node{nodes = [NewNode | NewNodes]}
      end
  end.

-spec make_qid() -> qid().
make_qid() ->
  0.

print(Root) ->
  print(Root, []).

print(#node{name=Name, qid=Qid, nodes=Nodes}, Path) ->
  NewPath = [Name|Path],
  io:format("~p#~p~n", [lists:reverse(NewPath), Qid]),
  [print(C, NewPath) || C <- Nodes],
  ok.
