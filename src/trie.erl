-module(trie).

-export([new/0,
         node_qid/1,
         node_children/1,
         find/2,
         insert/3,
         remove/2,
         qid_type/1,
         qid_version/1,
         qid_path/1,
         print/1]).

-type qid() :: {Type :: integer(),
                Version :: integer(),
                Path :: integer()}.

-type name() :: binary().

-record(node, {qid :: qid(),
               children :: dict()}).

-spec new() -> #node{}.
new() ->
  #node{qid=make_qid(),
        children=dict:new()}.

node_qid(Node) when is_record(Node, node) ->
  Node#node.qid.

node_children(Node) when is_record(Node, node) ->
  Node#node.children.

-spec find(#node{}, [binary()]) -> {ok, #node{}} | false.
find(Node, []) ->
  {ok, Node};

find(#node{children=C}, [Name|T]) ->
  case dict:find(Name, C) of
    {ok, Node} ->
      find(Node, T);
    _ ->
      false
  end.

-spec insert(#node{}, [name()], #node{}) -> #node{}.
insert(#node{children=C}=Root, [Name], NewNode) ->
  Root#node{children=dict:store(Name, NewNode, C)};

insert(#node{children=C}=Root, [PathName|T], NewNode) ->
  case dict:find(PathName, C) of
    {ok, Node} ->
      Update = insert(Node, T, NewNode);
    _ ->
      Update = insert(new(), T, NewNode)
  end,
  Root#node{children=dict:store(PathName, Update, C)}.

remove(#node{children=C}=Root, [Name]) ->
  Root#node{children=dict:erase(Name, C)};

remove(#node{children=C}=Root, [PathName|T]) ->
  case dict:find(PathName, C) of
    {ok, Node} ->
      Update = remove(Node, T),
      Root#node{children=dict:store(PathName, Update, C)};
    _ ->
      Root
  end.

%% qid

-spec make_qid() -> qid().
make_qid() ->
  {0, 0, 0}.

qid_type({T, _, _}) ->
  T.

qid_version({_, V, _}) ->
  V.

qid_path({_, _, P}) ->
  P.

%% utils

print(#node{children=C, qid=Q}) ->
  io:format("/ # ~p~n", [Q]),
  print1(C, "/"),
  ok.

print1(Children, Prefix) ->
  dict:map(fun(K, V) ->
               io:format("~p~p # ~p~n", [Prefix, K, V#node.qid]),
               print1(V#node.children, Prefix++K++"/")
           end, Children).
