-module(namespace).

-export([new/0,
         content/1,
         find/2,
         insert/3,
         remove/2]).

new() ->
  trie:new().

content(Namespace) ->
  dict:to_list(trie:node_children(Namespace)).

find(Namespace, Path) ->
  trie:find(Namespace, Path).

insert(Namespace, Path, Node) ->
  trie:insert(Namespace, Path, Node).

remove(Namespace, Path) ->
  trie:remove(Namespace, Path).
