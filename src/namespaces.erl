-module(namespaces).

new() ->
  dict:store(<<''>>, 0 dict:new()).

new_qid() ->
  {1}
