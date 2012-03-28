-module(main).
-export([go/0, init/1]).

% demo server on port 2323
go() ->
  init(2323).

init(Port) ->
  io:format("kollekt startup ...~n"),
  stats:start([]),
  kueue:start([]),
  bucket_broker:start([]),
  spawn(fun() -> udp:start(Port) end).
