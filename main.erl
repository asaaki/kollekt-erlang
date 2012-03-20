-module(main).
-export([go/0, init/1]).

% demo server on port 2323
go() ->
  init(2323).

init(Port) ->
  BucketStore = spawn(fun() -> bucket_store:init() end),
  spawn(fun() -> udp:init(Port, BucketStore) end).
