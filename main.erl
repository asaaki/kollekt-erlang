-module(main).
-export([go/0, init/1]).

% demo server on port 2323
go() ->
  init(2323).

init(Port) ->
  BucketBroker = spawn(fun() -> bucket_broker:init() end),
  spawn(fun() -> udp:init(Port, BucketBroker) end).
