-module(main).
-export([go/0, init/1]).

% demo server on port 2323
go() ->
  init(2323).

init(Port) ->
  KollektStats = spawn(fun() -> statz:init() end),
  BucketBroker = spawn(fun() -> bucket_broker:init(KollektStats) end),
  spawn(fun() -> udp:init(Port, BucketBroker, KollektStats) end).
  %spawn(fun() -> udp:init_multi(Port, BucketBroker, KollektStats) end).
