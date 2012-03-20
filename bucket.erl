-module(bucket).
-export([new/1,new/2]).

new(BucketId) ->
  % 5 min timeout as default
  DefaultTimeout = 5*60*1000,
  new(BucketId, DefaultTimeout).

new(BucketId, Timeout) ->
  spawn(fun() -> init(BucketId, Timeout) end).

init(BucketId, Timeout) ->
  BucketStore = [],
  loop(BucketId, BucketStore, Timeout).

loop(BucketId, BucketStore, Timeout) ->
  receive
    {data, Data} ->
      NewBucketStore = [Data|BucketStore],
      loop(BucketId, NewBucketStore, Timeout);

    _Any -> true

  after Timeout ->
    io:format("!!! bucket died: ~p~n    -> values: ~p ~n",[BucketId, BucketStore])
  end.