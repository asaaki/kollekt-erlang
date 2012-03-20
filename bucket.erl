-module(bucket).
-export([new/2, new/3]).

new(BucketId, BrokerPid) ->
  % 5 min timeout as default
  DefaultTimeout = 1*60*1000,
  new(BucketId, BrokerPid, DefaultTimeout).

new(BucketId, BrokerPid, Timeout) ->
  spawn(fun() -> init(BucketId, BrokerPid, Timeout) end).

init(BucketId, BrokerPid, Timeout) ->
  BucketStore = [],
  loop(BucketId, BrokerPid, BucketStore, Timeout).

loop(BucketId, BrokerPid, BucketStore, Timeout) ->
  receive
    {data, Data} ->
      NewBucketStore = [Data|BucketStore],
      loop(BucketId, BrokerPid, NewBucketStore, Timeout);

    _Any -> true

  after Timeout ->
    BrokerPid ! {remove, BucketId}
    % io:format("!!! bucket died: ~p~n    -> values: ~p ~n",[BucketId, BucketStore])
  end.