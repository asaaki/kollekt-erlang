-module(bucket).
-export([new/3, new/4]).

new(BucketId, BrokerPid, StatsPid) ->
  DefaultTimeout = 1*60*1000,
  new(BucketId, BrokerPid, StatsPid, DefaultTimeout).

new(BucketId, BrokerPid, StatsPid, Timeout) ->
  spawn(fun() -> init(BucketId, BrokerPid, StatsPid, Timeout) end).

init(BucketId, BrokerPid, StatsPid, Timeout) ->
  BucketStore = [],
  loop(BucketId, BrokerPid, StatsPid, BucketStore, Timeout).

loop(BucketId, BrokerPid, StatsPid, BucketStore, Timeout) ->
  receive
    {data, Data} ->
      NewBucketStore = [Data|BucketStore],
      loop(BucketId, BrokerPid, StatsPid, NewBucketStore, Timeout);

    _Any -> true

  after Timeout ->
    BrokerPid ! {remove, BucketId}
    % io:format("!!! bucket died: ~p~n    -> values: ~p ~n",[BucketId, BucketStore])
  end.