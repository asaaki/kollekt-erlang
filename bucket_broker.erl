-module(bucket_broker).
-export([init/0]).

init() ->
  BucketPidStore = ets:new(bucket_pid_store,[set]),
  io:format("BucketStore server started with ets ~p~n",[BucketPidStore]),
  loop(BucketPidStore).

loop(BucketPidStore) ->
  receive

    {lookup, BucketId, Data} ->
      Lookup = ets:lookup(BucketPidStore, BucketId),
      case Lookup of
        [{_BucketId, BucketPid}] -> self() ! {update, BucketPid, Data};
        []                       -> self() ! {create, BucketId, Data}
      end,
      loop(BucketPidStore);

    {create, BucketId, Data} ->
      BucketPid = bucket:new(BucketId),
      ets:insert(BucketPidStore, {BucketId, BucketPid}),
      call_bucket(BucketPid, Data),
      loop(BucketPidStore);

    {update, BucketPid, Data} ->
      call_bucket(BucketPid, Data),
      loop(BucketPidStore)

  end.

call_bucket(Pid, Data) ->
  Pid ! {data, Data}.