-module(bucket_broker).
-export([init/1]).

init(StatsPid) ->
  BucketPidStore = ets:new(bucket_pid_store,[set]),
  io:format("BucketStore server started with ets ~p~n",[BucketPidStore]),
  loop(BucketPidStore, StatsPid).

loop(BucketPidStore, StatsPid) ->
  receive

    {lookup, BucketId, Data} ->
      Lookup = ets:lookup(BucketPidStore, BucketId),
      case Lookup of
        [{_BucketId, BucketPid}] ->
          self() ! {update, BucketPid, Data};
        [] ->
          self() ! {create, BucketId, Data}
      end,
      StatsPid ! { buckets, count, ets:info(BucketPidStore, size)},
      loop(BucketPidStore, StatsPid);

    {create, BucketId, Data} ->
      BucketPid = bucket:new(BucketId, self(), StatsPid),
      ets:insert(BucketPidStore, {BucketId, BucketPid}),
      call_bucket(BucketPid, Data),
      StatsPid ! { buckets, create, 1},
      loop(BucketPidStore, StatsPid);

    {update, BucketPid, Data} ->
      call_bucket(BucketPid, Data),
      StatsPid ! { buckets, update, 1},
      loop(BucketPidStore, StatsPid);

    {remove, BucketId} ->
      ets:delete(BucketPidStore, BucketId),
      StatsPid ! { buckets, remove, 1},
      StatsPid ! { buckets, count, ets:info(BucketPidStore, size)},
      loop(BucketPidStore, StatsPid)

  % update buckets count also if no new incoming
  after 500 ->
    StatsPid ! { buckets, count, ets:info(BucketPidStore, size)},
    loop(BucketPidStore, StatsPid)

  end.

call_bucket(Pid, Data) ->
  Pid ! {data, Data}.