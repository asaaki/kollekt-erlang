-module(statz).
-export([init/0]).

init() ->
  io:format("Statistic process started.~n"),
  StatsStore = ets:new(stats_store,[set]),
  SelfPid = self(),
  InitList = [
    {buckets_count,      0},
    {buckets_create,     0},
    {buckets_remove,     0},
    {buckets_update,     0},
    {packets_count,      0},
    {payload_length,     0}],
  spawn(fun() -> loop_output(SelfPid,InitList) end),
  loop(StatsStore).

loop(Store) ->
  receive

    { packets, count, Count } ->
      update_count(Store, packets_count, Count),
      loop(Store);

    { payload, length, Length } ->
      update_count(Store, payload_length, Length),
      loop(Store);

    { buckets, create, Result } ->
      update_count(Store, buckets_create, Result),
      loop(Store);

    { buckets, update, Result } ->
      update_count(Store, buckets_update, Result),
      loop(Store);

    { buckets, remove, Result } ->
      update_count(Store, buckets_remove, Result),
      loop(Store);

    { buckets, count, Result } ->
      ets:insert(Store, { buckets_count, Result }),
      loop(Store);

    { From, calculate } ->
      ResponseBucketsCount   = get_int_kv(Store, buckets_count),
      ResponseBucketsCreate  = get_int_kv(Store, buckets_create),
      ResponseBucketsRemove  = get_int_kv(Store, buckets_remove),
      ResponseBucketsUpdate  = get_int_kv(Store, buckets_update),
      ResponsePacketsCount   = get_int_kv(Store, packets_count),
      ResponsePayloadLength  = get_int_kv(Store, payload_length),

      Response = [
        ResponseBucketsCount,
        ResponseBucketsCreate,
        ResponseBucketsRemove,
        ResponseBucketsUpdate,
        ResponsePacketsCount,
        ResponsePayloadLength
        ],
      From ! Response,

      % flush after calculate call
      ets:delete_all_objects(Store),

      loop(Store);

    _Any ->
      loop(Store)

  end.

update_count(Store, Key, Increment) ->
  New = get_int_value(Store, Key) + Increment,
  ets:insert(Store, { Key, New }).

get_int_value(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{_Key, Value}] -> Value;
    []              -> 0
  end.

get_int_kv(Store, Key) ->
  case ets:lookup(Store, Key) of
    [{K, V}] -> {K,   V};
    []       -> {Key, 0}
  end.

loop_output(LoopPid, LastResult) ->
  receive
    Response ->
      loop_output(LoopPid, Response)

  after 1000 ->
    LoopPid ! { self(), calculate },
    [
      { buckets_count,  BCount   },
      { buckets_create, BCreate  },
      { buckets_remove, BRemove  },
      { buckets_update, _BUpdate },
      { packets_count,  PCount   },
      { payload_length, PayLoad  }
    ] = LastResult,
    OutList = [BCount, BCreate, BRemove, (BCreate-BRemove), PCount, (PayLoad/(8*1024*1024)), (PayLoad/(1000*1000))],
    io:format("=== | Buckets: ~9B total [~7B +/s] [~7B -/s] [~7B +/-] | ~9B packets/s | PTP: ~5.1f MiB/s (~5.1f Mbit/s) ===~n", OutList),
    loop_output(LoopPid, LastResult)
  end.