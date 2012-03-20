-module(statz).
-export([init/0]).

init() ->
  io:format("Statistic process started.~n"),
  StatsStore = ets:new(stats_store,[set]),
  SelfPid = self(),
  InitList = [{buckets_count,0},{bucket_change,0},{packets_count,0},{payload_length,0}],
  spawn(fun() -> loop_output(SelfPid,InitList) end),
  LastLoop = [0],
  loop(StatsStore, LastLoop).

loop(Store, LastLoop) ->
  receive

    { packets, count, Count } ->
      update_count(Store, packets_count, Count),
      loop(Store, LastLoop);

    { payload, length, Length } ->
      update_count(Store, payload_length, Length),
      loop(Store, LastLoop);

    { buckets, count, Result } ->
      ets:insert(Store, { buckets_count, Result }),
      loop(Store, LastLoop);

    { From, calculate } ->
      [BucketCycle|Rest] = LastLoop,

      ResponseBucketsCount  = get_int_kv(Store, buckets_count),
      ResponsePacketsCount  = get_int_kv(Store, packets_count),
      ResponsePayloadLength = get_int_kv(Store, payload_length),

      {_, CurrentBuckets}   = ResponseBucketsCount,
      BucketChange = CurrentBuckets - BucketCycle,

      Response = [ResponseBucketsCount,{bucket_change, BucketChange},ResponsePacketsCount,ResponsePayloadLength],
      From ! Response,

      % flush after calculate call
      ets:delete_all_objects(Store),

      ThisLoop = [CurrentBuckets,Rest],
      loop(Store, ThisLoop);

    _Any ->
      loop(Store, LastLoop)

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
    %io:format("===> (~p) statz msg: ~p~n",[self(), LastResult]),
    [{buckets_count,BCVal},{bucket_change, BDVal},{packets_count,PCVal},{payload_length,PLVal}] = LastResult,
    OutList = [BCVal, BDVal, PCVal, (PLVal/(1024*1024))],
    io:format("-> stats - ~9B buckets - ~9B buckets/s - ~9B packets/s - ~9.3f Mbit/s~n", OutList),
    loop_output(LoopPid, LastResult)
  end.