% UDP server
%
% needs:   bucket_broker.erl
% used_by: -

-module(bucket).
-export([new/1, new/4]).

-export([checkAge/2, currentTime/0]).

-define(DEFAULT_TIMEOUT,    60).
-define(DEFAULT_MAXLIFE,   120). % only in seconds!
-define(DEFAULT_MAXITEMS, 1024).

new(BucketId) ->
  new(BucketId, (?DEFAULT_TIMEOUT*1000), ?DEFAULT_MAXLIFE, ?DEFAULT_MAXITEMS).

new(BucketId, Timeout, MaxLife, MaxItems) ->
  spawn(fun() -> init(BucketId, Timeout, MaxLife, MaxItems) end).

init(BucketId, Timeout, MaxLife, MaxItems) ->
  BucketStore = [],
  {MegaSecs, Secs, _} = now(),
  StartedAt = (MegaSecs * 1000000 + Secs),
  loop(BucketId, BucketStore, StartedAt, Timeout, MaxLife, MaxItems).

loop(BucketId, BucketStore, StartedAt, Timeout, MaxLife, MaxItems) ->
  receive
    {data, Data} ->
      NewBucketStore = [Data|BucketStore],
      sucess,
      OverAged = checkAge(StartedAt, MaxLife),
      if
        OverAged =:= true ->
          bucket_broker:remove(BucketId, maxlife);
        true ->
          loop(BucketId, NewBucketStore, StartedAt, Timeout, MaxLife, MaxItems)
      end;

    Any ->
      % debug
      io:format("Bucket :: No receiver matched! (with: ~p) [pid:~p]~n", [Any,self()]),
      oops,
      loop(BucketId, BucketStore, StartedAt, Timeout, MaxLife, MaxItems)

  after Timeout ->
    OverAged = checkAge(StartedAt, MaxLife),
    Reason = if
      OverAged =:= true ->
        maxlife;
      true ->
        timeout
    end,
    bucket_broker:remove(BucketId, Reason)
  end.

checkAge(StartedAt, MaxLife) ->
  CurrentTime = currentTime(),
  CurrentLife = CurrentTime - StartedAt,
  CurrentLife >= MaxLife.

currentTime() ->
  {MegaSecs, Secs, _} = now(),
  CurrentTime = (MegaSecs * 1000000 + Secs),
  CurrentTime.