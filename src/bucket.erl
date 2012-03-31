% UDP server
%
% needs:   bucket_broker.erl
% used_by: bucket_broker.erl

-module(bucket).
-export([new/1, new/2]).
-include("kollekt.hrl").

new(BucketId) ->
  Opts = #bucket_opts{},
  new(BucketId, Opts).

new(BucketId, Opts) ->
  spawn(fun() -> init(BucketId, Opts) end).

init(BucketId, Opts) ->
  BucketStore = [],
  {MegaSecs, Secs, _} = now(),
  StartedAt = (MegaSecs * 1000000 + Secs),
  loop(BucketId, BucketStore, StartedAt, Opts).

loop(BucketId, BucketStore, StartedAt, Opts) ->
  receive
    {data, Data} ->
      NewBucketStore = [Data|BucketStore],
      sucess,
      OverAged = checkAge(StartedAt, Opts#bucket_opts.maxlife),
      OverSized = length(NewBucketStore) >= Opts#bucket_opts.maxitems,
      case {OverAged, OverSized} of
        {true, false} ->
          bucket_broker:remove(BucketId, maxlife);
        {false, true} ->
          bucket_broker:remove(BucketId, maxitems);
        {true, true} ->
          bucket_broker:remove(BucketId, maxitems); % we take the maxitems as first prio reason
        {_,_} ->
          loop(BucketId, NewBucketStore, StartedAt, Opts)
      end;

    Any ->
      % debug
      io:format("Bucket :: No receiver matched! (with: ~p) [pid:~p]~n", [Any,self()]),
      oops,
      loop(BucketId, BucketStore, StartedAt, Opts)

  after Opts#bucket_opts.timeout ->
    OverSized = length(BucketStore) >= Opts#bucket_opts.maxitems,
    OverAged  = checkAge(StartedAt, Opts#bucket_opts.maxlife),
    Reason = if
      OverSized =:= true ->
        maxitems;
      OverAged =:= true ->
        maxlife;
      true ->
        timeout
    end,
    bucket_broker:remove(BucketId, Reason)
  end.


% helper

checkAge(StartedAt, MaxLife) ->
  CurrentTime = currentTime(),
  CurrentLife = CurrentTime - StartedAt,
  CurrentLife >= MaxLife.

currentTime() ->
  {MegaSecs, Secs, _} = now(),
  CurrentTime = (MegaSecs * 1000000 + Secs),
  CurrentTime.
