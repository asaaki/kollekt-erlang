% UDP server
%
% needs:   bucket_broker.erl
% used_by: -

-module(bucket).
-export([new/1, new/2]).

new(BucketId) ->
  DefaultTimeout = 1*60*1000,
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
      sucess,
      loop(BucketId, NewBucketStore, Timeout);

    Any ->
      % debug
      io:format("Bucket :: No receiver matched! (with: ~p) [pid:~p]~n", [Any,self()]),
      oops,
      loop(BucketId, BucketStore, Timeout)

  after Timeout ->
    bucket_broker:remove(BucketId, timeout)
    %, io:format("!!! bucket died: ~p~n    -> values: ~p ~n",[BucketId, lists:reverse(BucketStore)])
  end.