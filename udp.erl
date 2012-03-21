-module(udp).
-export([init/3]).

init(Port, BucketServerPid, StatsPid) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("UDP server started on socket ~p~n",[Socket]),
  loop(Socket, BucketServerPid, StatsPid).

loop(Socket, BucketServerPid, StatsPid) ->
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      [Bucket, Data] = binary:split(Bin, <<";">>),
      BucketServerPid ! {lookup, Bucket, Data},
      StatsPid ! { packets, count, 1},
      StatsPid ! { payload, length, bit_size(Bin)},
      loop(Socket, BucketServerPid, StatsPid)
  end.
