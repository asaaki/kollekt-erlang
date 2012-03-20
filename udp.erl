-module(udp).
-export([init/2]).

init(Port, BucketServerPid) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("UDP server started on socket ~p~n",[Socket]),
  loop(Socket, BucketServerPid).

loop(Socket, BucketServerPid) ->
  receive
    {udp, Socket, _Host, _Port, Bin} = _Msg ->
      [Bucket, Data] = binary:split(Bin, <<";">>),
      BucketServerPid ! {lookup, Bucket, Data},
      loop(Socket, BucketServerPid)
  end.