-module(udp).
-export([init/3, init_multi/3]).

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

init_multi(Port, BucketServerPid, StatsPid) ->
  {ok, Socket} = gen_udp:open(Port, [binary,{active,false},{reuseaddr,true}]),
  io:format("UDP service started on socket ~p~n",[Socket]),

  spawn(fun() ->
    io:format(" -> UDP processor 1 (~p) started.~n",[self()]),
    loop_passive(Socket, BucketServerPid, StatsPid)
  end),
  spawn(fun() ->
    io:format(" -> UDP processor 2 started.~n"),
    loop_passive(Socket, BucketServerPid, StatsPid)
  end),
  spawn(fun() ->
    io:format(" -> UDP processor 3 started.~n"),
    loop_passive(Socket, BucketServerPid, StatsPid)
  end),
  spawn(fun() ->
    io:format(" -> UDP processor 4 started.~n"),
    loop_passive(Socket, BucketServerPid, StatsPid)
  end),

  loop_multi().

loop_multi() ->
  % master process must be alive
  loop_multi().

loop_passive(Socket, BucketServerPid, StatsPid) ->
  case gen_udp:recv(Socket, 0) of
    {ok, {_Addr, _Port, Packet}} ->
      [Bucket, Data] = binary:split(Packet, <<";">>),
      BucketServerPid ! {lookup, Bucket, Data},
      StatsPid ! { packets, count, 1},
      StatsPid ! { payload, length, bit_size(Packet)},
      loop_passive(Socket, BucketServerPid, StatsPid);

    {error,ealready} ->
      % do nothing special - means only that no traffic comes in
      % and one of the other processes blocks the port
      loop_passive(Socket, BucketServerPid, StatsPid);

    Error ->
      io:format("udp_passive error: PID ~p ERR ~p~n", [self(), Error]),
      loop_passive(Socket, BucketServerPid, StatsPid)
  end.