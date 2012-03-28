% UDP server
%
% needs:   queue.erl, stats.erl
% used_by: -

-module(udp).
-export([start/1]).

start(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  %io:format("UDP server started on socket ~p~n",[Socket]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      kueue:push(Bin),
      stats:incr(packets),
      loop(Socket)
  end.
