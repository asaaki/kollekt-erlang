% UDP server
%
% needs:   queue.erl, stats.erl
% used_by: kollekt_app.erl

-module(udp).
-include("kollekt.hrl").
-export([start/1]).

start(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      kueue:push(Bin),
      stats:incr(packets),
      loop(Socket)
  end.
