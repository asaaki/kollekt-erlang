% kollekt supervisor
-module(kollekt_sup).
-behaviour(supervisor).
-include("kollekt.hrl").
-export([start_link/0, init/1]).

start_link() ->
  ChildSpecs =  [
    ?CHILD(stats, worker),
    ?CHILD(kueue, worker),
    ?CHILD(bucket_broker, worker),
    ?CHILD_W_ARGS(udp, worker, [2323])
  ],
  supervisor:start_link({local, ?MODULE}, ?MODULE, ChildSpecs).

init(ChildSpecs) ->
    {ok, { {one_for_one, 10, 3600}, ChildSpecs } }.
