
-module(kollekt_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start, []}, permanent, 5000, Type, [I]}).
-define(CHILD_W_ARGS(I, Type, Args), {I, {I, start, Args}, permanent, 5000, Type, [I]}).

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