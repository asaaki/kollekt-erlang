% Queue
%
% needs:   bucket_broker.erl, stats.erl
% used_by: udp.erl

-module(kueue).

-behaviour(gen_server).

-export([
  start/1, stop/0, push/1,
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
  ]).

-record(state, {}).

start(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

push(Bin) -> gen_server:call(?MODULE, {do, Bin}).

init([]) ->
  io:format("queue started.~n"),
  {ok, #state{}}.

handle_call({do, Bin}, _From, State) ->
  [Bucket, Data] = binary:split(Bin, <<";">>),
  Lookup = case bucket_broker:lookup(Bucket) of
    [{_BucketId, BucketPid}] ->
      bucket_broker:update(BucketPid, Data),
      {ok, update};
    [] ->
      bucket_broker:create(Bucket, Data),
      {ok, create}
  end,
  stats:add(payload, bit_size(Bin)),
  Reply = {lookup, Lookup},
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
