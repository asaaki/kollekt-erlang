% collector service
%
% needs:   -
% used_by: bucket.erl

-module(collector).
-behaviour(gen_server).
-include("kollekt.hrl").

-export([
  start/1, stop/0,
  add/2, flush_db/0,
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
  ]).

start(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

init(_Args) ->
  init_flush_looper(),
  {ok, ets:new(?MODULE,[set])}.

add(BucketId, BucketData) ->
  gen_server:call(?MODULE, {add_bucket, BucketId, BucketData}).

flush_db() ->
  gen_server:call(?MODULE, {flush, toDisk}).

handle_call({add_bucket, BucketId, BucketData}, _From, Table) ->
  % also if a bucket session was destroyed before we will add new values
  % if we find an existing bucket session in the collector table
  utils:upsert_list(Table, BucketId, BucketData),
  Reply = ok,
  {reply, Reply, Table};

handle_call({flush, toDisk}, _From, Table) ->
  DataList = ets:tab2list(Table),

  case length(DataList) of
    0 -> void;
    _ ->
      Output = lists:map(fun(Item)->
        {BucketId, BucketData} = Item,
        [BucketId,";",
        lists:map(fun(E)->
          [E, ","]
        end, BucketData),
        "\n"]
      end, DataList),

      file:write_file(
        "xdump.tmp",
        Output,
        [write,binary,{encoding,utf8}])
  end,

  Reply = ok,
  {reply, Reply, Table};

handle_call(_Request, _From, Table) ->
  Reply = ok,
  {reply, Reply, Table}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% flush looper
init_flush_looper() ->
  spawn(fun()-> flush_looper() end).

flush_looper() ->
  receive
    _ -> flush_looper()
  after 600000 ->
    flush_db(),
    flush_looper()
  end.