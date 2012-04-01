% general definitions and records

% dummy state record
-record(state, {}).

% defaults

-define(DEFAULT_PORT, 2323).

% supervisor

-define(CHILD(I, Type),
  {I, {I, start, []}, permanent, 5000, Type, [I]}).
-define(CHILD_W_ARGS(I, Type, Args),
  {I, {I, start, Args}, permanent, 5000, Type, [I]}).

% kueue

-define(DEFAULT_BUCKET_LIST_DELIMITER, <<"|||">>).
-define(DEFAULT_BUCKET_DATA_DELIMITER, <<";">>).

% buckets

-define(DEFAULT_TIMEOUT,   300). % seconds
-define(DEFAULT_MAXLIFE,   600). % seconds
-define(DEFAULT_MAXITEMS, 1024).

-record(bucket_opts,
  {
    timeout  = (?DEFAULT_TIMEOUT*1000),
    maxlife  = ?DEFAULT_MAXLIFE,
    maxitems = ?DEFAULT_MAXITEMS
  }).

% stats

-define(STATS_SHOW_TIMER, 2). % seconds
-define(STATS_KEY_LIST,[
  packets,
  payload,
  {buckets, processed},
  {buckets, created},
  {buckets, updated},
  {buckets, removed, timeout},
  {buckets, removed, maxlife},
  {buckets, removed, maxitems}
  ]).

% test emitter

% ALLOWED CHARS
-define(TEST_ALLOWED_SESSION_CHARS,
  list_to_tuple("0123456789abcdef")).
-define(TEST_ALLOWED_CHARS,
  list_to_tuple("abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789")).
