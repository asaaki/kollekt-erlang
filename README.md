# kollekt-erlang

Yet another implementation of the kollekt service (origin by [Paul Asmuth](https://github.com/paulasmuth)).

## Usage

### Prerequisites

Install Erlang (I build with R15B).

### Compile

```shell
erlc main.erl udp.erl bucket_broker.erl bucket.erl
```

### Run

```shell
# starts service with udp port 2323 (testing mode)
erl -s main go
```

**in development, everything can (and will) change, stay tuned …**