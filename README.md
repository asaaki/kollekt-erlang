# kollekt-erlang

Yet another implementation of the kollekt service (origin by [Paul Asmuth](https://github.com/paulasmuth)).

## Motivation

Collect statistical data without handling them in the corresponding app itself.

Using UDP for a fire-and-forget mechanism (if we loose some packets we don't care).

Using Erlang to reduce the packet drops as much as possible (because Erlang has message inboxes for every process).

Building a distributed collector app, so the overlying production app servers do not need to communicate to one single collect service. And the distributed system will give fault-tolerance by sharing collected data across the node cluster.

##

## Usage

### Prerequisites

Install Erlang _(I build with R15B)_.

### Compile

```shell
./compile
```

### Run

Demo service:

```shell
./run
```

Test emitter:

```shell
./run_emitter
```

_**in development, everything can (and will) change, stay tuned …**_

## Status

- OTP'fied where possible and reasonable
- statistical output on console
- app handles incoming buckets, collects data and let buckets die when needed

## TODOs

- when buckets die they should export their bucket data to a storage (files, DB, …)
- exports rules (time based, size based, …)
- distributable (running on different nodes)
- using Mnesia as a first level storage (a dedicated process can then export to further targets based on rules)
