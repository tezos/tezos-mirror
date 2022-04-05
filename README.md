# `teztale`

`teztale` is a tool that gathers information on arrival times of
blocks and consensus operations, and dumps this data as json files.

## Building

- clone this repo at the root of a tezos worktree (let's say `~/tezos/`)
- go into `~/tezos/teztale/`
- do `make`

## Running

Run a tezos node, with a RPC server opened on localhost:8732 and do
```
./main.exe run in <logdir>
```
where `<logdir>` is the directory where `teztale` will store the gathered data.

`teztale` is a tezos client, it thus accept the same global options as the client.

`teztale` will use delegate aliases found in the wallet's `public_key_hashs` file.

<!--
if run on public (test)network, it will use tzkt api to find delegate aliases.
-->
