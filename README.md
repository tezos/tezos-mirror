# `teztale`

`teztale` is a tool that gathers information on arrival times of
blocks and consensus operations.

It is splitted in 2 parts:
- `teztale-archiver` that monitors an octez node
- `teztale-server` which aggregates and delivers data gathered by
  archivers

## Building

- Install dependencies by doing `opam install tezos caqti-lwt
  caqti-driver-sqlite`
- clone this repo, `cd teztale` and do `make`

(As an alternative to install dependencies, clone this repo at the
root of an octez worktree)

## Running

### Aggregator

First, you need to write a configuration file. It is a json file
folowing the schema:
```json
{
"db": "path_to_the_database",
"interfaces": [ { "address":"::1","port":8080 },
                { "address":"0.0.0.0","port":8443,
                  "tls":{ "certfile":"ssl/cert.pem",
                          "keyfile":"ssl/key.pem" }
                }
				<list_of_network_point_to_bind_to> ],
"users": [ {"login":"alice","password":"secret"},
           {"login":"bob","password":"xxx"}
           <archivers_allowed_to_post_data> ]
}
```

Then you can launch the server by giving your config file as argument like
```
server/main.exe ./config.json
```

### Archiver

Run a tezos node, with a RPC server opened on localhost:8732 and do
```
archiver/main.exe feed <teztale-server-endpoint> -u <login> -p <password>
```

`teztale-archiver` is a tezos client, it thus accepts the same global
options as the client (especially the `--endpoint` option to specify
where to reach octez node RPC endpoint).

<!--
if run on public (test)network, it will use tzkt api to find delegate aliases.
-->
