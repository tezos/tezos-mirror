# Testnet Scenarios

This directory contains scenarios that can be run against a testnet,
(by default, Mondaynet).

```
dune exec -- src/bin_testnet_scenarios/main.exe --info
```

Configuration is fetched from a file called `scenarios.json` in the
current working directory. Alternatively, a path to such a file can be
provided with `-a "configuration=PATH"`.

For instance, running the scenarios against Mondaynet can be done with
the following configuration file (where `MM` and `DD` are customized
to match the current Mondaynet).

```
{
  "protocol": "alpha",
  "network": "https://teztnets.xyz/mondaynet-2023-MM-DD",
  "snapshot": "http://mondaynet.snapshots.s3-website.eu-central-1.amazonaws.com/mondaynet-rolling-snapshot"
}
```

The `snapshot` can be omitted and the nodes will bootstrap themself (but it
can becomes very long).


Alternatively, if you have an octez node available and you want to use the
data-dir you can provide the path:

```
{
  "protocol": "alpha",
  "network": "https://teztnets.xyz/mondaynet-2023-MM-DD",
  "data-dir": /home/.octez-node-mondaynet"
}
```
This also mean that you cannot run scenarios in parallel and need to stop
your own node.

