# Testnet Scenarios

This directory contains scenarios that can be run against a testnet,
(by default, Weeklynet).

```
dune exec -- src/bin_testnet_scenarios/main.exe --info
```

Configuration is fetched from a file called `scenarios.json` in the
current working directory. Alternatively, a path to such a file can be
provided with `-a "configuration=PATH"`.

For instance, running the scenarios against Weeklynet can be done with
the following configuration file (where `MM` and `DD` are customized
to match the current Weeklynet).

```
{
  "protocol": "alpha",
  "network": "https://teztnets.com/weeklynet-2023-MM-DD",
  "snapshot": "https://snapshots.eu.tzinit.org/weeklynet/rolling"
}
```

The `snapshot` can be omitted and the nodes will bootstrap themself (but it
can becomes very long).


Alternatively, if you have an octez node available and you want to use the
data-dir you can provide the path:

```
{
  "protocol": "alpha",
  "network": "https://teztnets.com/weeklynet-2023-MM-DD",
  "data-dir": /home/.octez-node-weeklynet"
}
```
This also mean that you cannot run scenarios in parallel and need to stop
your own node.

You can also specify a client data dir, if you have one available. You
can provide the path with:

```
{
  "protocol": "alpha",
  "network": "https://teztnets.com/weeklynet-2023-MM-DD",
  "client-dir": /home/.tezos-client-testnet"
}
```
Any keys and addresses known to the client will be accessible.
