(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** LAYER1 tezt-cloud scenario
    --------------------------

    This scenario runs a small private network derived from Mainnet or Ghostnet
    (using a rolling snapshot), with yes‑crypto enabled, and does the following:

    1. Bootstrapping
      - Starts one node that first synchronises with the real network to pull in
        full metadata.
      - Shuts it down, then reconfigures it for private mode with yes‑crypto
        enabled and a predetermined list of peers (all the nodes we will spawn).
    2. Peers calculation
      - Collects ahead of time the P2P endpoint of every baker and stresstester
        node so that each one can be launched in private mode knowing exactly which
        peers to connect to.
    3. Bakers
      - Each baker gets its own node process and client, imports its public keys,
        converts its yes‑wallet, and then runs an agnostic baker over that node.
    4. Stresstest nodes
      - Potentially launch one or more stresstest nodes that generate transactions at
        the target TPS against the private network.
    5. Metrics & Monitoring
      - Every node (bootstrap, bakers, stresstesters) is registered as a Prometheus
        scrape target if `--prometheus` is set.
      - Teztale archiver is configured automatically to record chain data.
      - Metrics on block levels and operation counts are regularly pushed.
    6. Finishing
      - The scenario itself never exits; your test runs until you kill it
        or a Cloud level timeout or an error is reached.

    For full documentation on the CLI options of the scenario, use:

    [dune exec tezt/tests/cloud/main.exe -- LAYER1 --help]

    Running an experiment on a fresh snapshot could look like this:

    {[
       # fetch a rolling snapshot:
       wget -O ghostnet.snapshot https://snapshots.tzinit.org/ghostnet/rolling
       dune exec tezt/tests/cloud/main.exe -- LAYER1 -v --log-file /tmp/log \
         --stake 1,1 \
         --network ghostnet \
         --stresstest 100/1 \
         --faketime "-$(($(date --utc +%s) - $(date --date $(./octez-node snapshot info --json ./ghostnet.snapshot | jq -r .snapshot_header.timestamp) +%s)))" \
         --tezt-cloud $USER \
         --dockerfile-alias debian \
         --prometheus \
         --grafana \
         --website \
         --snapshot file:./ghostnet.snapshot
    ]}

    As you can read using the help of [--help], [--stresstest] is:

    {v
      --stresstest TPS[/seed]
          A Public key hash and its public key are automatically retrieved from \
          the yes wallet to fund fresh accounts for reaching TPS stresstest \
          traffic generation. A seed for stresstest initialization can also be \
          specified.
    v}

    On ghostnet, the goto account to use is the faucet account (the one used above).
    For mainnet, you can either ask a local node or try with an indexer and find a
    big account.

    Along these steps, monitoring tools common to tezt cloud scenarios can be
    started (prometheus, grafana). Teztale service is started automatically.

    All nodes (bootstrap one, bakers ones and stresstest ones) are
    registered as a prometheus source.

    If you use the [--prometheus] option, you should automatically obtain a
    snapshot of data scraped during the experiment.

    For now, teztale database has to be dowloaded manually before the bootstrap
    docker container is destroyed:
    - [ssh] into the bootstrap docker container.
    - [ps aux] and find the teztale server and archiver processes
    - Note the teztale server configuration path
    - [kill] the teztale archiver
    - [cat] the teztale server configuration file and find the sqlite
      database file path
    - [scp] that file one your computer
*)
val register : (module Scenarios_cli.Layer1) -> unit
