(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** Layer 1 test scenario goal is to run experiments in a mainnet or ghostnet
    context.

    [dune exec tezt/tests/cloud/main.exe -- cloud l1 --help]

    The different steps of this scenario are:
    - Bootstrap a node using real network so that we have at least one node
      with the latest block metadata
    - Disconnect the node from this network
    - From here, every node will be ran with --private-mode and a known list
      of peers (i.e. the full set of nodes involved in the experiment)
    - Start bakers
    - Wait for the nodes to sync
    - Launch some nodes and clients and use them for stresstesting the network
    - The experiment won't stop by itself unless some tezt-cloud timeout is
      reached or a failure occurs.

    Running an experiment on a fresh snapshot could look like this:

    {[
       wget -O ghostnet.snapshot https://snapshots.tzinit.org/ghostnet/rolling
       dune exec tezt/tests/cloud/main.exe -- cloud l1 -v --log-file /tmp/log \
         --l1-stake 1,1 \
         --network ghostnet \
         --l1-stresstest tz1KhnTgwoRRALBX6vRHRnydDGSBFsWtcJxc/edpkuFRauFAdhipQu9s4xmfNJWmtLxPKpoaoG41gYGq5AgUA43Vxqx/100/1 \
         --faketime "-$(($(date --utc +%s) - $(date --date $(./octez-node snapshot info --json ./ghostnet.snapshot | jq -r .snapshot_header.timestamp) +%s)))" \
         --tezt-cloud $USER \
         --dockerfile-alias debian \
         --prometheus \
         --grafana \
         --website \
         --snapshot ./ghostnet.snapshot
    ]}

    As you can read using the help of [--help], [--l1-stresstest] is:

    {v
      --l1-stresstest pkh/pk/TPS[/seed]
          Public key hash / public key of an account used to fund fresh accounts
          for reaching TPS stresstest traffic generation. A seed for stresstest
          initialization can also be specified.
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
