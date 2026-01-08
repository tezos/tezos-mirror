(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** DAL tezt-cloud scenario
    --------------------------

    This scenario deploys a private Octez network augmented with
    Tezos’s Data Availability Layer (DAL), plus optional Etherlink and
    Echo Rollup operators, producers, and observers.

    Prerequisites
    -------------
    In order to be able to run the following test successfully, you need to make
    sure that your environment is well configured. To do so, have a look at the
    `tezt/lib_cloud/README.md` documentation.

    Additionally, if you are running the test you must ensure that:
    - `DAL_TRUSTED_SETUP_PATH` contains the expected data -- this can be done by
      running `./scripts/install_dal_trusted_setup.sh`
    - smart rollup binaries are available -- requires to run `make -f kernels.mk
      build-deps` and `make -f kernels.mk kernel_sdk`
    - floodgate binaries are available -- `make build-floodgate`

    Workflow
    --------

    1. Bootstrapping
      - Create a single “bootstrap” L1 node, optionally seeded from a
        public network snapshot and run it in private mode.

    2. DAL layer
      - If `--with-dal` is set, spin up an Octez DAL node alongside
        the bootstrap L1 node, configured to serve data availability
        and validate publish‑commitment proofs.

    3. Bakers
      - We either use the provided unencrypted secret keys or generate
        fresh delegate keys, funding them from the bootstrap account according
        to the desired stake distribution (`--stake`) or simulating the real network
        (using yes-crypto) via the `--simulate-network` option.

    4. Producers
      - For each configured DAL slot index, start a DAL producer daemon that
        periodically injects commitments operations into the chain.

    5. Observers
      - Launch observers that monitor on‑chain attestations for particular slots or
        baker addresses, collecting metrics on missing or delayed attestations.

    6. Etherlink
      - If `--etherlink` is enabled, start the sequencer and batching
        operators, bridge L1 block data into an Ethereum rollup, and fund their
        accounts from the bootstrap node.

    7. Echo Rollup
      - If `--echo-rollup` is enabled, deploy a simple smart rollup and operator
        that repeatedly sends and reads messages through the rollup inbox.
      - Fund their accounts from the bootstrap node.

    8. Metrics & Monitoring
      - Register every node, baker, producer, observer, and rollup operator as
        Prometheus scrape targets (if `--prometheus` is enabled).
      - Attach a Teztale archiver for historical data.

    9. Finishing
      - The scenario itself never exits; your test runs until you kill it
        or a Cloud level timeout or an error is reached.

    For full documentation on the CLI options of the scenario, use:

    [dune exec tezt/tests/cloud/main.exe -- DAL --help]

    Running an experiment on a fresh snapshot could look like this:

    {[
      # fetch a rolling snapshot:
       wget -O ghostnet.snapshot https://snapshots.tzinit.org/ghostnet/rolling

       dune exec tezt/tests/cloud/main.exe -- DAL -v --log-file /tmp/log \
         --producers 16 \
         --producer-slot-indices 0 \
         --network ghostnet \
         --tezt-cloud $USER \
         --dockerfile-alias debian \
         --snapshot file:./ghostnet.snapshot \
         --simulate map\(1000,40) \
         --prometheus \
         --grafana \
         --website \
         --monitoring
    ]}

    To avoid running such a long command, one can use the configuration file mechanism
    provided, using:

    {[
    dune exec tezt/tests/cloud/main.exe -- DAL -v --log-file /tmp/log \
      --config-file <config-file>
    ]}

    Where the configuration file contains:

    {[
        {
            "tezt_cloud": "$USER",
            "dockerfile_alias": "dal",
            "prometheus": true,
            "website": true,
            "grafana": true,
            "monitoring": true,
            "scenario_specific": [
                "DAL",
                {
                    "network": "ghostnet",
                    "producers": 16,
                    "dal_producers_slot_indices": [
                        0
                    ],
                    "simulate_network": [
                        1000,
                        40
                    ],
                    "snapshot": {
                        "local_file": "./ghostnet.snapshot"
                    }
                }
            ]
        }
    ]}
*)

val register : (module Scenarios_cli.Dal) -> unit
