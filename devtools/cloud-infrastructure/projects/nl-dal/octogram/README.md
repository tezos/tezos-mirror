This directory contains Octogram scenarios that spawn DAL networks on Google
Cloud to measure DAL throughput and/or TPS.


# basic-scenario.yml

This is a basic scenario with few agents intended to be run locally. It's
written to test jobs while they are added to Octogram.


# dal-throughput-scenario.yml.in

This parameterized scenario is intended to be run on Google Cloud (or locally
with few agents to debug it).

## What it does

The scenario templates assume that the Octez node binaries are provided via some
Google Cloud bucket (this could be easily adapted if needed). Agents can be
categorized into an `http_server`, a `boot_` agent, attesters (named
`delegate_<i>` by default), and slots producers (called 0, 1, ... <max> for
convenience).

- `http_server` and `boot_` agents: these two agents are run on the same VM in
  our experiment. The `http_server` will serve an archive containing fresh TZ
  keys generated with `octez-client stresstest` command. On the `boot_` agent,
  we will spawn an L1 and a DAL node what will play the role of bootstrap nodes;

- Attesters: each agent of this kind (agent = VM in our scenario) is dedicated
  to a particular delegate taken from the set of generated keys. On the agent,
  we will run an L1 node, a DAL node with an attester profile and a baker for
  that delegate;

- Slots producers: each agent of this kind (agent = VM in our scenario) is
  dedicated to a particular slot index. On the agent, we will run an L1 node, a
  DAL node with a slot producer profile and execute the `tezos.publish_dal_slot`
  Octogram job at each new level (for levels from 10 to 109) to publish slots to
  DAL nodes and the corresponding slots headers to L1 nodes;

Once Octogram initializes the different agents, the first stage copies various
Octez binaries (client, node, DAL node, and baker) to all agents. Then, fresh
keys are generated and compressed into an archive on `http_server`. On this
agent, a custom protocol file is also generated depending on the chosen network
(mainnet, sandbox, test) and some parameterizable options (number of slots, time
between blocks, attestation lag, etc.). At the end of this stage, an HTTP server
is started to be able to download the generated artifacts.

The third stage just downloads and uncompresses the generated keys. Then, in the
fourth stage, we start a bootstrap Octez and DAL node and activate the alpha
protocol with previously generated custom protocol parameters. Once the Octez
node is bootstrapped, we start the Octez and DAL nodes for attesters (bakers)
and slot producers in the next stages, respectively.


Finally, once the Octez nodes of the attesters and slots producers are
bootstrapped, we start the bakers' binaries in the seventh stage and the slots
producers' jobs in the last one. The slots producers are configured to publish a
DAL slot for every level between 10 and 109. Depending on the value of the
attestation lag flag, injected slots are expected to be attested between levels
10 + attestation_lag and 109 + attestation_lag.

## How to instantiate it

Before running the scenario, you need to compile and execute the OCaml script in
`dal_throughput_gen.ml`. To compile the script, run:

```shell
dune build devtools/cloud-infrastructure/projects/nl-dal/octogram/
```

Then, inside `devtools/cloud-infrastructure/projects/nl-dal/octogram/` directory, run:

```shell
  `git rev-parse --show-toplevel`/_build/default/devtools/cloud-infrastructure/projects/nl-dal/octogram/dal_throughput_gen.exe --auto
```

This command will generate a file named `dal-throughput-scenario.yml` from `dal-throughput-scenario.yml.in`, where:
- The attesters and slots producers agents sections are filled with one agent for each kind;
- All the placeholders of the form %%VAR%% are replaced with some values.

In fact, by default and when run in automatic mode (`--auto` option) without any
additional option, the program replaces the placeholders with some default
values. To see the set of available options, just run:

```shell
`git rev-parse --show-toplevel`/_build/default/devtools/cloud-infrastructure/projects/nl-dal/octogram/dal_throughput_gen.exe --help
```

Note that, when run without the `--auto` command, some values related to the L1 network parameters are read from stdin.

A typical use of the script that allows to generate a scenario with 64 attesters and 32 slots producers is:

```shell
`git rev-parse --show-toplevel`/_build/default/devtools/cloud-infrastructure/projects/nl-dal/octogram/dal_throughput_gen.exe \
    --output dal-throughput-scenario--64-attesters-32-injectors-.yml \
    --ip-addresses gcloud-addresses.json \
    --octogram-path "pull: https://storage.googleapis.com/iguer-tfstate/octogram" \
    --gcloud-binaries-bucket iguer-tfstate \
    --number_of_attesters 64 \
    --number_of_slots_producers 32 \
    --auto
```

Assuming you spawned 97 VMs (64 for the attesters, 32 for the slot producers and
1 for the http server and bootstrap nodes) for the experiment on Google Cloud
with terraform, the IP addresses could be obtained with

```shell
terraform output -json | jq '.ssh_addresses | .value' > gcloud-addresses.json
```



At this point, you should be able to run the generated scenario instance with:

```shell
`git rev-parse --show-toplevel`/octogram orchestrator -a recipe=dal-throughput-scenario--64-attesters-32-injectors-.yml -v -a remote_verbosity=verbose
```
