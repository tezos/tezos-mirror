## Grafazos: Grafana dashboards for Tezos node monitoring.

Grafazos allows to generate dashboards from jsonnet programs.

To reflect the fact that various networks or protocol may expose different metrics, the branches are structured such that:

- master: compatible with mainnet and its current protocol,
- master-PROTO_NAME: compatible with the aforementioned protocol, such as master-Ithaca.

### Resources:

- Grafonnet: https://github.com/grafana/grafonnet-lib

- Jsonnet: https://jsonnet.org/

### Tools:

- Emacs mode: https://github.com/mgyucht/jsonnet-mode

### Run:

To create a dashboard with for branch "master" in the title (default)

    make

To create a dashboard for a different branch

    BRANCH=foo make
