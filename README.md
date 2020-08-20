## Grafazos: Grafana dashboards for Tezos node monitoring.

Grafazos allows to generate dashboards from jsonnet programs.

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
