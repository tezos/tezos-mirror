## Grafazos: Grafana dashboards for Octez node monitoring.

Grafazos allows to generate dashboards from jsonnet programs.

It relies on the metrics exposed by the Octez Tezos node, see http://tezos.gitlab.io/developer/openmetrics.html, to build grafana dashboards.

To reflect the fact that various networks or protocol may expose different metrics, the branches are structured such that:

- master: compatible with mainnet and its current protocol
- master-PROTO_NAME: (not always available) compatible with the aforementioned protocol, such as master-ithaca.

To maintain the backward compatibility with the `tezos-metrics` tool previously used to query metrics, a legacy branch is proposed. However, this branch is deprecated and not maintained anymore:
- legacy-tezos-metrics-ithaca: compatible with the former version of Grafazos which was based on `tezos-metrics`

### Resources:

- Grafonnet: https://github.com/grafana/grafonnet-lib

- Jsonnet: https://jsonnet.org/

### Tools:

- Emacs mode: https://github.com/mgyucht/jsonnet-mode

### Run:

First, run
```
git submodule init
git submodule update
```

To create a dashboard with for branch "master" in the title (default)

    make

To create a dashboard for a different branch

    BRANCH=foo make
