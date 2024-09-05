## Grafazos: Grafana dashboards for Octez node monitoring.

Grafazos allows to generate dashboards from jsonnet programs.

It relies on the metrics exposed by the Octez Tezos node, see http://tezos.gitlab.io/developer/openmetrics.html, to build grafana dashboards.

To reflect the fact that various networks or protocol may expose different metrics, the branches are structured such that:

- `master`: compatible with mainnet and its current protocol
- `master-<proto_name>`: (not always available) compatible with the aforementioned protocol, such as master-ithaca

### Resources

#### Jsonnet

To build the dashboards, you need to install `jsonnet` with version `0.18.0` minimum.
We recommend to use https://github.com/google/go-jsonnet.

#### Grafonnet

`Grafonnet` is the jsonnet library used to generate grafana dashboards.

`Grafonnet` should be installed using `jsonnet-bundler` (https://github.com/jsonnet-bundler/jsonnet-bundler/).

To do so, run `go install -a github.com/jsonnet-bundler/jsonnet-bundler/cmd/jb@latest` and update your environment variables such as `export GOPATH=$HOME/go` and `export PATH=$PATH:$GOPATH/bin`.

Once `jsonnet-bundler` installed you can import dependencies (here `grafonnet`) by running the following:

```sh
jb install
```

### Tools

- Emacs mode: https://github.com/mgyucht/jsonnet-mode

### Build

To create the dashboards for branch `master` in the title (default)

```sh
make
```

The dashboards will be availbale in the `output/` folder.

You can also create a specific dashboard with

```sh
make dashboard_name
```

with `dashboard_name` one of:
- `compact`: a single page compact dashboard which aims to give a
  brief overview of the node's health,
- `basic`: a simple dashboard displaying detailed node's metrics,
- `logs`: same as `basic` but also displaying node's logs (thanks to [Loki and promtail](https://github.com/grafana/loki))
- `full`: same as `logs` but also displaying hardware metrics (thanks to [netdata](https://www.netdata.cloud/))
- `dal-basic`: a simple dashboard displaying some DAL node's metrics,

To create the dashboards for a different branch

```sh
BRANCH=foo make
```

By default, the instance names label is set to `instance`. If you are
using a particular label for the instance names, you can use

```sh
NODE_INSTANCE_LABEL=my_instance_label
```

If you need hardware metrics, note that, by default, the storage stat considered is the used space of the whole disk.
Optionnally you can enable storage monitoring with ``filecheck``:

```sh
STORAGE_MODE=filecheck
```

### Distribution

Grafana dashboards (JSON files) are automatically released on git tags to [GitLab packages of this project](https://gitlab.com/nomadic-labs/grafazos/-/packages).

They are also manually released to [Grafana.com](https://grafana.com/grafana/dashboards/)
so they can insealy imported via ID.
