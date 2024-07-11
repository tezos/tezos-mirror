## Grafazos: Grafana dashboards for Octez node monitoring.

Grafazos allows to generate dashboards from jsonnet programs.

It relies on the metrics exposed by the Octez Tezos node, see http://tezos.gitlab.io/developer/openmetrics.html, to build grafana dashboards.

To reflect the fact that various networks or protocol may expose different metrics, the branches are structured such that:

- `master`: compatible with mainnet and its current protocol
- `master-<proto_name>`: (not always available) compatible with the aforementioned protocol, such as master-ithaca

To maintain the backward compatibility with the `tezos-metrics` tool previously used to query metrics,
a legacy branch is proposed. However, this branch is deprecated and not maintained anymore:

- legacy-tezos-metrics-ithaca: compatible with the former version of Grafazos which was based on `tezos-metrics`

### Resources

- Grafonnet: https://github.com/grafana/grafonnet-lib

- Jsonnet: https://jsonnet.org/

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

**Deprecated:**
If you use an old versoin of netdata you might need to use the legacy metrics by setting the `NETDATA` environment variable:

```sh
NETDATA=legacy
```

### Distribution

Grafana dashboards (JSON files) are automatically released on git tags to [GitLab packages of this project](https://gitlab.com/nomadic-labs/grafazos/-/packages).

They are also manually released to [Grafana.com](https://grafana.com/grafana/dashboards/)
so they can insealy imported via ID.

### Dashboards built with the new version of Grafonnet

The current dashboards are built we a vendored old version of `grafonnet`.

New dashboard are being introduced. These dashboards use the latest version of `grafonnet` that is introduced as a dependency.

#### Dependencies

To built the new dashboard, you need to install `jsonnet` with version `0.18.0` minimum.
We recommend to use https://github.com/google/go-jsonnet.

Grafonnet should be installed using `jsonnet-bundler` (https://github.com/jsonnet-bundler/jsonnet-bundler/).
Once `jsonnet-bundler` installed you can import dependencies (here `grafonnet`) by running the following:

```sh
jb install
```

#### Build

For now the new dashboards are built seperatly with specific rules in the `Makefile`.

For instance, to build the new `octez-compact` dashboard you need to run:

```sh
make octez-compact-new
```

Note that environment variables available for the old dashboards can still be used.
