# Cookbook

This document provides examples of commands you can use with
tezt-cloud. The focus is primarily on localhost mode, allowing you to
execute these commands without requiring access to GCP.

Before using this cookbook, ensure you are familiar with the basics of
tezt-cloud. If not, please refer to the [tezt-cloud
readme](https://gitlab.com/tezos/tezos/-/blob/master/tezt/lib_cloud/README.md)
first.

## Simple Test

Use this command to verify that everything is well-configured locally:

```
dune exec tezt/tests/cloud/main.exe -- cloud simple -v --log-file /tmp/log --localhost
```

To check your configuration with GCP, use the following command:

```
dune exec tezt/tests/cloud/main.exe -- cloud simple -v --log-file /tmp/log
```

## Running a Basic Scenario

With Octez, you can run a scenario using that will start a fresh
network (sandboxed mode) using protocol Alpha and bake some blocks:

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost
```

By default, in localhost mode, some services like Prometheus are not
started. To enable them, run:

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost --prometheus --grafana --website
```

Once enabled, you can access the various services at
`http://localhost:8080`. Additional services are available. Refer to
the manual using the `--help` flag for more details.

## Running on Ghostnet

You can execute these scenarios on Ghostnet. Note that some options
may currently be deactivated such as `--stake` and just need to be
implemented.

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost --network ghostnet
```

## Running DAL Producers

The number of DAL producers can be specified using the `--producers`
option. For example the following command will run 3 producers:

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost --network ghostnet --producers 3
```

This command runs 3 producers.

## Specifying the Stake

In sandboxed mode (the default), you can specify the number of bakers
and their respective stake ratios using the `--stake` option. The
following command runs 3 bakers, where the first has `1/6` of the
stake, the second `2/6`, and the third `3/6`.

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost --stake 1,2,3
```

## Running a New Etherlink Instance

To originate a new Etherlink instance and generate operations for it, use the following command:

```
dune exec tezt/tests/cloud/main.exe -- cloud dal -v --log-file /tmp/log --localhost --etherlink --etherlink-producers 1
```

## Running Maintenance Jobs

`tezt-cloud` comes via dedicated jobs. This section details some of them.

### Cleaning up the VMs

With `tezt-cloud`, you can execute maintenance jobs to manage active
developments. For example, if you encounter errors related to port
allocation (e.g., ports already in use), it is likely that a previous
run left some residual resources. In such cases, you can clean up the
VMs (remove running processes launched by `tezt-cloud`) using this
command:

```
dune exec tezt/tests/cloud/main.exe -- cloud clean up -v
```
