# Opentelemetry tracing for the EVM node

This guide explains how to enable and visualize kernel traces in Etherlink using
Datadog APM and the Grafana stack

## Datadog

### Prerequisites

Before starting, ensure you have the following:

* Access to a Datadog account with APM enabled.
* A valid Datadog API key.
* Docker installed and running on your machine.


### Launch the Datadog APM Collector

From the root of the Tezos repository, navigate to the profiling directory:

    cd etherlink/profiling

Start the Datadog APM collector using Docker Compose:

    DD_API_KEY=<datadog-api-key> docker compose up otelcol -d

Wait a few moments to ensure all containers are up and running before proceeding.

### Compile the Kernel with Tracing Enabled

Build the EVM kernel with the tracing feature flag:

    make -f etherlink.mk EVM_KERNEL_FEATURES=tracing evm_kernel.wasm

This produces a tracing-enabled kernel at the root of the repository.

### Configure and Run the EVM Node

In the data directory (`<path-data-dir>`) specified when you will launch the node, create a configuration file named `config.json`:

```json
{
    "performance_profile": "performance",
    "opentelemetry": {
        "enable": true,
        "trace_host_functions": false,
        "environment": "<env-name>",
        "instance_id": "<instance-id>"
    }
}
```

**Note:**

* You can set any custom <env-name> and <instance-id>, as long as they are
  recognizable to you in Datadog APM.
* Ensure this file is placed in the root of the <path-data-dir> directory used
  by the node.
* You can set trace_host_functoins to true but it will add a lot of verbosity to
  the traces. If you do not intend to specifically debug the host functions, it
  is recommend to keep that option off.

Now, start the EVM node with the tracing kernel:

```sh
octez-evm-node run sandbox \
  --replicate \
  --network mainnet \
  --init-from-snapshot \
  --data-dir <path-data-dir> \
  evm_kernel.wasm \
  --kernel-verbosity debug
```

### Viewing Traces in Datadog

Once the node is running, open your Datadog dashboard and navigate to:
APM → Explorer

Search for the `<env-name>` you specified in `config.json`.
You should now see your kernel traces being reported in near real time.
Here's an example of traces you can observe:

> **Nota Bene**
>
> You can easily add traces and recompile the kernel via the macro (or
> procedural macro) that were added in the kernel. You can use git to grep for
> `__trace_kernel!` or `#[trace_kernel]` in the kernel codebase to look out for
> examples and add more to your needs.

## Grafana

Start all services with

```
docker compose up -d
```

and configure your EVM node as described in the previsous section.

You can now head over to http://localhost:3000 to use the Grafana UI (login:
`admin`, password: `admin`). You will find the traces and logs in the
**Explore** tab of Grafana.

Select the source _Tempo_ if you want to see traces and source _Loki_ if you
want to see logs.
