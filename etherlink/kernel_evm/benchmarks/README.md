
# Context

To help with reproducibility of benchmark, a Dockerfile was added, to be able
to build an image that could easily run on a dedicated benchmark server, without
any need for installation.

This image is distinct from the images built by the CI, but is based on one of
them: `tezos/debug`, to which was added an unstripped version of the kernel with
a faucet (cf `scripts/benchmarks/players/faucet.json`).

The image needs to be built from the tezos sources, but only requires the
`src/kernel_evm/benchmarks` directory.

Let's assume `<WORKDIR>` as the working directory for the benchmark suite, and
`<WORKDIR>/output` containing the results of the benchmarks.

# Instrumentation

A feature flag `benchmark` was added to the kernel, to activate the monitoring
in the kernel. For the benchmarking to work, an unstripped version of the kernel,
compiled with the benchmark flag, is necessary.

# Procedure

## To initialize the repo

To avoid pulling all the `tezos` repo, a sparse-checkout can be done, to pull
only the `src/kernel_evm/benchmarks` which must contain everything needed by
the `Dockerfile`.`

```
cd <WORKDIR>
mkdir tezos
cd tezos
git init
git remote add -f origin https://gitlab.com/tezos/tezos.git
git sparse-checkout set src/kernel_evm/benchmarks
git checkout master
cd ..
ln -s tezos/src/kernel_evm/benchmarks/docker/start_image.sh start_image.sh
```

## To build the image
```
cd <WORKDIR>
git -C tezos pull
docker build tezos/src/kernel_evm/benchmarks -t evm-benchmark
```

The image can be created from any branch, but will always pull the `tezos/tezos-debug` built from master by default. To change that, the Dockerfile can be updated locally to point to a different branch then master by editing the first line:
```
FROM registry.gitlab.com/tezos/tezos/debug:amd64_<branch_name>
```
Note that `@` are replaced by `_` in the branch name.

# To run

A script was created to help start the benchmark script:
```
cd <WORKDIR>
./start_image.sh
```
