# Building Etherlink from source

Etherlink codebase is hosted as part of [Tezos
monorepository](https://gitlab.com/tezos/tezos), in the `etherlink/` directory.
It primarily features two components: Etherlink kernel (implemented in Rust)
and the so-called EVM node `octez-evm-node` (implemented in OCaml).

A good starting point for building Etherlink from source is the Octez
documentation website, and the section explaining how to [setup the development
environment from scratch in
particular](https://tezos.gitlab.io/introduction/howtoget.html#compiling-with-make).

## Building the node

`octez-evm-mode` is part of the binaries built by calling `make` without any
argument. That being said, this command builds the full Octez software suite.
More simply, it is possible to build `octez-evm-node` exclusively.

```bash
make octez-evm-node
```

## Building the kernel

### For testing

The kernel of Etherlink is implemented in Rust, and built on top of the [Rust
SDK for building Smart Rollups](https://crates.io/crates/tezos-smart-rollup).
Its build process is defined in the `etherlink.mk` Makefile.

To prepare your development environment to build the kernel, a dedicated 
`build-deps` rule has been defined.

```bash
make -f etherlink.mk build-deps
```

Additionally, you can run `make -f etherlink.mk build-dev-deps` to install
development dependencies (such as Clippy for instance).

A smart rollup kernel is a WASM binary; in the case of Etherlink, this binary
is called `evm_kernel.wasm`.

```bash
make -f etherlink.mk evm_kernel.wasm
```

### For production

The procedure described in the previous section is relatively fast, but it has
one drawback: Rust builds are not reproductible by default.

We provide an alternative build procedure relying on a Docker container to get
reproductible builds. Thanks to this, it becomes possible for third-parties to
build a given commit of Etherlink from source, without relying on any trusted
parties. It is a necessary component of the governance process of Etherlink, as
WASM binaries are not reputed for their auditability.

From the root of Tezos repository, simply run the following command.

```bash
etherlink/scripts/build-wasm.sh
```

This will create a new directory `etherlink/kernels-<CURRENT_COMMIT>` which
will contain `evm_kernel.wasm`.

## Building the installer

The Etherlink kernel largely exceeds the 32KB limit of Tezos manager
operations. To originate a new Etherlink instance (that is, a new smart rollup
running the Etherlink kernel), it is necessary to build the
`smart-rollup-installer` program from the Smart Rollups Rust SDK.

This time, the necessary build rules are defined in the `kernels.mk` Makefile.

To prepare your development environment to build the kernel, a dedicated 
`build-deps` rule has been defined.


```bash
make -f kernels.mk build-deps
```

Then, to build `smart-rollup-installer`, use the following rule.

```bash
make -f kernels.mk kernel_sdk
```
