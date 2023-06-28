When originating a Smart Rollup, there is a maximum size of kernel supported. If your kernel is too large,
you must instead use an installer.

To generate the `installer` kernel, you can use the `smart-rollup-installer` tool.

## Prerequisites

You will need:

- at least **rust-1.66** installed (you can check with `cargo --version`), with the `wasm32-unknown-unknown` toolchain.
- the [WebAssemblyBinaryToolkit](https://github.com/WebAssembly/wabt) (`wabt`) installed. In particular, the `wasm-strip` executable.
- `$HOME/.cargo/bin` should be available on your `$PATH`.
- `clang>=11` installed, and set as `CC` environment variable.

## Installing `smart-rollup-installer`

Run the following to install:

```
cargo install tezos-smart-rollup-installer
```

> to get the latest development version, you can instead run
> `cargo install tezos-smart-rollup-installer --git https://gitlab.com/tezos/tezos.git`

This will place the `smart-rollup-installer` executable at `$HOME/.cargo/bin` by default.

## Creating the installer kernel

Assuming the kernel you'd like to run is called `kernel.wasm`, you can create an installer kernel for it:

```
smart-rollup-installer get-reveal-installer \
    --upgrade-to kernel.wasm \
    --output installer.hex \
    --preimages-dir <preimages-dir>
```

where `<preimages-dir>` is the directory to save the *preimages* of the kernel. The installer will request these through the *reveal data* channel.

And you can now originate the rollup by supplying `installer.hex` to the `octez-client originate smart rollup` command, setting `KERNEL=$(cat installer.hex)`.

## Running a rollup node

To be able to run a rollup node for the rollup, you will need to copy the contents of the `<preimages-dir>` to `${ROLLUP_NODE_DIR}/wasm_2_0_0` - where `${ROLLUP_NODE_DIR}` is the data directory of your rollup node.

Once you've copied these across, you should then be able to run your rollup node, and the installer kernel will automatically upgrade.

For more information on originating the rollup & running a rollup node, see the [rollup docs](https://tezos.gitlab.io/alpha/smart_rollups.html).

## Using the `octez-smart-rollup-wasm-debugger`

You can also try this out by using the debugger, which may be easier than running a rollup node. To do so you need to use the `.wasm` extension on the `--output` argument:

```
smart-rollup-installer get-reveal-installer \
    --upgrade-to kernel.wasm \
    --output installer.wasm \
    --preimages-dir <preimages-dir>
```

You can then run the installer in the [debugger](https://tezos.gitlab.io/alpha/smart_rollups.html#testing-your-kernel), using the `--preimage-dir` argument to point to the preimages generated previously:

```
octez-smart-rollup-wasm-debugger --kernel installer.wasm --inputs inputs.json --preimage-dir <preimages-dir>
```
