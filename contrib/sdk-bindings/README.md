<!--
SPDX-FileCopyrightText: 2025 Trilitech <contact@trili.tech>

SPDX-License-Identifier: MIT
-->

# Client Libs: SDK bindings

Multi-language bindings for the Rust SDK.  These allow for high-level logic to be written in multiple languages,
whilst re-using the same set of functionalities.

Initially, the focus is on providing the cryptographic operations required for interacting with the Tezos blockchain.
In the future, additional support - such as local forging of operations - may be added.

## Supported Languages

Current supported languages are `python`, `kotlin` and `swift`. Additionally, `rust` is supported given that this is the language the SDK itself is developed in.

## Development

### Pre-requisites

It is recommended to use the provided [nix shell](./shell.nix) - See [nix installation](https://nixos.wiki/wiki/Nix_Installation_Guide). Nix is available for both Linux and MacOS platforms.

Once `nix` is installed, run the following to be brought into a shell with all required dependencies available.

```sh
cd contrib/sdk-bindings
nix-shell
```

Alternatively, you may use the Dockerfile located [here](../../images/rust-sdk-bindings/Dockerfile), or ensure you have all
such dependencies installed otherwise on your system.

### Building

Run the following to build the core rust library, and generate bindings for all supported languages.

```sh
make bindings
```

This will create bindings under `./<language>/uniffi_generated_files`.

### Tests

A test-suite for each supported language is included, you can run them all by running:

```sh
make test
```
