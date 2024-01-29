# `tezos/tezos` Docker images

This directory contains the definition of Docker images used to build
the Docker distribution of Octez.

The images, their content and indented usage, are:

| Image                             | Contents                           | Usage                             |
|-----------------------------------|------------------------------------|-----------------------------------|
| `rust-toolchain`                  | cargo                              | CI: kernel build, test and SDK    |

For more details on the contents and usage of each image, see below
and in the header comment of each corresponding Dockerfile.

# `rust-toolchain` image

The `rust-toolchain` image is used in the CI to build and test
kernels, and to build the kernel SDK. It is also used to build the
`evm_kernel` included in the Octez Docker distribution.
