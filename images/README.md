# `tezos/tezos` Docker images

This directory contains the definition of Docker images used to build
the Docker distribution of Octez.

The images, their content and indented usage, are:

| Image                             | Contents                           | Usage                             |
|-----------------------------------|------------------------------------|-----------------------------------|
| `rust-toolchain`                  | cargo                              | CI: kernel build, test and SDK    |
| `client-libs-dependencies`        | kaitai-struct-compiler, xxd, java  | CI: Kaitai e2e tests              |

For more details on the contents and usage of each image, see below
and in the header comment of each corresponding Dockerfile.

## Image provenance

Images are rebuilt on-demand in the Tezos CI whenever one of their
inputs change.  The inputs are image-specific and correspond to a set
of paths in the tree. Images are tagged with the input hash and the
branch on which they were produced. More provenance meta-data can be
found by inspecting the labels of an image's manifest:

 - `com.tezos.build-pipeline-id`: The ID of the CI pipeline in which
   the image was produced.
 - `com.tezos.build-pipeline-url`: The URL of the CI pipeline in which
   the image was produced.
 - `com.tezos.build-job-id`: The ID of the CI job in which the image
   was produced.
 - `com.tezos.build-job-url`: The URL of the CI job on which the image
   was produced.
 - `com.tezos.build-tezos-revision`: The git commit SHA for which the
   image was produced.

These labels can be extracted from a locally pulled image using
`docker image inspect`.

# `rust-toolchain` image

The `rust-toolchain` image is used in the CI to build and test
kernels, and to build the kernel SDK. It is also used to build the
`evm_kernel` included in the Octez Docker distribution. To build this
image for local use, run `create_rust_toolchain_image.sh`.

# `client-libs-dependencies` image

The `client-libs-depencies` image is used in the CI to run end-to-end
tests of client-libs. To build this image for local use, run
`create_client_libs_dependencies_image.sh`.

# Common files in `common`

Files that are shared between image built contexts are stored in
`common` and are symlinked into the build contexts for images that
require them. `docker build` does not resolve symlinks, but we work
around this by tarring the image's build context directory, having tar
resolve symlinks and piping the result to `docker build`. See
`create_client_libs_dependencies_image.sh` for an example.
