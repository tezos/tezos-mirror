# `tezos/tezos` Docker images

This directory contains the definition of Docker images used to build
the Docker distribution of Octez.

The images, their content and indented usage, are:

| Image                      | Contents                                | Usage                          |
|----------------------------|-----------------------------------------|--------------------------------|
| `rust-toolchain`           | cargo                                   | CI: kernel build, test and SDK |
| `client-libs-dependencies` | kaitai-struct-compiler, xxd, java, node | CI: Kaitai e2e tests           |

For more details on the contents and usage of each image, see below
and in the header comment of each corresponding Dockerfile.

## Input hashes

Images are rebuilt on-demand in the Tezos CI whenever one of their
*inputs* change.  The inputs are image-specific and correspond to a
set of paths in the tree. Images are tagged with the input hash and
the branch on which they were produced (see `images/image_tag.sh` for
more details).

## Image build contexts

Build contexts for image builds are produced from the images build
context directory `images/IMAGE_NAME`. All files in this folder will
be available to the Dockerfile during build.

## Image provenance

Provenance meta-data can be found by inspecting the labels of an
image's manifest:

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

## Usage in the Tezos CI

If you modify the definition of these images above, then the Tezos CI
will automatically rebuild it and the new image will be used in
subsequent jobs of the pipeline. This works by tagging each image with
its input hash, and putting this tag as a variable in a [dotenv report
artifact](https://docs.gitlab.com/ee/ci/yaml/artifacts_reports.html#artifactsreportsdotenv).
Jobs that use the image refer to this variable in their `image:`
field and thus reuse the image directly.

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

# Building the Octez Docker Distribution

To build the Octez Docker distribution locally, use the script
`./scripts/create_docker_image.sh`. Called without parameters, this
script will build the Octez Docker Distribution with the *released*
set of executables (see `scripts-inputs/released-executables`) and
without EVM artifacts.

Building the Octez Docker distribution requires the
[tezos/opam-repository](https://gitlab.com/tezos/opam-repository)
build-deps images as input. By default, build-deps images of version
`opam_repository_tag` in `scripts/version.sh` is pulled. To use a
custom version, for instance when building the build-deps images
locally, see the parameters `--build-deps-image-name` and
`--build-deps-image-version` of the script
`./scripts/create_docker_image.sh`.

To build the Octez Docker Distribution with EVM artifacts, pass
`--docker-target with-evm-artifacts` to
`./scripts/create_docker_image.sh`. Building the EVM artifacts
requires the rust-toolchain image as input. By default, the script
will attempt to re-use the latest version of this image built on the
`master` branch of the tezos/tezos CI. If you need to use a different
image for the rust-toolchain image, follow the instructions in
`./images/README.MD` and `./images/create_rust_toolchain_image.sh` to
create one. Then specify the `--rust-toolchain-image-name` and
`--rust-toolchain-image-tag` parameters to
`./scripts/create_docker_image.sh` accordingly.

For more info on how to configure:
 - including naming of the built images in the Octez Docker distribution;
 - the input images used;
 - the contents of the distribution, and
 - the configuration of the version number reported by included executables;

see `./scripts/create_docker_image.sh --help`.
