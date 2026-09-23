# `tezos/tezos` Docker images

This directory contains the definition of Docker images used to build
the Docker distribution of Octez.

The images, their content and indented usage, are:

| Image                      | Contents                                | Usage                          |
|----------------------------|-----------------------------------------|--------------------------------|
| `jsonnet`                  | jsonnet                                 | CI: Grafazos jobs              |
| `ci`                       | ocaml, cargo, npm, python ...           | CI: the majority of jobs       |

For more details on the contents and usage of each image, see below,
in the header comment of each corresponding Dockerfile and in the
`IMAGE/README.md` file when applicable.

## Image tags

Images are built by `base_images.daily` pipelines. They are
tagged after the commit they were built from
(`<ref-slug>-<short-sha>`, e.g. `master-18884dda`).

## Image build contexts

Images are built from the repository root and reference their inputs
by real repo-root paths, so their build context is the whole tree (see
`images/ci/ci-images.hcl` and `scripts/ci/build-base-images.sh`).

NB: docker never resolves symlinks in a build context. Building from the
root and referencing files by their real path removes the need for
symlinks altogether.

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

Images are referenced in the CI by the shared tag `base_images_tag` in
`ci/lib_tezos_ci/tezos_ci.ml`. This tag should contain the commit hash
for which a `base_images.daily` ran. The CI uses then the images
produced by the latest `base_images.daily` pipeline that ran on this
commit.

NB: in order to have fresh images, `base_images.daily` pipeline run
regularly on the commit that is referred to in `base_images_tag`. Note
also that due to our retention policy, older CI images will be erased
(currently after 30 days since the last push).

Bumping the commit hash in `base_images_tag` is what makes the CI use a newer build.

If you want to modify the images used in CI jobs, you should:
1. merge your changes on `master`
2. make sure that a `base_images.daily` pipeline ran successfully on a commit recent enough to contain your changes
3. merge the bump of the tag.

Ideally, the tag should be bumped not too long (less than a day) after
modifications in the CI image are merged.

# `jsonnet` image

The `jsonnet` image is used in the CI to build and test Grafazos.  It
contains the `jsonnet` tools to compile and format jsonnet files.  ~To
build this image for local use, run `create_image.sh "grafazos"`~
(This has changed and building this image locally is currently not
straightforward. Simplifying local build is planned as future work.)

# `ci` images

The `ci` images is a suite of images, defined as different layer in
the Dockerfile `ci/Dockerfile`. It is used in the CI to run a wide
variety of jobs.  Its `runtime` and `build` layers are also used as
input for the Octez Docker distribution.  To build these images for
local use, run `create_ci_images.sh`.


# Common files in `common`

Files that are shared between image built contexts are stored in
`common` and are symlinked into the build contexts for images that
require them. `docker build` does not resolve symlinks, but we work
around this by tarring the image's build context directory, having tar
resolve symlinks and piping the result to `docker build`. See
`create_ci_images.sh` for an example.

# Building the Octez Docker Distribution

To build the Octez Docker distribution locally, use the script
`./scripts/create_docker_image.sh`. Called without parameters, this
script will build the Octez Docker Distribution with the *released*
set of executables (see `scripts-inputs/released-executables`) and
without EVM artifacts.

Building the Octez Docker distribution requires some of the CI images
as input. The definition of these input images can be found in
`images/ci`, and they can be built locally using
`./images/create_ci_images.sh.`

By default, the script `./scripts/create_docker_image.sh` uses the
CI images that correspond to the current state of the checkout (see
`images/image_tag.sh` for more info). To use other images, for
instance the CI images built in a specific pipeline, pass their full
references (`name:tag`) with the parameters `--runtime-image` and
`--build-deps-image`.

For more info on how to configure:
 - naming of the built images in the Octez Docker distribution,
 - input images used,
 - contents of the distribution, and
 - configuration of the version number reported by included executables,

see `./scripts/create_docker_image.sh --help`.

## Using the CI images from tezos/tezos CI

Like the Octez Docker distribution, the CI images are also built in
the tezos/tezos CI. By default, `./scripts/create_docker_image.sh` is
configured to pull the CI images from the CI's protected Docker
registry if they cannot be found locally.

Note that if the image you want to use is from a branch that has not
yet been merged to `master`, then it will not be in the protected
Docker registry, but in the public one. To configure the script to use
the public registry:

```
$ ci_image_name="$(. ./scripts/version.sh; echo "$GCP_PUBLIC_REGISTRY")/tezos/tezos/ci"
$ ci_image_tag="${ARCH:-amd64}--$(./images/image_tag.sh images/ci)"
$ ./scripts/create_docker_image.sh \
    --runtime-image "${ci_image_name}/runtime:${ci_image_tag}" \
    --build-deps-image "${ci_image_name}/build:${ci_image_tag}"
```

## Using local CI images

First, build the `ci` images locally:

```
$ ./images/create_ci_images.sh
```

The newly built images will use the same naming scheme as the images
built in the tezos/tezos CI. Therefore, you can now simply rebuild the
Octez Docker distribution and it will automatically use the locally
built CI images:

```
$ ./scripts/create_docker_image.sh
```

To make this more explicit, run:

```
$ ./images/create_ci_images.sh \
    --image-base octez-local-ci \
    --tag-suffix ""
$ ./scripts/create_docker_image.sh \
    --runtime-image octez-local-ci/runtime:amd64 \
    --build-deps-image octez-local-ci/build:amd64
```

The first command will create the set of CI images on the following naming scheme:

 - `octez-local-ci/runtime:amd64`
 - `octez-local-ci/prebuild:amd64`
 - ...

(These images are always tagged by architecture, and the architecture defaults to amd64).

The parameters to the second command state the full references of the
images `create_docker_image.sh` builds FROM.
