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

The `jsonnet` image is used in the CI to build and test Grafazos. It
contains the `jsonnet` tools to compile and format jsonnet files. It is
one of the base images: `images.debian-jsonnet` builds it from
`images/base-images/Dockerfile.debian-jsonnet`.

# `ci` images

The `ci` images are a suite of images named `alpine-<component>`
(`runtime`, `monitoring`, `prebuild`, `build`, `test`, `release-page`,
`e2etest`), each defined by its own Dockerfile
(`images/ci/Dockerfile.<component>`) and built on top of the previous
one. They are used in the CI to run a wide variety of jobs; the
`alpine-runtime` and `alpine-build` images are also used as input for
the Octez Docker distribution.

The images are built by `docker buildx bake` from the bake definition
`images/ci/ci-images.hcl`, which declares one target per image and
wires the inter-image dependencies as named build contexts. See
`images/ci/README.md` for the layering and the contents of each image.

In the CI, they are built by the
`images.alpine-ci-all:{amd64,arm64}` jobs of the `base_images.daily`
pipeline. They are merged into multi-arch manifests by
`images.alpine-ci-all.merge`.

## Local build

To build them locally, run the bake from the repository root:

```
$ docker buildx bake -f images/ci/ci-images.hcl --load
```

NB: `--load` ensures that the produced images are loaded in the local
store. You can omit it if the `docker` [build
driver](https://docs.docker.com/build/builders/drivers/) is used, but
not if you use the `docker-container` driver, which is the one mostly
used for CI images.

Local images will be named using the `REGISTRY` and `TAG` bake
variables. The default ones in the bake file `ci-images.hcl` are respectively `octez-local-ci` and `latest`.

So for example, the `runtime` image will have `octez-local-ci/alpine-runtime:latest` as full name.

If you only need to build some targets, you can specify them directly, it will save you some disk storage and time.

For example if you want to build the Octez Docker distribution locally (cf. below), you
will need only `runtime` and `build`:

```
$ docker buildx bake -f images/ci/ci-images.hcl --load runtime build
```

This produces `octez-local-ci/alpine-runtime:latest` and
`octez-local-ci/alpine-build:latest` -- the `REGISTRY` and `TAG` bake
variables default to those local names. The images a named target builds
upon (here `prebuild` and `monitoring`) are built as well, but only into
the build cache.

# Building the Octez Docker Distribution

To build the Octez Docker distribution locally, use the script
`./scripts/create_docker_image.sh`. Called without parameters, this
script will build the Octez Docker Distribution with the *released*
set of executables (see `scripts-inputs/released-executables`) and
without EVM artifacts.

Building the Octez Docker distribution requires two of the CI images
as input: `alpine-runtime` (the base of the distribution variants) and
`alpine-build` (the build environment). Their definition can be found
in `images/ci`, and they can be built locally with:

```
$ docker buildx bake -f images/ci/ci-images.hcl --load runtime build
```

By default, the script `./scripts/create_docker_image.sh` uses the
locally baked images `octez-local-ci/alpine-runtime:latest` and
`octez-local-ci/alpine-build:latest`. To use other images, for
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
the tezos/tezos CI, by the `images.alpine-ci-all` jobs. To build the
distribution FROM a specific build of those images, pass their full
references. For the images the CI currently uses, the tag is the value
of `base_images_tag` in `ci/lib_tezos_ci/tezos_ci.ml`:

```
$ registry="$(. ./scripts/version.sh; echo "$GCP_PROTECTED_REGISTRY")/tezos/tezos"
$ tag=<base_images_tag>
$ ./scripts/create_docker_image.sh \
    --runtime-image "${registry}/alpine-runtime:${tag}" \
    --build-deps-image "${registry}/alpine-build:${tag}"
```

Reading from that registry requires the corresponding credentials.
Builds on unprotected refs push to the public registry
(`GCP_PUBLIC_REGISTRY`) rather than the protected one, under the tag
`<ref-slug>-<short-sha>`.

## Using local CI images

First, build the `ci` images locally:

```
$ docker buildx bake -f images/ci/ci-images.hcl --load runtime build
```

Both the bake and `./scripts/create_docker_image.sh` default to
`octez-local-ci/alpine-*:latest`, so you can now simply rebuild the Octez
Docker distribution and it will use the locally built CI images:

```
$ ./scripts/create_docker_image.sh
```

To use a different naming scheme, set `REGISTRY` and `TAG`: they drive
both commands, since `create_docker_image.sh` reads the same two bake
variables.

```
$ export REGISTRY=my-ci TAG=wip
$ docker buildx bake -f images/ci/ci-images.hcl --load runtime build
$ ./scripts/create_docker_image.sh
```

Passing `--runtime-image` / `--build-deps-image` explicitly, as in the
previous section, still overrides both.
