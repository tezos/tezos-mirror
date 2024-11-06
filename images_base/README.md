# (WIP) Import of images in [tezos/docker-images](https://gitlab.com/tezos/docker-images/)

This is a temporary folder for importing
[ci-docker](https://gitlab.com/tezos/docker-images/ci-docker) and
[ci-release](https://gitlab.com/tezos/docker-images/ci-release).

These are base images used in `tezos/tezos` CI jobs:

- `ci-docker`: used in jobs that build Docker images
- `ci-release`: used in jobs that release Octez products on GitLab.

Note that
[tezos/docker-images](https://gitlab.com/tezos/docker-images/)
contains a third image,
[ci-package](https://gitlab.com/tezos/docker-images/ci-package). This
image appears unused in this repository's CI.
