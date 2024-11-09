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


## [ci-docker]

- Pipeline defined in [./.gitlab-ci.yml] is created when commit is pushed to `ci-docker-latest-release`.
- At this stage, the pipeline will succeed and build and push `ci-docker` images to the GCP and GitLab registries.
```
docker image pull us-central1-docker.pkg.dev/nl-gitlab-runner/registry/tezos/tezos/ci-docker:branch-ci-docker-latest-release
docker image pull registry.gitlab.com/tezos/tezos/ci-docker:branch-ci-docker-latest-release
```
- However these images are not yet used in the regular CI pipelines.
