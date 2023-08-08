# Docker

Adds the given SSH key to the docker image and pushes it to Google Docker Image Registry (Artifact Registry) using the `/devtools/cloud-infrastructure/libraries/docker-registry` library.

## How to Use

First initialize Terraform:

```shell
terraform init
```

The next step would vary depending on what you want to do.

### Case 1: Add your SSH public key to an existing docker image

Typically, this is what you would want to do, and in most instances, you should use `debian` as the value for `<os>`.

```shell
DOCKER_BUILDKIT=1 ./push.sh <os> <ssh_public_key_file>
```

### Case 2: Initialize and push a new docker image

You would want to do this if, for example, you want to create a new docker image with a separate `<os>`.

```shell
DOCKER_BUILDKIT=1 ./push.sh <os> <ssh_public_key_file> bootstrap
```
