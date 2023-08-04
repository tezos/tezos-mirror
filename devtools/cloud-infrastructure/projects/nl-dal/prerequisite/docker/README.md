# Docker

Builds a docker image and pushes it to Google Docker Image Registry (Artifact Registry).
The image will:

- Store the given SSH public key in it's `.ssh/authenticated_keys`.
- Store the zcash-params in `/usr/local/share/zcash-params`
- Start an SSH daemon at port 30000.

## How to Use

First initialize Terraform:

```shell
terraform init
```

If you want to (re)initialize the authorized_keys file in the docker image, run:

```shell
DOCKER_BUILDKIT=1 ./push.sh <os> <ssh_public_key_file> bootstrap
```

if not, run:

```shell
DOCKER_BUILDKIT=1 ./push.sh <os> <ssh_public_key_file>
```
