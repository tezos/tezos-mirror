# Docker image to build other Docker images

* Docker client to build multi-arch images (from an x86 host)
* Extra tools to create a user Docker JSON configuration and push to Docker Hub, GitLab container registry and AWS ECR

This image is meant to be used in pipelines to build other images. Directly use the host Docker socket
with the Docker client. `DOCKER_VERSION` is already set in GitLab runners/executors configuration to match.

We are building this image without Docker, because this *resulting image* is meant to be used for that matter.
Hence the usage of Kaniko.

## Tools

### Kaniko

* No need for a priviled Docker container running with the `root` user
* No need for Docker-in-Docker or access to the host Docker socket

https://github.com/GoogleContainerTools/kaniko

### AWS Docker Credential Helper

Log in to AWS ECR automatically to push/pull Docker images

https://github.com/awslabs/amazon-ecr-credential-helper

### Buildx

Build multi-architecture Docker images (no need to merge manifests) and push them to multiple registries

https://docs.docker.com/buildx/working-with-buildx/

### regctl

Client interface for the Docker registry API

https://github.com/regclient/regclient

### Hadolint

Dockerfile linter

https://github.com/hadolint/hadolint
