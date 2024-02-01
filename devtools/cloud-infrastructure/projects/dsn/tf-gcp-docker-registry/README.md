# GCP docker registry to keep docker images

Create a docker image registry in europe region (multiregion) to store docker images.

## Requirements

- GCP project with Artifact Registry API enabled
- GCP account access with Artifact Registry Administrator permissions
- gcloud version ~> 440.0.0
- terraform version ~> 1.0

## Usage

For developers:

```sh
make format
make lint
```

For the deployment:

```sh
make init
make plan
make apply
```

Instructions to push images to the registry once created:

```sh
gcloud auth configure-docker $region-docker.pkg.dev # set credential helpers for the registry in $HOME/.docker/config.json

docker pull hello-world # pull image from dockerhub. Or you can build your own image instead

docker tag hello-world $region-docker.pkg.dev/$project_id/dsn-registry/hello-world # the registry where to push the image must be specified in the image tag

docker push $region-docker.pkg.dev/$project_id/dsn-registry/hello-world
```

## Variables

| Name       | Description                                      | Type     | Default         | Required |
|------------|--------------------------------------------------|----------|-----------------|:--------:|
| project_id | The GCP project ID                               | `string` | "dsn-benchmark" |   yes    |
| region     | The GCP region where the docker registry resides | `string` | "europe"        |   yes    |
