# GCP docker registry to create docker images

Create a docker image registry for the given project.

## Variables

| Name                 | Description                                | Type     | Default           | Required |
| -------------------- | ------------------------------------------ | -------- | ----------------- | :------: |
| project_id           | The GCP project ID.                        | `string` | "nl-dal"          |   yes    |
| region               | The GCP region.                            | `string` | "europe-west1"    |   yes    |
| docker_registry_name | The registry name for the docker registry. | `string` | "docker-registry" |   yes    |

# How to use

```
module "docker_registry" {
  source = "./modules/docker-registry" # relative path to this module

  project_id = "my_fancy_project"
  region = "europe"
  name = "docker-registry"
}
```
