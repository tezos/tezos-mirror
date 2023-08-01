# GCP docker registry to create docker images

Create a docker image registry for the given project.

# How to use

module "docker_registry" {
  source     = "./modules/docker-registry" # relative path to this module

  project_id = "my_fancy_project"
  region     = "europe"
  name       = "docker-registry"
}

