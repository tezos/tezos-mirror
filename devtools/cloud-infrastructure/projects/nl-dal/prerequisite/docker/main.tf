module "docker_registry" {
  source     = "../../../libraries/docker-registry"

  project_id = var.project_id
  region     = var.region
  name       = var.docker_registry_name
}
