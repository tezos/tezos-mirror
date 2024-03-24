# https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/artifact_registry_repository

resource "google_artifact_registry_repository" "docker-registry" {
  repository_id = "dsn-registry"
  description   = "Docker registry for images to be deployed by dsn nodes"
  format        = "DOCKER"
  location      = var.region
  labels        = local.labels
}
