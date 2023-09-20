resource "google_artifact_registry_repository" "docker-registry" {
  repository_id = var.name
  description   = "Docker images registry"
  format        = "DOCKER"
  location      = var.region
  labels        = local.labels

  lifecycle {
    prevent_destroy = true
  }
}
