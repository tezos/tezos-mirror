locals {
  labels = {
    terraform = "true"
  }

  gcp_docker_registry = format("%s-docker.pkg.dev", google_artifact_registry_repository.docker-registry.location)
}
