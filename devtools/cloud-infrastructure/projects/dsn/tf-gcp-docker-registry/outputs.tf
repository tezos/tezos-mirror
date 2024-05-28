output "project_id" {
  description = "The GCP project ID"
  value       = var.project_id
}

output "id" {
  description = "ID of the Google artifact registry deployed"
  value       = google_artifact_registry_repository.docker-registry.id
}

output "name" {
  description = "Name of the Google artifact registry deployed"
  value       = google_artifact_registry_repository.docker-registry.name
}
