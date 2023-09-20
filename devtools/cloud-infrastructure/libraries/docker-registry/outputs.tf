output "gcp_docker_registry" {
  value = local.gcp_docker_registry
}

output "url" {
  value       = format("%s/%s/%s", local.gcp_docker_registry, var.project_id, var.name)
  description = "URL of the registry that will contain the docker images"
}

