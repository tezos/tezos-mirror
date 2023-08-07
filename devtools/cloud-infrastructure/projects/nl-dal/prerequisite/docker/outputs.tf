output "docker_registry_url" {
  value       = module.docker_registry.url
  description = "URL of the Docker registry"
}

output "gcp_docker_registry" {
  value       = module.docker_registry.gcp_docker_registry
  description = "URL of the GCP docker registry for this region"
}

