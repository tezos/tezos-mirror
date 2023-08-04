variable "project_id" {
  type        = string
  description = "The GCP project ID"
  default     = "nl-dal"
}

variable "region" {
  type        = string
  description = "The GCP region where the unmanaged instance group resides."
  default     = "europe-west1"
}

variable "docker_registry_name" {
  type        = string
  description = "The registry name for the docker registry."
  default     = "docker-registry"
}