variable "project_id" {
  type        = string
  description = "The GCP project ID"
  default     = "nl-dal"
}

variable "zone" {
  type        = string
  description = "The GCP zone where the custom image resides."
  default     = "europe-west1-b"
}

variable "docker_registry_url" {
  type        = string
  description = "Google docker image registry (artifact registry) URL to authenticate with"
  default     = "europe-west1-docker.pkg.dev"
}

variable "container_image_name" {
  type        = string
  description = "The docker image"
  default = "nl-dal/docker-registry/debian-tezos"
}
