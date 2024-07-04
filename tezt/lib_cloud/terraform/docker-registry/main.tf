# Inputs values that can be modified by the user
variable "region" {
  type        = string
  description = "The GCP region where the unmanaged instance group resides."
  default     = "europe-west1"
}

variable "zone" {
  type        = string
  description = "Set this variable to force a particular zone within the region"
  default     = "europe-west1-c"
}

variable "project_id" {
  type        = string
  description = "Your project ID"
}

locals {
  repository_id = "${terraform.workspace}-docker-registry"
  labels = {
    terraform = "true"
  }
}

terraform {
  required_version = "~> 1.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }

  backend "gcs" {
    # Generic bucket state that contains all the states for all the
    # workspaces
    bucket = "dal-tfstate"
  }
}

resource "google_artifact_registry_repository" "docker-registry" {
  repository_id = local.repository_id
  description   = "Docker images registry"
  format        = "DOCKER"
  project       = var.project_id
  location      = var.region
  labels        = local.labels

  lifecycle {
    prevent_destroy = true
  }
}

locals {
  hostname = format("%s-docker.pkg.dev", google_artifact_registry_repository.docker-registry.location)
  registry = format("%s/%s/%s", local.hostname, var.project_id, local.repository_id)
}

output "hostname" {
  value       = local.hostname
  description = "Hostname of the docker registry"
}

output "docker_registry" {
  value       = local.registry
  description = "URL of the GCP docker registry for this region"
}

output "zone" {
  value       = var.zone
  description = "Zone of the docker registry"
}
