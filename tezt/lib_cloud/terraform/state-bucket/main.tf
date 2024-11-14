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
  bucket_name = "${terraform.workspace}-tfstate"
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
}

resource "google_storage_bucket" "default" {
  name          = local.bucket_name
  project       = var.project_id
  force_destroy = true
  location      = var.region
  storage_class = "STANDARD"
  versioning {
    enabled = true
  }
  labels = local.labels
}

output "name" {
  value       = local.bucket_name
  description = "Name of the bucket containing the terraform state"
}
