variable "project_id" {
  description = "The GCP project ID"
  type        = string
  default     = "nl-dal"
}

variable "region" {
  description = "The GCP region"
  type        = string
  default     = "europe-west1"
}

variable "bucket" {
  description = "The name of the bucket"
  type        = string
}
