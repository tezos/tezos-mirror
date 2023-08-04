variable "project" {
  description = "The GCP project ID"
  type        = string
  default        = "nl-dal"
}

variable "region" {
  description = "The GCP region"
  type        = string
}

variable "name" {
  description = "The name of the bucket"
  type        = string
}
