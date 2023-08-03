variable "project_id" {
  type        = string
  description = "The GCP project ID."
}

variable "region" {
  type        = string
  description = "The GCP region where the unmanaged instance group resides."
}

variable "name" {
  type        = string
  description = "The name of the docker registry."
}
