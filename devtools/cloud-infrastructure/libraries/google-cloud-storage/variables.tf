variable "project" {
  description = "The GCP project ID"
  type        = string
}

variable "region" {
  description = "The GCP region"
  type        = string
}

variable "name" {
  description = "The name of the bucket"
  type        = string
}

variable "storage_class" {
  description = "Storage class"
  type        = string
  default     = "STANDARD"
}

variable "versioning_enabled" {
  description = "Enable versioning"
  type        = bool
  default     = true
}

variable "force_destroy" {
  description = "Force destroy"
  type        = bool
  default     = false
}
