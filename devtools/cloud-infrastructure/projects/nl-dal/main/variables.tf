variable "project_url" {
  type        = string
  description = "GitLab - The HTTPS address of the project"
  default     = "https://gitlab.com/tezos/tezos/"
}

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

variable "zone" {
  type        = string
  description = "Set this variable to force a particular zone within the region"
  default     = "europe-west1-c"
}

variable "hostname" {
  type        = string
  description = "Hostname prefix for instances. Different hostnames are used to facilitate the concurrent execution of independent experiments. This ensures that resources remain distinct and non-conflicting for each experiment."
}

variable "machine_type" {
  type        = string
  description = "Machine type of the VM instance."
  default     = "c2-standard-4"
}

variable "num_instances" {
  type        = string
  description = "The target number of running instances for this managed or unmanaged instance group. This value should always be explicitly set unless this resource is attached to an autoscaler, in which case it should never be set."
  default     = 1
}

variable "docker_registry_url" {
  type        = string
  description = "Google docker image registry (artifact registry) URL to authenticate with"
  default     = "europe-west1-docker.pkg.dev"
}
