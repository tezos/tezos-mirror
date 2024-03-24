variable "project_id" {
  description = "The GCP project ID"
  type        = string
  default     = "dsn-benchmark"
}

variable "machine_type" {
  description = "The machine type to deploy"
  type        = string
  default     = "n2-standard-4"
}

variable "testbed_id" {
  description = "The id of the testbed that will be used for benchmarking"
  type        = string
  default     = "benchmark"
}

variable "docker_image" {
  description = "The image to be pulled by dsn nodes at startup"
  type        = string
  default     = "mysticeti"
}

variable "docker_registry" {
  description = "The registry from where the image will be pulled"
  type        = string
  default     = "europe-docker.pkg.dev/dsn-benchmark/dsn-registry"
}

variable "ssh_user" {
  description = "The username of the account that can be used to authenticate via ssh"
  type        = string
  default     = "tmpuser"
}

variable "ssh_pub_key_file" {
  description = "The file containing the ssh public key that can be used to authenticate to instances as `dockeruser`"
  type        = string
  default     = "~/.ssh/google_compute_engine.pub"
}
