variable "project_id" {
  description = "The GCP project ID"
  type        = string
  default     = "dsn-benchmark"
}

variable "network_name" {
  description = "The GCP VPC name"
  type        = string
  default     = "dsn-vpc"
}

variable "regions" {
  description = "The Regions where to deploy the VPC"
  type        = list(string)
  default = [
    "europe-west1",
    "europe-north1",
    "us-west1",
    "us-east1",
    "us-central1",
    "asia-south1",
    "asia-east1",
  ]
}
