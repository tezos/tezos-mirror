# Inputs values that can be modified by the user
variable "region" {
  type        = string
  description = "The GCP region where the unmanaged instance group resides."
  default     = "europe-west1"
}

variable "zone" {
  type        = string
  description = "Set this variable to force a particular zone within the region"
  default     = "europe-west1-b"
}

variable "machine_type" {
  type        = string
  description = "Machine type of the VM instance."
}

variable "number_of_vms" {
  type        = number
  description = "The target number of running VM."
  default     = 1
}

variable "docker_registry_name" {
  type        = string
  description = "The docker registry"
  default     = "docker-registry"
}

variable "docker_image_name" {
  type        = string
  description = "The docker image name"
}

variable "base_port" {
  type        = number
  description = "First open port by the firewall"
  default     = 30000
}

variable "ports_per_vm" {
  type        = number
  description = "Number of port opened by the firewall"
  default     = 50
}

variable "project_id" {
  type        = string
  description = "Your project ID"
}

# Those values should not be modified
locals {
  artifact_registry = "europe-west1-docker.pkg.dev"
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

provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}

# A service account must be associated with a VM
resource "google_service_account" "default" {
  account_id   = "${terraform.workspace}-id"
  display_name = "${terraform.workspace} service Account"
}

# We want the service account to be able to fetch docker image from
# the docker registry
resource "google_project_iam_member" "artifact_registry_reader" {
  project = var.project_id
  role    = "roles/artifactregistry.reader"
  member  = "serviceAccount:${google_service_account.default.email}"
}

# This is an helper that enables to run the docker image once the
# machine is up
module "gce-container" {
  source  = "terraform-google-modules/container-vm/google"
  version = "~> 3.0"

  container = { image = "${local.artifact_registry}/${var.project_id}/${var.docker_registry_name}/${var.docker_image_name}" }
}

# When running a VM, it must be associated with a Virtual Private
# Cloud (VPC). A VPC is made of subnetworks (generally per region).
# For this experiments, we want the VM to be reached from the internet
# and also initiates connections.  To do so, we create a simple VPC
# with a single subnetwork.

resource "google_compute_network" "default" {
  name                    = "${terraform.workspace}-vpc"
  auto_create_subnetworks = false
}

resource "google_compute_subnetwork" "default" {
  name          = "${terraform.workspace}-subnet"
  network       = google_compute_network.default.self_link
  region        = var.region
  ip_cidr_range = "10.10.0.0/16"
}


# This enables each VM to get an external IP address (accessible from
# the internet).  Using [count] we create as many addresses as there
# are machines. We ensure to give them a unique name.
resource "google_compute_address" "default" {
  count        = var.number_of_vms
  name         = format("${terraform.workspace}-address-%02d", count.index)
  address_type = "EXTERNAL"

  # See https://cloud.google.com/network-tiers/docs/set-network-tier
  network_tier = "PREMIUM"

  region = var.region
}


# By default, the firewall blocks outbounds and inbounds connections.
# We define a set of rules that should be enough for our experiments
resource "google_compute_firewall" "default" {
  name    = "${terraform.workspace}-firewall"
  network = google_compute_network.default.self_link

  # Rule to enablle ssh
  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  # Define a plage of ports accessible from the internet for Octez
  # binaries
  allow {
    protocol = "tcp"
    ports    = ["${var.base_port}-${var.base_port + var.ports_per_vm}"]
  }

  # Enable access to the netdata dashboard if monitoring is enabled
  allow {
    protocol = "tcp"
    ports    = ["19999"]
  }

  # Anybody can contact the machine on the open ports.
  source_ranges = ["0.0.0.0/0"]
}

# This module creates a blueprint for the VM that will be spawned.
module "instance_template" {
  source  = "terraform-google-modules/vm/google//modules/instance_template"
  version = "~> 11.0.0"

  project_id = var.project_id

  # This source image is specialized to run docker images.
  source_image = "cos-stable"

  source_image_project = "cos-cloud"

  service_account = {
    # Google recommends custom service accounts that have cloud-platform scope and permissions granted via IAM Roles.
    email  = google_service_account.default.email
    scopes = ["cloud-platform"]
  }

  # This declares the docker image that must be run when the machine is up
  metadata = {
    gce-container-declaration = module.gce-container.metadata_value
  }

  # We register the subnetwork configuration
  subnetwork = google_compute_subnetwork.default.self_link

  machine_type = var.machine_type

  disk_type = "pd-ssd"

  region = var.region
}

# This module deploys a set of VM using the same blueprint
module "umig" {
  source  = "terraform-google-modules/vm/google//modules/umig"
  version = "~> 10.0"

  project_id        = var.project_id
  num_instances     = var.number_of_vms
  hostname          = terraform.workspace
  instance_template = module.instance_template.self_link
  zones             = [var.zone]
  region            = var.region
  subnetwork        = google_compute_subnetwork.default.self_link

  # This enables to set external IP address for each of the deployed
  # VM
  access_config = [
    for addr in google_compute_address.default[*] : [{
      nat_ip       = addr.address
      network_tier = addr.network_tier
    }]
  ]
}

# This variable outputs the list of IP addressed for each VM deployed
output "addresses" {
  description = "Instances addresses"
  value       = [for instance in google_compute_address.default[*] : instance.address]
}

output "zone" {
  description = "Zone where the VM is deployed"
  value       = var.zone
}

output "machine_type" {
  description = "Machine type"
  # All the instances have the same machine type
  value = module.umig.instances_details[0].machine_type
}
