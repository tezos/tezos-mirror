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

variable "disk_type" {
  type        = string
  description = "Disk type of the VM instance."
  default     = "pd-ssd"
}

variable "disk_size_gb" {
  type        = number
  description = "Disk size of the VM instance in GB."
  default     = 200
}

variable "number_of_vms" {
  type        = number
  description = "The target number of running VM."
  default     = 1
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

variable "docker_image" {
  type        = string
  description = "Docker image used by the VM"
  default     = null
}

variable "max_run_duration" {
  type        = number
  description = "Default maximum duration of a VM (seconds)"
  default     = null
}

variable "os" {
  type        = string
  description = "Os description of the machine (default is cos). Possible value: cos, debian, archlinux"
  default     = "cos"

  validation {
    condition     = contains(["cos", "debian"], var.os)
    error_message = "The os value must be one of 'cos' or 'debian'"
  }
}

variable "prometheus_port" {
  type        = number
  description = "Port used by the Prometheus instance."
  default     = 9090
}


# Those values should not be modified
locals {
  artifact_registry = "europe-west1-docker.pkg.dev"

  os_image_map = {
    "cos"    = data.google_compute_image.cos.self_link
    "debian" = data.google_compute_image.debian.self_link
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

provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}

# A service account must be associated with a VM
resource "google_service_account" "default" {
  account_id   = "${terraform.workspace}-id"
  display_name = "${terraform.workspace}"
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

  container = { image = "${var.docker_image}"

    # This can be useful to execute some processes from the docker containers
    # that requires some capabalities on the VM
    securityContext = {
      privileged = true
    }

    # Volume settings is only necessary for the proxy VM
    volumeMounts = [
      {
        # Using the proxy mode, this is necessary if the docker image runs another docker image
        mountPath = "/var/run/docker.sock"
        name      = "docker-socket"
        readOnly  = false
      },
      {
        # Necessary to provide access from the image docker to the website
        mountPath = "/tmp/website"
        name      = "website"
        readOnly  = false
      },
      {
        # Same for Prometheus
        mountPath = "/tmp/prometheus"
        name      = "prometheus"
        readOnly  = false
      },
      {
        # Same for Grafana
        mountPath = "/tmp/grafana"
        name      = "grafana"
        readOnly  = false
      },
      {
        # Same for Alert manager
        mountPath = "/tmp/alert_manager"
        name      = "alert-manager"
        readOnly  = false
      },
      {
        # Same for OpenTelemetry
        mountPath = "/tmp/otel"
        name      = "otel"
        readOnly  = false
      }
    ]
  }

  volumes = [
    {
      name = "docker-socket"
      hostPath = {
        path = "/var/run/docker.sock"
      }
    },
    {
      name = "website"
      hostPath = {
        path = "/tmp/website"
      }
    },
    {
      name = "prometheus"
      hostPath = {
        path = "/tmp/prometheus"
      }
    },
    {
      name = "alert-manager"
      hostPath = {
        path = "/tmp/alert_manager"
      }
    },
    {
      name = "otel"
      hostPath = {
        path = "/tmp/otel"
      }
    },
    {
      name = "grafana"
      hostPath = {
        path = "/tmp/grafana"
      }
    }
  ]
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
  name         = format("${terraform.workspace}-%02d", count.index)
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

  # Enable access to Opentelemetry/Jaeger if enabled
  # 4317 used by Otel collector to receive observability data via gRPC
  # 55681 used by Otel collector to receive observability data via JSON
  # 14250 used by Jaeger to accept data over  gRPC.
  # 16686 Provides access to the Jaeger web UI for tracing visualization.
  allow {
    protocol = "tcp"
    ports    = ["4317", "14250", "16686","55681"]
  }

  # Rule to enable static page web access
  allow {
    protocol = "tcp"
    ports    = ["80", "443", "8080"]
  }

  # Rule to enable prometheus access
  allow {
    protocol = "tcp"
    ports    = ["${var.prometheus_port}"]
  }

  # Rule to enable grafana access
  allow {
    protocol = "tcp"
    ports    = ["3000"]
  }

  # Anybody can contact the machine on the open ports.
  source_ranges = ["0.0.0.0/0"]
}

data "google_compute_image" "cos" {
  family  = "cos-stable"
  project = "cos-cloud"
}

data "google_compute_image" "debian" {
  family  = "debian-12"
  project = "debian-cloud"
}

# This module creates a blueprint for the VM that will be spawned.
resource "google_compute_instance_template" "default" {

  # To support the `max-run-duration` argument
  provider = google-beta

  project = var.project_id

  name_prefix = "${terraform.workspace}-template"

  service_account {
    # Google recommends custom service accounts that have cloud-platform scope and permissions granted via IAM Roles.
    email  = google_service_account.default.email
    scopes = ["cloud-platform"]
  }

  # This declares the docker image that must be run when the machine is up
  metadata = {
    gce-container-declaration = module.gce-container.metadata_value
  }

  # We register the subnetwork configuration
  network_interface {
    subnetwork = google_compute_subnetwork.default.self_link
  }

  machine_type = var.machine_type

  disk {
    source_image = local.os_image_map[var.os]
    type         = "PERSISTENT"
    disk_type    = var.disk_type
    disk_size_gb = var.disk_size_gb
    boot         = true
    auto_delete  = true
  }

  region = var.region


  # Write a scheduling block only if variable "max_rune_duration" is set
  dynamic "scheduling" {
    for_each = var.max_run_duration == null ? [] : [var.max_run_duration]
    content {
      max_run_duration {
        seconds = var.max_run_duration
      }

      instance_termination_action = "DELETE"
    }
  }

  # We don't want to replace the instances if there is just a change in the 'max_run_duration'
  lifecycle {
    ignore_changes = [
      scheduling
    ]
  }
}

# This module deploys a set of VM using the same blueprint
module "umig" {
  source  = "terraform-google-modules/vm/google//modules/umig"
  version = "~> 10.0"

  project_id        = var.project_id
  num_instances     = var.number_of_vms
  hostname          = terraform.workspace
  instance_template = google_compute_instance_template.default.self_link
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
  value = length(module.umig.instances_details) > 0 ? module.umig.instances_details[0].machine_type : null
}
