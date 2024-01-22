resource "random_string" "account-name" {
  length  = 8
  special = false
  upper   = false
}

resource "google_service_account" "this" {
  account_id   = "terraform-generated-${random_string.account-name.result}"
  display_name = "terraform-generated-${random_string.account-name.result}"
  description  = "Terraform generated service account"
}

resource "google_project_iam_member" "this" {
  project = var.project_id
  role    = "roles/artifactregistry.reader"
  member  = "serviceAccount:${google_service_account.this.email}"
}

resource "google_compute_network" "this" {
  name                     = "${var.project_id}-${var.hostname}-vpc"
  auto_create_subnetworks  = false
  enable_ula_internal_ipv6 = true
}

resource "google_compute_subnetwork" "this" {
  name             = "${var.project_id}-${var.hostname}-subnet"
  network          = google_compute_network.this.self_link
  region           = var.region
  ip_cidr_range    = local.subnet_cidr
  stack_type       = "IPV4_IPV6"
  ipv6_access_type = "EXTERNAL"
}

resource "google_compute_firewall" "this" {
  name    = "${var.project_id}-${var.hostname}-firewall"
  network = google_compute_network.this.self_link
  allow {
    protocol = "icmp"
  }
  allow {
    protocol = "tcp"
    ports    = ["22", "8732"]
  }
  allow {
    protocol = "tcp"
    ports    = ["3000"] # For monitoring purpose: Grafana
  }
  allow {
    protocol = "tcp"
    ports    = ["9090"] # For monitoring purpose: Prometheus
  }
  allow {
    protocol = "tcp"
    ports    = ["9100"] # For monitoring purpose: node-exporter
  }
  allow {
    protocol = "tcp"
    ports    = ["22", "30000"] # default ssh + container
  }
  allow {
    protocol = "tcp"
    ports    = ["30001-30999"] # Opened for the experiment. Will be routed to the docker container.
  }
  source_ranges = ["0.0.0.0/0"] # Allow incoming traffic from all IPv4 source addresses to the specified open ports.
  # source_tags   = []
  # target_tags   = []
}

resource "google_compute_address" "this" {
  count        = var.num_instances
  name         = format("address-%s-%02d", var.hostname, count.index + 1)
  address_type = "EXTERNAL"
  network_tier = local.network_tier
  region       = var.region
}

resource "google_compute_address" "internal" {
  count        = var.num_instances
  name         = format("internal-address-%s-%02d", var.hostname, count.index + 1)
  address_type = "INTERNAL"
  subnetwork   = local.subnetwork
  region       = var.region
}

module "vm_instance_template" {
  source  = "terraform-google-modules/vm/google//modules/instance_template"
  version = "~> 7.9.0"

  project_id      = var.project_id
  name_prefix     = "${var.hostname}-instance-template"
  machine_type    = local.machine_type
  labels          = local.labels
  tags            = local.tags
  metadata        = local.metadata
  service_account = local.service_account
  startup_script  = local.startup_script

  /* network */
  network            = local.network
  subnetwork         = local.subnetwork
  subnetwork_project = local.subnetwork_project
  can_ip_forward     = local.can_ip_forward

  /* image */
  source_image         = local.source_image
  source_image_family  = local.source_image_family
  source_image_project = local.source_image_project

  /* disks */
  disk_size_gb     = local.disk_size_gb
  disk_type        = local.disk_type
  auto_delete      = local.auto_delete
  additional_disks = local.additional_disks
}

module "vm_umig" {
  source             = "terraform-google-modules/vm/google//modules/umig"
  version            = "~> 7.9.0"
  project_id         = var.project_id
  network            = local.network
  subnetwork         = local.subnetwork
  subnetwork_project = local.subnetwork_project
  hostname           = var.hostname
  static_ips         = google_compute_address.internal[*].address
  num_instances      = var.num_instances
  instance_template  = module.vm_instance_template.self_link
  named_ports        = local.named_ports
  region             = var.region
  access_config      = local.access_config
}
