resource "google_compute_network" "vpc_network" {
  project = var.project_id # Replace this with your project ID in quotes
  name    = var.network_name
  # This prevents BGP routes to be propagated from one subnetwork to another.
  # As a result, instances in two different regions will not connect using the internal
  # Google network.
  routing_mode            = "REGIONAL"
  auto_create_subnetworks = false
}

resource "google_compute_subnetwork" "subnetwork" {
  count         = length(var.regions)
  name          = "${var.network_name}-subnetwork-${var.regions[count.index]}"
  ip_cidr_range = "10.10.${count.index + 1}.0/24"
  region        = var.regions[count.index]
  network       = google_compute_network.vpc_network.id
}

resource "google_compute_firewall" "ssh" {
  name    = "${var.network_name}-allow-ssh-inbound"
  network = google_compute_network.vpc_network.name

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  direction     = "INGRESS"
  source_ranges = ["0.0.0.0/0"]
}

resource "google_compute_firewall" "icmp" {
  name    = "${var.network_name}-allow-icmp"
  network = google_compute_network.vpc_network.name

  allow {
    protocol = "icmp"
  }

  direction     = "INGRESS"
  source_ranges = ["0.0.0.0/0"]
}

resource "google_compute_firewall" "dsn_inbound" {
  name    = "${var.network_name}-allow-connections-inbound"
  network = google_compute_network.vpc_network.name

  allow {
    protocol = "tcp"
    ports    = ["1024-65535"]
  }

  direction     = "INGRESS"
  source_ranges = ["0.0.0.0/0"]
}

resource "google_compute_firewall" "dsn_outbound" {
  name    = "${var.network_name}-allow-connections-outbound"
  network = google_compute_network.vpc_network.name

  allow {
    protocol = "tcp"
    ports    = ["1024-65535"]
  }

  direction = "EGRESS"
}
