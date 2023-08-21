locals {
  /* instance */
  machine_type = var.machine_type
  labels = {
    terraform = "true"
  }
  tags         = []
  metadata = {
    terraform           = "true"
    project_url         = var.project_url
    docker_registry_url = var.docker_registry_url
  }
  service_account = {
    email  = google_service_account.this.email
    scopes = ["cloud-platform"]
  }
  # Startup_script (max 256 kB)
  startup_script = templatefile(
    "./scripts/startup_script.sh", {
      docker_registry_url = var.docker_registry_url
    }
  )

  /* network */
  network            = ""
  subnetwork         = google_compute_subnetwork.this.self_link
  subnetwork_project = var.project_id
  subnet_cidr        = "10.10.0.0/16"
  network_tier       = "PREMIUM" # STANDARD or PREMIUM: https://cloud.google.com/network-tiers/docs/overview
  # For each address aka each instance, there is one of N access_config:
  access_config = [
    for addr in google_compute_address.this[*] : [{
      nat_ip       = addr.address
      network_tier = local.network_tier
    }]
  ]
  can_ip_forward = "false"
  named_ports = [
    {
      name = "ssh"
      port = "22"
    },
    {
      name = "ssh"
      port = "30000"
    }
  ]

  # image
  source_image         = "custom-cos-1691418108"
  source_image_family  = "custom-cos" # Build by Packer
  source_image_project = var.project_id

  # Filter to get the latest image built

  /* disks */
  disk_size_gb     = "100"
  disk_type        = "pd-ssd"
  auto_delete      = "true"
  additional_disks = []
}
