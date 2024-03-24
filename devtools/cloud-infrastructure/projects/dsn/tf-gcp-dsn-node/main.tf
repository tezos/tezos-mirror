resource "google_service_account" "dsn_nodes_service_account" {
  account_id   = "${data.terraform_remote_state.network.outputs.network_name}-service-account"
  display_name = "Service Account used by DSN nodes"
}

resource "google_project_iam_member" "artifact_registry_reader" {
  project = var.project_id
  role    = "roles/artifactregistry.reader"
  member  = "serviceAccount:${google_service_account.dsn_nodes_service_account.email}"
}

resource "google_compute_instance" "dsn_node" {
  depends_on   = [google_project_iam_member.artifact_registry_reader]
  count        = length(data.terraform_remote_state.network.outputs.subnets)
  name         = "${data.terraform_remote_state.network.outputs.network_name}-${var.testbed_id}-${data.terraform_remote_state.network.outputs.subnets[count.index].region}"
  machine_type = var.machine_type
  # This is hacky, as it relies on all regions in GCP having a "b" zone.
  zone = "${data.terraform_remote_state.network.outputs.subnets[count.index].region}-b"

  tags = ["dsn-node"]

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-11"
      labels = {
        testbed_id = "${var.testbed_id}"
      }
    }
  }

  // Local SSD disk
  scratch_disk {
    interface = "NVME"
  }

  network_interface {
    network    = data.terraform_remote_state.network.outputs.network_name
    subnetwork = data.terraform_remote_state.network.outputs.subnets[count.index].name

    access_config {
    }
  }

  metadata = {
    testbed_id = "${var.testbed_id}"
    ssh-keys   = "${var.ssh_user}:${file(pathexpand(var.ssh_pub_key_file))}"
  }

  metadata_startup_script = templatefile(
    "./scripts/startup.sh", {
      docker_registry = var.docker_registry
      docker_image    = var.docker_image
      ssh_user        = var.ssh_user
    }
  )

  service_account {
    # Google recommends custom service accounts that have cloud-platform scope and permissions granted via IAM Roles.
    email  = google_service_account.dsn_nodes_service_account.email
    scopes = ["cloud-platform"]
  }
}
