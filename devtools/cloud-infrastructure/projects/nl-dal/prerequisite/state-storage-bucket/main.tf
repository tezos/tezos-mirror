module "google_cloud_storage" {
  source     = "../../../../libraries/google-cloud-storage"

  project = var.project
  region     = var.region
  name       = "${var.name}-tfstate"
}
