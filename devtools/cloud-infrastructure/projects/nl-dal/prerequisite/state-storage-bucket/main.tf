module "google_cloud_storage" {
  source = "../../../../libraries/google-cloud-storage"

  project_id = var.project_id
  region     = var.region
  name       = var.bucket
}
