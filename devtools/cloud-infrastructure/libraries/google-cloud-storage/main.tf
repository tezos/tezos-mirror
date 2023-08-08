resource "google_storage_bucket" "default" {
  name          = var.name
  project       = var.project_id
  force_destroy = var.force_destroy
  location      = var.region
  storage_class = var.storage_class
  versioning {
    enabled = var.versioning_enabled
  }
  labels = local.labels
}
