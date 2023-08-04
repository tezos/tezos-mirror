resource "google_storage_bucket" "default" {
  name          = "${var.name}-tfstate"
  project       = "${var.project}"
  force_destroy = false
  location      = "EU"
  storage_class = "STANDARD"
  versioning {
    enabled = true
  }
  labels = local.labels
}
