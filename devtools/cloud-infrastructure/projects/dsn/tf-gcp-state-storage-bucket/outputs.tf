output "project_id" {
  description = "The GCP project ID"
  value       = var.project_id
}

output "url" {
  description = "URL to the Google storage bucket deployed"
  value       = google_storage_bucket.default.url
}

output "self_link" {
  description = "Link to the Google storage bucket deployed"
  value       = google_storage_bucket.default.self_link
}
