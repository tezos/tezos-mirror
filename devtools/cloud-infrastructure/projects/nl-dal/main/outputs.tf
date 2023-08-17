output "instance_template_self_link" {
  description = "Self-link of instance template"
  value       = module.vm_instance_template.self_link
}

output "tags" {
  description = "Tags that will be associated with instance(s)"
  value       = module.vm_instance_template.tags
}

output "ssh_addresses" {
  description = "Instances addresses"
  value       = google_compute_address.this[*].address
}

output "internal_addresses" {
  description = "Instances addresses"
  value       = google_compute_address.internal[*].address
}
