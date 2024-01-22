source "googlecompute" "image" {
  project_id          = var.project_id
  zone                = var.zone
  image_name          = local.image_name
  image_family        = local.image_family
  machine_type        = local.machine_type
  source_image_family = local.source_image_family
  ssh_username        = "packer"
  disk_size           = 20
  disk_type           = "pd-ssd"
}

build {
  sources = ["sources.googlecompute.image"]

  provisioner "shell" {
    inline = [templatefile("./scripts/install_script.sh", {
      docker_registry_url  = var.docker_registry_url
      container_image_name = var.container_image_name
    })]
    execute_command = "{{.Vars}} bash '{{.Path}}'"
  }

  post-processor "shell-local" {
    inline = ["echo \"Setup in $(pwd)\""]
  }
}
