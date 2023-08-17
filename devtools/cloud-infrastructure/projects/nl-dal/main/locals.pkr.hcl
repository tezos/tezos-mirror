locals {
  container_image_name = "blockchain-speedtest-368211/default-repo/lthms/1mtps-demo-archlinux:latest"
  image_name           = "custom-cos-{{timestamp}}" # Add date
  image_family         = "custom-cos"
  machine_type         = "n2-standard-4"
  source_image_family  = "cos-101-lts"
}
