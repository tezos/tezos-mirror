terraform {
  backend "gcs" {
    bucket = "dsn-benchmark-tfstate"
    prefix = "terraform/state/docker-registry"
  }
}
