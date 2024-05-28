terraform {
  backend "gcs" {
    bucket = "dsn-benchmark-tfstate"
    prefix = "terraform/state/dsn-vpc"
  }
}
