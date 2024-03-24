data "terraform_remote_state" "network" {
  backend = "gcs"

  config = {
    bucket = "dsn-benchmark-tfstate"
    prefix = "terraform/state/dsn-vpc"
  }
}
