terraform {
  backend "gcs" {
    bucket = "lin-tfstate"
    prefix = "lin"
  }
}
