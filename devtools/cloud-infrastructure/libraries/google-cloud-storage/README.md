# Google Cloud Storage

Creates a google cloud storage bucket.

# How to use

```
module "google_cloud_storage" {
  source     = "../../../../libraries/google-cloud-storage" # relative path to this module

  project = <your-project-id>
  region     = <region>
  name       = <name-of-bucket>
}

```
