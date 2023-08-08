# Google Cloud Storage

Creates a google cloud storage bucket.

## Variables

| Name               | Description                                           | Type     | Default    | Required |
| ------------------ | ----------------------------------------------------- | -------- | ---------- | :------: |
| project_id         | The GCP project ID.                                   | `string` | None       |   yes    |
| region             | The GCP region.                                       | `string` | None       |   yes    |
| name               | The name of the bucket.                               | `string` | None       |   yes    |
| storage_class      | The Storage Class of the new bucket.                  | `string` | "STANDARD" |   yes    |
| versioning_enabled | Enable versioning.                                    | `bool`   | true       |   yes    |
| force_destroy      | When deleting a bucket, delete all contained objects. | `bool`   | false      |   yes    |

## How to use

```
module "google_cloud_storage" {
  source     = "../../../../libraries/google-cloud-storage" # relative path to this module

  project_id = <your-project-id>
  region     = <region>
  name       = <name-of-bucket>
}

```
