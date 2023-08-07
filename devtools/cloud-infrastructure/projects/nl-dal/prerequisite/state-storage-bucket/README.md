# State storage bucket

Terraform code for creating a Google Cloud Storage bucket for storing terraform state using the `/devtools/cloud-infrastructure/libraries/google-cloud-storage` library.
You only need to run this once at your initial setup.

## Variables

| Name       | Description             | Type     | Default        | Required |
| ---------- | ----------------------- | -------- | -------------- | :------: |
| project_id | The GCP project ID.     | `string` | "nl-dal"       |   yes    |
| region     | The GCP region.         | `string` | "europe-west1" |   yes    |
| bucket     | The name of the bucket. | `string` | None           |   yes    |

## How to Use

```shell
terraform init
terraform plan --var region="europe-west1" --var name="<your-name>"
terraform apply--var region="europe-west1" --var name="<your-name>"
```

For example:

```shell
terraform init
terraform plan --var region="europe-west1" --var name="lin"
terraform apply --var region="europe-west1" --var name="lin"
```

Remember what you used of `<your-name>` as it will be needed in subsequent steps.
