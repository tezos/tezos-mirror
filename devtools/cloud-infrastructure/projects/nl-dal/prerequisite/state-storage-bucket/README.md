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

Create a file named `terraform.tfvars` in this directory that contains the following single line:

```
bucket=<bucket>
```

Recommendation for `<bucket>`:

- It contains a string that is unique to you, like your name.
- It ends with `-tfstate` to indicate that it is a Terraform state file.

For example:

```
bucket="lin-tfstate"
```

Now run:

```shell
terraform init
terraform plan
terraform apply
```
