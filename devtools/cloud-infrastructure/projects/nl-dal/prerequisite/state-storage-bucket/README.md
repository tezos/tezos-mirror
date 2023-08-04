# State storage bucket

Terraform code for creating a Google Cloud Storage bucket for storing terraform state.

## How to Use

```shell
terraform init
terraform plan --var project="NL-dal" --var region="europe-west1" --var name="<your-name>"
terraform apply --var project="NL-dal" --var region="europe-west1" --var name="<your-name>"
```

For example:

```shell
terraform init
terraform plan --var project="NL-dal" --var region="europe-west1" --var name="lin"
terraform apply --var project="NL-dal" --var region="europe-west1" --var name="lin"
```
