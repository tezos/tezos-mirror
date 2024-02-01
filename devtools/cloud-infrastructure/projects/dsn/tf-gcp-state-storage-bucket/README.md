# GCP storage bucket to keep terraform state

Create a storage bucket in EU region to keep the terraform state

## Requirements

- GCP account access with good rights
- gcloud version ~> 440.0.0
- terraform version ~> 1.0

## Usage

For developers:

```sh
make format
make lint
```

For the deployment:

```sh
make init
make plan
make apply
```

## Variables

| Name       | Description                              | Type     | Default         | Required |
|------------|------------------------------------------|----------|-----------------|:--------:|
| project_id | The GCP project ID                       | `string` | "dsn-benchmark" |   yes    |
