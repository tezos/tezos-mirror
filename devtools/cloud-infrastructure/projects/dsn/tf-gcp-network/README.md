# Simple Project

VPC Configuration for running benchmarks.

#  Auto mode network

Auto mode network is set to true. A regional vpc

<!-- BEGINNING OF PRE-COMMIT-TERRAFORM DOCS HOOK -->
## Inputs

| Name          | Description                              | Type     | Default         | Required |
|---------------|------------------------------------------|----------|-----------------|:--------:|
| project\_id   | The project ID to host the network in    | `string` | "dsn-benchmark" | yes      |
| network\_name | The name of the VPC that will be created | `string` | "dsn-vpc"       | yes      |


## Outputs

| Name                | Description                        |
|---------------------|------------------------------------|
| auto                | The value of the auto mode setting |
| network\_name       | The name of the VPC being created  |
| network\_self\_link | The URI of the VPC being created   |
| project\_id         | VPC project id                     |
