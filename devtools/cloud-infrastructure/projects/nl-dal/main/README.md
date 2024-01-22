# Deploy to Google Cloud VM

Terraform for deploying docker images to the Google Cloud VM.

It will run `num_instances` (see the "Variables" section below) number of VM instances with machine type `machine_type` under the host name `hostname`. Each VM instance will run the docker image created in `../prerequisite/docker/`.

## Variables

| Name          | Description                                                                                                                              | Type     | Default          | Required |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------------------- | -------- | ---------------- | :------: |
| project_id    | The GCP project ID.                                                                                                                      | `string` | "nl-dal"         |   yes    |
| region        | The GCP region.                                                                                                                          | `string` | "europe-west1"   |   yes    |
| zone          | The zone within the region.                                                                                                              | `string` | "europe-west1-c" |   yes    |
| machine_type  | Machine type of the VM instance.                                                                                                         | `string` | "c2-standard-4"  |   yes    |
| num_instances | The number of VM instances to run.                                                                                                       | `string` | "1"              |   yes    |
| hostname      | Hostname prefix for instances. Using different hostnames ensures that resources remain distinct and non-conflicting for each experiment. | `string` | None             |   yes    |

## Prerequisite

### Add your SSH key to the docker image

Follow `../prerequisite/docker/README.md`.

### Create a Google Cloud Storage bucket for storing your terraform state

Follow `../prerequisite/state-storage-bucket/README.md`.

## How to Deploy

First, initialize terraform using the bucket you created in the previous section. This can be done by referencing the `terraform.tfvars` file you created in `../prerequisite/state-storage-bucket/README.md`.

```shell
terraform init -backend-config="../prerequisite/state-storage-bucket/terraform.tfvars"
```

Run terraform plan and check the execution plan. You can use any string as `<hostname>`. However, to avoid conflicts with others, it's advisable to use a unique identifier such as your `<name>`.

```shell
terraform plan --var hostname="<hostname>"
```

If the plan looks good, apply the change.

```shell
terraform apply --var hostname="<hostname>"
```

You should see an output that looks like this:

```shell
ssh_addresses = [
  "<ssh-address>",
]
```

## How to Interact with the VM

Since:

- The VM routes `30000` to the docker container.
- The docker container opens `30000` for SSH.

You should be able to interact with the deployed docker image via the address `<ssh-address>` and port `30000` using SSH and/or SCP.

You can SSH into the docker image by running:

```shell
ssh  root@<ssh-address> -p 30000
```

You can push files by running:

```shell
scp -P 30000 <path-to-file> root@<ssh-address>:<file-name>
```

For example, if you want to push your local `octez-node` you can run:

```shell
scp -P 30000 <path-to-your-octez-node> root@<ssh-address>:octez-node
```

## How to Destroy

Do not forget to destroy the deployed VM once you are finished with your testing.

```shell
terraform destroy --var hostname="<hostname>"
```
