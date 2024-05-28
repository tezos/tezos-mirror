# Docker

Generate a Docker image that can be deployed onto the cloud

## How to Use

1. Initialize Terraform:

```shell
terraform init
```

2. Export an environment variable called `TF_WORKSPACE` that will
   contain your workspace name. Ensure first this name is not used by
   anyone.

3. Generate an ssh-key with name `<workspace_name>-tf`.

	- If a passphrase was set, you might want to add the key to
      `ssh-agent`:

	  ```
	  eval "$(ssh-agent -s)"
	  ssh-add <ssh-private-file>
	  ```

4. Create your docker image called `<workspace_name>.Dockerfile` that
   corresponds to your needs. This docker image must provide the
   minimal setup to run Octez binaries (hence the need for zcash
   parameters) and its entrypoint should run an ssh server to accept
   Octogram commands.

5. Call `./push.sh`

The script `push` will builda docker image from the docker file
created at step 3 and push it onto the Docker registry on GCP.
