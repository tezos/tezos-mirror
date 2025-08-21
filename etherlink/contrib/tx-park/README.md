# DISCLAIMER

## Development and Testing Use Only

This Docker project is intended for **development and testing purposes
only**. It is not designed, tested, or certified for production or
mainnet deployment.

## Security Notice

**IMPORTANT**: This Docker environment is not configured for secure handling of sensitive information:

- Docker containers are not designed to handle secrets securely by
  default
- Secret keys, private keys, and other sensitive information stored in
  environment files, Docker volumes, or container filesystems may be
  vulnerable to extraction
- Container images may retain sensitive information in build layers
  even after deletion


HOW-TO
=======

### How to setup a project

Project configuration is done using Docker env files.

A project configuration template is available at `env/template.env`.

#### Quick start

One can generate a fresh configuration using the `./scripts/setup.sh`
script as follows:

```
./scripts/setup.sh env/template.env env/<output_file> <project_user> <project_name>
```

For instance,

```
./scripts/setup.sh env/template.env env/output.env phink testnet
```

This script purpose is to:
- generate the mandatory data to run the Docker `build` command,
- fetch tez from the faucet in order to originate the smart contracts and the smart rollup.

Feel free to custom the generated env file with the desired configuration.

One can inspect the current Docker config by running `docker-compose --env-file env/output.env config`.

#### Project configuration

Mandatory fields are project and accounts configurations.

##### Project configuration

Project configuration is done using the following fields:

- `PROJECT_USER`: your user name.
- `PROJECT_NAME`: name of your current project.

##### Accounts configuration

Accounts configuration is done using the following fields:

- `ADMIN_SK`: admin private key.
- `SEQUENCER_SK`: sequencer private key.
- `SMART_ROLLUP_BATCHER1_SK`: smart rollup batcher 1 private key.
- `SMART_ROLLUP_BATCHER2_SK`: smart rollup batcher 2 private key.
- `SMART_ROLLUP_BATCHER3_SK`: smart rollup batcher 3 private key.

If you did not rely on the `setup.sh` script, please ensure you have enough funds on these accounts:

- `ADMIN_SK` account needs enough funds to originate the contracts (if needed) and the smart rollup.
- `SMART_ROLLUP_BATCHER*_SK` account needs enough funds to publish messages to the smart rollup inbox.

##### Kernel configuration

The kernel configuration options can be specified in the `env` file.
Note that activation flags such as `--enable-dal` are enabled by
uncommenting the line `# ENABLE_DAL=` and do not require any value.

### How to build a project

Now that the project configuration is ready, run the following command to build the Docker base image:

```
docker compose --env-file env/output.env build
```

This command performs the following tasks:

1. Import the configured accounts.
2. If not specified by the `env` file, originate the contracts
   `exchanger.tz`, `chunked_delayed_transaction_bridge.tz` and `admin.tz`
   as found under the `tezos/etherlink/tezos_contracts/` directory in the
   current branch.
3. Generate the kernel installer configuration file `kernel_setup.yml`.
4. Build the kernel installer `installer.hex`.
5. Originate the corresponding smart rollup.

Note: Use the `--no-cache` argument if you want to start from a fresh state and originate new contracts.

```
docker compose --env-file env/example.env build --no-cache
```

### Inspect the image

To inspect the generated image, run the following:

```
$ docker images
REPOSITORY          TAG             IMAGE ID       CREATED          SIZE
tx-park/etherlink   phink.testnet   1423c3ec73a2   31 seconds ago   3.53GB
$ docker run --entrypoint /bin/bash -it tx-park/etherlink
.testnet
1417d032f7eb:/build$ ls
app               client            etherlink         installer.hex     kernel_setup.yml  root_hash
```

FAQ
===
