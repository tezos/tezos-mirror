# Tezt cloud

This library can used to write Tezt tests that can use machines deployed in the
cloud. At the moment those machines must be deployed onto GCP, and hence require
a GCP account to be used.

# Initialisation

## Creating a GCP account

Check with your manager how you can have a GCP account and to which
project you should be attached.

## Installing tools

Once it is done, you also need to have some tools installed locally:

- [`gcloud`](https://cloud.google.com/sdk/docs/install): to interact with the
  GCP API
- [`terraform`](https://developer.hashicorp.com/terraform/install): to interpret
  the script deploying VM onto GCP
- [`docker`](https://docs.docker.com/desktop/): to build and run images that
  will be used on VMs

Check out the different project pages to know how to install those tools on your
machine.

## Configuring tools

Run the following commands:

1. `gcloud init`: For initial setup. Note: the default project will be used by
   default. It can be overriden on the CLI.

2. You need a plugin for authentification. If you haven't installed the `gcloud`
   application with your package manager, then run: `gcloud components install
   gke-gcloud-auth-plugin`. Otherwise, install this plugin using your package
   manager. In any case, you can run the command and see whether it succeeds or
   not. If it does not, some hint will be given about what to do.

3. `gcloud auth application-default login`: Give the rights associated
   with your login.

4. `gcloud services enable artifactregistry.googleapis.com`: Allow using the
   artifact registries of Google (useful for pushing docker images).

5. `sudo usermod -aG docker $USER` where `$USER` is your user. This is to enable
   `docker` commands to be run without being sudo.

6. `gcloud config set project [YOUR_PROJECT_ID]`: For configuring the default
    project.

7. (optional) `gcloud auth configure-docker europe-west1-docker.pkg.dev`:
   Authorize the artifact registry used by this project.

Please set an environment variable `TF_WORKSPACE` which will be used to identify
the resources you deploy onto the cloud. The value should be unique for the
given project. We recommend using a similar username as the one you use on
Gitlab.

# Docker

The library assumes that you have a dockerfile image in the subdirectory of the
library `tezt/lib_cloud/dockerfiles` (it can be committed so that it gives
people an easy way to copy/paste the dockerfile for they own purpose). This
image will be the one that will be installed on each running VM. The library
takes care for you to push this image on a dedicated GCP registry by running the
appropriate command (see below).

## Write your own dockerfile

Depending on your local setup, we recommend to copy/paste an image that is
similar to your local setup. An example for a ubuntu/debian-like machine is
given by `saroupille.Dockerfile`. At the moment, several assumptions are made:

1. An ssh server is running and you have access to it.
2. The ssh key should be in the default ssh directory and named
   `$(TF_WORKSPACE)-tf`. The docker variable for specifying the ssh key should
   be named `SSH_PUBLIC_KEY`.
3. For convenience, we recommend the image to contain the minimum necessary to
   run Octez binaries that were compiled on your local setup. In particular, we
   use the docker variable `ZCASH_PARAMS_PATH` to provide the place where the
   zcash parameters are used.
4. Optionally, you can also put the binaries into the docker image (see
   `saroupille.Dockerfile`).

# One-time deployment

The library `tezt-cloud` needs to deploy resources that will be used throughout
your different experimentations and these resources should not be destroyed.

At the moment there are two resources of this kind which are detailed
below.

## Terraform state

Terraform maintains a state of the deployed resources that are *owned* by
`terraform`.  A standard way to store them is to use a GCP bucket.

Once your `TF_WORKSPACE` value is set and you have the appropriate
rights, you should be able to create the bucket using:

```bash
dune exec tezt/tests/main.exe -- terraform state deploy bucket -v
```

## Docker registry

A docker registry is needed so that, during initialisation of the VMs,
the docker image can be pulled from the cloud.

Once your `TF_WORKSPACE` value is set and you have the appropriate
rights, you should be able to create the registry using:

```bash
dune exec tezt/tests/main.exe -- docker registry deploy -v
```

## Deployment

To push your image, you should run:

```bash
dune exec tezt/tests/main.exe -- docker push -v
```

## Run your first scenario

Any scenario can be run on the local host for checking any deployment
error. Hence, when testing any scenario we recommend to provide the
`--localhost` parameter with a low number of machines to check whether your
scenario works as expected. The number of VMs can be specified on the
command-line via `--vms`. But it can also be computed by the scenario, so that
`--vms` is not mandatory.

We recommend to test it twice: using localhost or not:

```bash
dune exec tezt/tests/main.exe -- cloud health --localhost -v
```

and using VMs:

```bash
dune exec tezt/tests/main.exe -- cloud health -v
```

(Any scenario using this library should contain the tag `cloud`.)

If these two tests succeed, you are ready to create your own scenarios with
`tezt-cloud`.

## Destroying VMs

By default, VMs are **not** destroyed. This is to enable fast iteration while
debugging your scenarios where you can quickly fix and patch your scenarios
without having to redeploy the VMs. However, you must destroy your VMs at some
point, either by specifying `--destroy` when running your scenario or by
running:

```bash
dune exec tezt/tests/main.exe -- terraform destroy -v
```

(It is actually quite difficult to provide an automatic way to destroy resources
published onto the cloud. A possible solution in the future would be to deploy a
cron job locally.)

## Monitoring

Tezt-cloud offers you a way to monitor your VMs via
[netdata](https://www.netdata.cloud/). To enable monitoring, you only
have to provide the option `--monitoring` while running your scenario.

The netdata dashboard can be accessed on port `19999` on the remote machine. Do
note that if you use localhost, the monitoring will monitor your entire machine.
You might want to inspect container data to get the monitoring output of each of
the docker run locally.

Netdata enables to monitor the machines as well as docker containers.

At the moment, if the monitoring is activated, the netdata docker won't be
stopped until the VM is being teared down.

## Website

A website can be run to summarize information about the experiment. This can be
activated with the option `--website`. The website runs at
`http://localhost:8080`. The port can be changed with option `--website-port`.
The website can also be used to report ad-hoc metrics (see below).

## Prometheus

Prometheus can be used to export metrics from `netdata`. A prometheus container
can be spawned on the host machine via the option `--prometheus`. As a
side-effect, this will also export a snapshot at the end of the experiment. By
default, the snapshot will be stored in the `/tmp` directory. The directory can
be modified with the `--prometheus-snapshot-directory` option. The name of the
snapshot can be set via `--prometheus-snapshot`.

After being exported, a snapshot can be imported by running:

```bash
dune exec tezt/tests/main.exe -- prometheus import --prometheus-snapshot <filename>
```

## Exporting ad-hoc metrics

While writing an experiment, it may be interesting to export metrics. This is
doable if the experiment is run with options `--website` and `--prometheus`. By
doing so, metrics can be exported via the function `Cloud.push_metric`. It takes
as parameters the name of the metric as well as optional labels and the value to
export. Metrics are reported on a special page `/metrics.txt` on the website and
will be scrapped by the Prometheus instance. Such metrics will be automatically
snapshotted at the end of the experiments.

## Exporting daemons metrics

Metrics exported by daemons such as `octez-node` or `octez-dal-node` can also be
exported to the Prometheus instance using the function
`Cloud.add_prometheus_source`.

## Recommendation when writing experiments

0. Never forget to destroy your VMs.

1. You can spawn mutiple daemons on the same VM.

2. Any time you create a new daemon that runs on a remote machine, you **must** specify:
   1. The path where to find the daemon (if you used `Agent.copy`, the
      path is given as the returned value).
   2. All the ports that will be used. To get the next available port,
      use `Agent.next_port`.
   3. The runner that will be used (this can be given to the agent).

3. Whenever it is possible, write your test without depending specifically on a
   fixed number of machines. Whenver you want to increase or decrease the number
   of machines, you can just run your test again and it will just work.

4. Use `--log-file` option from Tezt so that you can inspect errors a posteriori.

5. We recommend to use `-v` while experimenting. This may be an issue if tests
   are run with a large number of machines (`> 100`).

6. `Tezt-cloud` is made so that if an experiment succeeds with `--localhost`,
   it should also succeeds without this option. However, this is not always the
   case. If it happens, here are some hints:

   - There is an network configuration issue. This can happen when daemons
     (`octez-node`, ...) are not initialised correctly with runners. Check the
     parameters given to the daemons. All points accessible from the internet
     should probably listen on all the P2P interface. For example the
     `octez-node` should be run with `--rpc-addr 0.0.0.0:30001`.

   - The clean-up of tezt or tezt-cloud failed. A manual clean-up can be done via

   ```bash
   dune exec tezt/tests/main.exe -- clean up -v
   ```

   - There is an internal issue with `tezt-cloud`. Please feel free to report
     it.

7. When debugging a scenario, using `--keep-alive` can be useful. In that case,
   when the experiment ends (successfully or with an error/exception), you will
   be prompted to end the test by pressing `<enter>`. Before doing that you can
   inspect the current state of the machine. You can connect on the docker image
   via ssh given the IP address of the machine. Or you can directly connect onto
   the VM itself via:

   ```bash
   gcloud compute ssh --ssh-key-file <ssh-key> <workspace>-XYZ --zone <zone>
   ```

   where:
   - `ssh-key` is the one you generated above
   - `workspace` is the content of the `$TF_WORKSPACE` variable
   - `XYZ` is the counter of the VM
   - `zone` is the zone of the VM (default is `europe-west1-b`)

8. It may be possible that the scenario does not behave as expected, especially
   when the scenario is run with a high number of VMs and daemons. We recommend
   several things:

   - If possible, change the default level for events read by the daemon to
     `Notice`. By default, daemons read events through a pipe and send it to the
     host machine (the one on which the scenario is run). Then the host machine
     will parse all those events. This can be relatively costly.

   - You may run `netdata` on your local machine to check whether you are
     reaching some limitations occasionnaly during your test. For example, if at
     some specific point in time, a lot of data are sent or received. This can
     explain why the scenario behaves unexpectedly. We recommend to write your
     scenario so that only the minimum amount of information is shared between
     the daemons.

9. When deploying a large number of VMs, ssh may open too many file descriptors,
   which can either reach some OS limits or cause broken pipe errors. To avoid
   this, ssh multiplexing can help. To enable multiplexing, add something along
   these lines in the ssh config file:

   ```
   Host *
    ControlMaster auto
    ControlPath ~/.ssh/master-%r@%h:%p
    ControlPersist 120
   ```

## Limitations and future work

At the moment, there are two main limitations to `tezt-cloud`:

- The host machine receives and parses all the events fromm all the daemons. It
  could be interesting instead to have a `proxy` mode so that intermediate
  processes are hosted on VMs themselves to catch some of those events. This
  could be also used as a basis to implement `detach/attach` capabilities to
  Tezt cloud so that you don't have to be connected all the time on your machine
  during the experiment.

- All the VMs must use the same kind of machine. For performance-related
  experiments, it may be interesting to have different kinds of machines for
  different kinds of VMs. While this could be supported in future versions of
  `tezt-cloud`, the only way to by-pass this limitation is to use multiple
  `workspace`s (you need to implement the logic to handle multiple workspaces
  yourself).
