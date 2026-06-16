# Tezt cloud

This library can be used to write Tezt tests that can use VMs deployed in the
cloud. At the moment those VMs must be deployed onto GCP, and hence require a
GCP account to be used. Scenarios written with tezt cloud can also be run in
localhost without using GCP.

# Initialisation

There are two important configuration steps before using the library:

- Having a GCP account
- Have your own dockerfile (or choose an existent one that suits you)

Do note that the GCP account is not necessary if you use the library
in `localhost` mode (and so you don't need `gcloud` and `terraform`).

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
- [`docker-buildx`](https://github.com/docker/buildx): to install extra
  dependencies to use `docker`

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
   artifact registries of Google (necessary for pushing docker images).

5. `sudo usermod -aG docker $USER` where `$USER` is your user. This is to enable
   `docker` commands to be run without being sudo. This is only required for `Linux`
   distributions, as on `macOS` `docker` runs inside a virtualized environment and
   the user automatically has the necessary privileges.

6. `gcloud config set project [YOUR_PROJECT_ID]`: For configuring the default
    project.

7. (optional) `gcloud auth configure-docker
   europe-west1-docker.pkg.dev`: Authorize the artifact registry used
   by this project. This can be useful if you plan to push docker
   images without tezt cloud.

# Tezt cloud variable

Any time you use the Tezt cloud library, a namespace for your
experiment must be provided. Most of the time, if you only run one
experiment at a time, you can use your gitlab username for example.
Such a namespace can be provided via the environment variable
`TEZT_CLOUD` or via the CLI option `--tezt-cloud`. It may be
interesting to set the `TEZT_CLOUD` environment variable with your
gitlab username since this is the one you will use the most.

This variable is also used to determine the dockerfile to be used for
your experiment. In case you want to use a different dockerfile, you
can use the CLI option `--dockerfile-alias`.

You can use several namespaces by switching between several values of the `TEZT_CLOUD`
variable but avoid values which are prefixes of each other. For instance use
`<username>`, `test-<username>`, and `prod-<username>` instead of `<username>`,
`<username>-test`, and `<username>-prod`; otherwise destroying the machines
(with the `CLOUD terraform destroy` command) of the `<username>` namespace
would also destroy the machines of the `<username>-test` and `<username>-prod` namespaces.

# Docker

The library relies mainly on docker. Each VM that will be deployed
will run a docker image. This docker image must follow some conditions
(mainly running an SSH server), but apart from that you are free to
use your own dockerfile.

To help you write such an image, a default one based on debian is
provided in `tezt/lib_cloud/dockerfiles/debian.Dockerfile`.

The library takes care for you to push this image on a dedicated GCP
registry. A docker image such as the one generated from
`debian.Dockerfile` may contain some binaries. Tezt cloud always
tries to rebuild the docker image to be sure it uses the last version,
however be sure to compile the binaries beforehand.

Finally, do note that with Tezt cloud you can specify for each VM
individually the docker image to be used. The one provided by the
dockerfile as discussed above will be the one by default. It is also
possible to use docker images published by the Octez CI. At the
moment, the docker image from the latest release of Octez is
supported.

There is now also a docker file that can be used for macOS distributions,
which is provided in `tezt/lib_cloud/dockerfiles/macosx.Dockerfile`. Please
note that this image is highly experimental, and several improvements are
to be made to it, but it should work for the time being.

## Write your own dockerfile

Depending on your local setup, we recommend copying and pasting an
image that closely matches your environment. An example for an
Ubuntu/Debian-like machine is given by `debian.Dockerfile`. The
following assumptions apply:

1. An ssh server is running on port `30000`

2. For convenience, the image should include the minimum necessary
components to run Octez binaries compiled on your local setup. In
particular, the Docker variables ZCASH_PARAMS_PATH and
DAL_TRUSTED_SETUP_PATH should specify the location where the Zcash
parameters and DAL trusted setup are used.

3. Optionally, you can also put the binaries into the docker image (see
   `debian.Dockerfile`).

To ensure you can connect to the ssh server, the library generates (if
it does not exist) and ssh key based on the `tezt-cloud` variable.

**Note**: In addition to the Dockerfile, you need to create a
  `.dockerignore` file in the same directory. This ensures the Docker
  context doesn't exclude some files ignored by the main Octez
  Dockerfiles but still needed for tezt-cloud.

### OS specific dockerfiles

If you can't find an existing image that fits your distribution, feel
free to create one. If you do so, remember to update the project's
.gitignore file accordingly to ensure any changes to these files are
committed.

# One-time deployment

The library `tezt-cloud` needs to deploy resources that will be used
and reused throughout your different experimentations. Tezt cloud
should deploy them for you automatically. Hence the commands in this
section are optional.

## Docker registry

Tezt cloud does a best effort to create a docker registry if it does
not exists already. The name of the docker registry depends on the
`TEZT_CLOUD` value.

```bash
dune exec tezt/tests/cloud/main.exe -- CLOUD docker registry deploy -v
```

## Deployment

If you want to push your image manually on the registry you can do:

```bash
dune exec tezt/tests/cloud/main.exe -- CLOUD docker push -v
```

## Run your first scenario

Any Tezt cloud scenario can be run on the host machine for checking
any deployment error, this is the `localhost` mode. Hence, when
testing any scenario we recommend to provide the `--localhost`
parameter with a low number of machines to check whether your scenario
works as expected.

When you are ready to deploy resources, you can remove the
`--localhost` option, or alternatively use the `--cloud` option. When
deploying machines on the cloud, the number of VMs can be specified on
the command-line via `--vms`. But it can also be computed by the
scenario, so that `--vms` is not mandatory.

We recommend to test it twice: using localhost or not:

```bash
dune exec tezt/tests/cloud/main.exe -- BASIC health --localhost -v
```

and using VMs:

```bash
dune exec tezt/tests/cloud/main.exe -- BASIC health -v
```

(Any scenario using this library should contain the tag `cloud`.)

If these two tests succeed, you are ready to create your own scenarios with
`tezt-cloud` or use existent ones.

## Destroying VMs

By default, VMs are **not** destroyed. This is to enable fast iteration while
debugging your scenarios where you can quickly fix and patch your scenarios
without having to redeploy the VMs. However, you must destroy your VMs at some
point, either by specifying `--destroy` when running your scenario or by
running:

```bash
dune exec tezt/tests/cloud/main.exe -- CLOUD terraform destroy -v
```

However, all your VMs may come with a time to live parameter. For the
`--cloud` mode, it is set to 2 hours. For the proxy mode (see below),
it is not set by default.

Also, note that the role of the `TEZT_CLOUD` variable (or the `--tezt-cloud`
option) is crucial here, as the VMs being destroyed are the ones whose name have
a prefix that is the value of `TEZT_CLOUD`.

## Monitoring

Tezt-cloud offers you a way to monitor your VMs via
[netdata](https://www.netdata.cloud/). To enable monitoring, you only
have to provide the option `--monitoring` while running your scenario.
In which case, all the VMs will be monitored.

The netdata dashboard can be accessed on port `19999` on the remote machine. Do
note that if you use localhost, the monitoring will monitor your entire machine.
You might want to inspect container data to get the monitoring output of each of
the docker run locally.

Netdata enables to monitor the machines as well as docker containers.

At the moment, if the monitoring is activated, the netdata docker won't be
stopped until the VM is being teared down.

## Website

A website can be run to summarize information about the experiment.
This can be activated with the option `--website`. The website runs at
`http://localhost:8080`. The port can be changed with option
`--website-port`. The website can also be used to report ad-hoc
metrics (see in the next section).

## Prometheus

Prometheus can be used to export metrics from `netdata` or export
Octez metrics. A prometheus container can be spawned on the host
machine via the option `--prometheus`. As a side-effect, this will
also export a snapshot at the end of the experiment. By default, the
snapshot will be stored in the `/tmp` directory. The name of the
snapshot can be specified via `--prometheus-snapshot-filename`.

After being exported, a snapshot can be imported by running:

```bash
dune exec tezt/tests/cloud/main.exe -- CLOUD prometheus import --prometheus-snapshot-filename <filename>
```

### Exporting ad-hoc metrics

While writing an experiment, it may be interesting to export metrics
to the scenario run (for example to ensure the scenario behaves as
expected). This feature requires the experiment to be run with
`--website` and `--prometheus` (which is the case by default). By
doing so, metrics can be exported via the function
`Cloud.push_metric`. It takes as parameters the name of the metric as
well as optional labels and the value to export. Metrics are provided
on the website at the following URI `/metrics.txt` and will be
scrapped by the Prometheus instance. Those metrics are also exported
at the end of the experiment in the Prometheus snapshot.

### Exporting daemons metrics

When writing a new scenario, you can instruct how to export daemons
(e.g. `octez-node`) metrics to Prometheus by calling the function
`Cloud.add_prometheus_source`.

## Grafana

Grafana can be used to visualize prometheus metrics (activated by
default). This can be accessed on `http://localhost:3000`. Tezt cloud
comes with a set of dashboards so that it is ready to use. Feel free
to update/add new dashboards in
`tezt/lib_cloud/grafana/dashboards`.

## Proxy mode

**EXPERIMENTAL**

The `--proxy` option allow you to run your experiment onto a VM
instead of your local machine. This may have several benefits, but in
particular, it can be used to set up long experiments (that can last
for weeks).

When running a test in proxy mode, you can detach it after the
deployement has been made and the scenario start to kick in. In that
case, you can detach from your experiment by pressing `ctrl+d`. By
doing so, you can do whatever you want, but the experiment is still
executed on a remote VM.

To reattach the test, simply rerun the command. Normally, it should
reattach back the running experiment.

Killing an experiment manually requires some care. You must do it in
two steps:

1. Press Ctrl+C. This will trigger the shutdown of the
experiment.

2. Once you see the scenario has ended properly, you can press Ctrl+D.

If you make a mistake during the shutdown, the scenario may not end
properly. In that case, you can clean up the VMs by running:

```
dune exec tezt/tests/cloud/main.exe -- CLOUD clean up -v
```

UX-wise, the proxy mode has still some awkwards behaviour regarding
how log events are printed on `Ctrl+D` or `Ctrl+C`. Moreover, the
handling of `sigint` (Ctrl+C) may be a bit fragile.

We recommend to use first the default mode before using the proxy
mode to ensure the scenario behaves as expected.

With the proxy mode, we recommend to use the website if you want to
follow your experiment. The website should be provided in the logs
once you detach from the experiment.

Otherwise, you can connect to it by looking at the external IP address
of the proxy machine on GCP and use the port `8080`.

### DNS

The website deployed with the proxy mode can be associated with a
domain name if the `--dns` argument is provided (true by default).

The domain associated with the website will be:
`http://<tezt_cloud>.<gcp-project>.<domain>`.

To make it work, any project must register a subdomain zone
beforehand. This can be done via:

```
dune exec tezt/tests/cloud/main.exe -- CLOUD create dns zone -v --dns-domain <domain>
```

The name servers associated with this domain must be added manually to
the domain regstriy associated with `<domain>`.

You can check this work as expected by running:

```
dig ns <gcp-project>.<domain>
```

You should see the name servers you just added.

## Recommendation when writing experiments

0. Even though there is a time to live for the VM, please don't forget
   to destroy your VMs.

1. You can spawn mutiple daemons on the same VM.

2. Any time you create a new daemon that runs on a remote machine, you **must** specify:
   1. The path where to find the daemon (if you used `Agent.copy`, the
      path is given as the returned value).
   2. All the ports that will be used. To get the next available port,
      use `Agent.next_port`.
   3. The runner that will be used (this can be given to the agent).

3. Whenever it is possible, write your test without depending
   specifically on a fixed number of machines. Whenever you want to
   increase or decrease the number of machines, you can just run your
   test again and it will just work. This can be done by using a
   dispenser (via the OCaml module `Seq` for example).

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
   dune exec tezt/tests/cloud/main.exe -- CLOUD clean up -v
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
   - `workspace` is the content of the `$TEZT_CLOUD` variable
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

# Deploying on non-gcp machines via ssh.

You need ssh access to the machine, and the ssh-key to be loaded in your
ssh-agent. If the login used to connect to the machine is not 'root',
`tezt-cloud` will automatically enable 'root' connections by key pair.

At this point, the machine is expected to be a debian. It does not need to be
provisionned, it will be provisionned automatically, by installing docker and
setting up docker repository access.

The following command should deploy and start an experiment:
```
dune exec tezt/tests/cloud/main.exe -- DAL -v --bootstrap --vms-limit 0 --ssh-host my.ssh.machine
```
