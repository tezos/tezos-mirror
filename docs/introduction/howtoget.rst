.. _howtoget:

Installing Octez
================

In this how-to we explain how to get up-to-date binaries to run Tezos
(more precisely, the "Octez" implementation of Tezos software)
on any network (either on the mainnet or on one of the test networks).
Octez consists of :ref:`several binaries <tezos_binaries>` (i.e., executable files), including: a client, a node, and a baker.

There are several options for getting the binaries, depending on how you plan to use Octez:

- :ref:`installing packages <installing_packages>`.
  This is the easiest way to install native binaries for the latest stable release, together with their dependencies, using a package manager.
- :ref:`using docker images <using_docker_images>`.
  This is the easiest way to run the latest stable release of the binaries in
  Docker containers, on any OS supported by Docker.
- :ref:`getting static binaries <getting_static_binaries>`.
  This is the easiest way to get native binaries for the latest stable release,
  requiring no dependencies, under Linux.
- :ref:`using homebrew <using_homebrew>`.
  For macOS (and also Linux) you can use our Homebrew formula to compile and
  install octez.
- :ref:`building the binaries via the OPAM source package manager <building_with_opam>`.
  Take this way to install the latest stable release in your native OS
  environment, automatically built from sources.
- :doc:`Building Octez <howtobuild>` by
  compiling the sources like developers do.
  This is the way to take if you plan to contribute to the source code.
  It allows to install any version you want (typically, the current
  development version on the master branch) by compiling it yourself from the
  sources.


These different options are described in the following sections, except the last one, described in its own page.

Some Octez binaries also require certain parameter files to run. Only some of the packaged distributions include such parameter files. Therefore, depending on the type of installation and your user profile, you may have to install some extra parameter files separately. Their installation is described in page :doc:`howtobuild`, but those instructions may be used for other installation types:

- :ref:`setup_zcash_params`
- :ref:`setup_dal_crypto_params`

Note that some of the packaged distributions are not only available for the latest stable release. For instance, static binaries are also available for release candidates, and Docker images are also available for the current development version (see :doc:`../releases/releases` for more information).

When choosing between the installation options, you may take into account the
convenience of the installation step (and of upgrading steps), but also
efficiency and security considerations. For instance, static binaries have a
different memory footprint compared to dynamically-linked binaries. Also,
compiling the sources in the official Octez
repository is more secure than installing OPAM packages from a repository that
is not under Tezos control. In particular, compiling from sources enforces a fixed set of dependencies; when compiling via OPAM, this set of dependencies may change, which may or may not be compatible with your security practices.

All our installation scenarios are tested daily, including by automated means, to ensure that they are correct and up to date.
These tests are performed by applying scenarios in several standard environments, from scratch.
However, if you encounter problems when performing one of the installation scenarios in your own environment, you may want to take a look at :doc:`get_troubleshooting`.

.. _installing_packages:

Installing binary packages
--------------------------

When it comes to installing software, especially for critical
applications like Tezos/Octez, it’s crucial to ensure a secure and
stable environment. While compiling from source can provide
customization options, it often introduces complexities and risks.
Instead, opting for binary packages such as Deb packages from a trusted source simplifies
installation and enhances security.

Deb packages compiled for a specific platform should be always preferred
over statically compiled binaries. Deb packages can also be used to
simplify the creation of `OCI <https://opencontainers.org/>`__ images or simply
deployed on bare metal using provisioning tools such as
`Ansible <https://docs.ansible.com/>`__.

Using the official Deb packages offers several advantages:

-  **Security**: Packages are pre-compiled and thoroughly tested,
   reducing the risk of vulnerabilities introduced during compilation.
   All our packages are signed and our supply chain is strictly
   monitored to make sure the packages that we deliver only use
   components that were vetted by our engineering team.

-  **Stability**: Packages from a trusted repository undergo rigorous testing,
   ensuring stability and compatibility with the target system. We make sure to
   compile our binaries in a clean environment and using an up-to-date software
   distribution. We use LTS (long-term service) distributions to enhance
   stability and reduce the attack surface.

-  **Ease of Installation**: Packages can be installed using standard package
   management tools, streamlining the process. For instance, ``apt`` is
   ubiquitous in the Debian world. These tools allow us to sign our packages
   that can be automatically verified by the end user during installation. We
   provide packages that allow the end user to easily tailor their installation
   for different use cases.

-  **Reduced Downtime**: With reliable binaries and straightforward
   installation, system downtime due to installation errors or
   compatibility issues is minimized. We carefully test the upgrade
   process of our packages to make sure that end users can enjoy a click and go
   upgrade process with near to zero downtime.

.. _installing_deb:

Ubuntu and Debian Octez packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you're using Ubuntu or Debian, you can install the Debian packages
using ``apt`` directly from our APT repository.

We support the following distribution/releases:

- ``debian/trixie``
- ``debian/bookworm``
- ``ubuntu/noble``
- ``ubuntu/jammy``

both on ``amd64`` and ``arm64`` architectures.

In order to set the Tezos package repository to your machine, do:

::

  export distribution=debian
  export release=trixie

We also maintain a separate repository for release candidates. To set
the last release candidate or Beta simply prepend ``RC/`` or ``BETA/`` to the distribution name
as in ``export distribution=RC/debian``.

Now add the Tezos package repository:

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [add repository]
   :end-before: [end add repository]

Then, to install the binaries, run the following command to install the octez-baker and all its dependencies:

::

  sudo apt install octez-baker

Once the Octez binary packages are installed, they can be set up as services
as explained in :doc:`./services`.

To remove the Octez packages you can simply run the following command.

::

  sudo apt-get autopurge -y octez-baker

If migrating from Serokell packages you can check out migration documentation
:doc:`./serokell`.

To upgrade packages, use ``apt-get update`` and ``apt-get upgrade``.
If runnning Octez as services, see also how to :ref:`restart them <services_upgrade>`.

.. _installing_rpm:

RPM Octez packages
~~~~~~~~~~~~~~~~~~

.. warning::

	RPM packages are no longer provided starting with Octez version 24.
	Please choose another installation method.

If you're using Fedora or Rocky Linux, you can install RPM packages with Octez binaries from the
Octez from our DNF repository. Currently we support the latest LTS release for
Fedora and for RockyLinux :

- ``rockylinux/9.3``
- ``fedora/39``
- ``fedora/42``

both on ``amd64`` and ``arm64`` architectures.

In order to add the Tezos package repository to your machine, do:

::

  export distribution=rockylinux
  export release=9.3

and run:

.. literalinclude:: install-bin-rpm.sh
   :language: shell
   :start-after: [add repository]
   :end-before: [end add repository]

For ``rockylinux`` user you also need to add the ``devel`` repository

::

  dnf -y config-manager --set-enabled devel

To update the local dnf registry run:

::

  dnf update

We also maintain a separate repository for release candidates. To install
the last release candidate or Beta simply prepend ``RC/`` or ``BETA/`` to the distribution name
as in ``export distribution=RC/rockylinux``

Then, to install the binaries, run the following commands:

::

  dnf -y install octez-node
  dnf -y install octez-client
  dnf -y install octez-baker
  dnf -y install octez-dal-node
  dnf -y install octez-smart-rollup-node

To remove the Octez packages you can simply run the following command.

::

  dnf -y remove octez-node octez-client octez-baker \
                octez-dal-node octez-smart-rollup-node

To upgrade packages, use ``dnf update``.
If running Octez as services, see also how to :ref:`restart them <services_upgrade>`.


.. _getting_static_binaries:

Getting static binaries
-----------------------

You can get static Linux binaries of the latest release from the
`Octez package registry <https://gitlab.com/tezos/tezos/-/packages/>`__.

This repository provides static binaries for x86_64 and arm64 architectures. Since these binaries
are static, they can be used on any Linux distribution without any additional prerequisites.
However, note that, by embedding all dependencies, static binary executables are typically much larger than dynamically-linked executables.

For upgrading to a newer release, you just have to download and run the new
versions of the binaries.

Octez static binaries are signed via GPG. To verify that a binary was not tampered with,
you can download and import our public GPG key and use ``gpg`` to verify the signature associated with the binary.

::

  curl -O https://packages.nomadic-labs.com/octez.asc
  gpg --import octez.asc
  ... # download the static binary you want to verify and the associated sig file
  gpg --verify <bin>.sig <bin>

.. _using_homebrew:

Using Homebrew
--------------

On macOS and Linux, you can compile and install Octez using Homebrew. If
Homebrew is not yet installed on your system, please refer to the official
`Homebrew installation guide <https://brew.sh/>`__ for detailed instructions.

Once Homebrew is set up, follow these steps to install Octez.

Download the Octez formula by executing the following command in your terminal::

    curl -q "https://packages.nomadic-labs.com/homebrew/Formula/octez.rb" -O

For ``RC`` versions, do rather::

    curl -q "https://packages.nomadic-labs.com/homebrew/RC/Formula/octez.rb" -O

Install Octez using the downloaded formula with the following command::

    brew install -v ./octez.rb

Depending on the speed of your system, the build can take more than 10
minutes. We regularly test the build in our CI using macOS 14 (Sonoma) with Xcode 15 on an ARM-based Mac.
More recent configurations should also work.

.. _using_docker_images:

Using Docker images
-------------------

For every change committed in the GitLab repository, Docker images are
automatically generated and published on `DockerHub
<https://hub.docker.com/r/tezos/tezos/>`_. This provides a convenient
way to run an always up-to-date ``octez-node``.

From version 22.0 all Octez Docker images are signed using Cosign.
You can verify if the images are correctly signed, :doc:`using Cosign <./cosign-verify>`.

You can use the Docker images either directly or using Docker compose files, as explained next.
In both cases, you need to have `Docker <https://www.docker.com>`__ installed and started (`Docker Desktop <https://www.docker.com/products/docker-desktop/>`__ would suffice for the instructions below).

Plain Docker images
~~~~~~~~~~~~~~~~~~~

The Docker images can be directly used to run the different Octez binaries.
To make sure you use the most recent version of Octez, run::

    docker pull tezos/tezos-bare:latest

For instance, to run a node on the Shadownet :doc:`test network <../user/multinetwork>`, starting :doc:`from a snapshot <../user/snapshots>`, in Rolling :doc:`history mode <../user/history_modes>`, start with a fresh directory and configure the node::

    mkdir $HOME/rolling-data-directory
    docker run -it --rm \
      --volume "$HOME/rolling-data-directory:/home/tezos/.tezos-node" \
      tezos/tezos-bare:latest \
      octez-node config init --network shadownet --rpc-addr 127.0.0.1 \
        --history-mode rolling

(You may use another location than ``$HOME``, but note that option ``--volume`` requires absolute paths.)

Then, download and import a snapshot, and finally run the node::

    wget -O $HOME/rolling <snapshot-url>
    docker run -it --rm \
      --volume "$HOME/rolling-data-directory:/home/tezos/.tezos-node" \
      --volume "$HOME/rolling:/rolling:ro" \
      tezos/tezos-bare:latest \
      octez-node snapshot import /rolling
    docker run --name octez-local-node -d \
      --volume "$HOME/rolling-data-directory:/home/tezos/.tezos-node" \
      tezos/tezos-bare:latest \
      octez-node run

You may check when your node is bootstrapped by executing in another terminal::

    docker exec -it octez-local-node octez-client bootstrapped

You may stop and restart the node as needed, for instance if you need to upgrade the version of the storage::

    docker stop octez-local-node
    docker run --rm --volumes-from octez-local-node tezos/tezos-bare:latest \
      octez-node upgrade storage
    docker start octez-local-node

Docker compose files
~~~~~~~~~~~~~~~~~~~~

Another way to run those Docker images is with `docker-compose <https://docs.docker.com/compose>`_.
A working example of a simple Docker compose file is available at :src:`scripts/docker/bake.yml`.
The compose file is able to launch services for an Octez node, a DAL node, a baker, and an accuser.

First, you have to make some choices:

- choose a :doc:`network <../user/multinetwork>` to connect to (a testnet name, ``sandbox``,  or ``mainnet``)
- choosing the desired :doc:`history mode <../user/history_modes>` (``rolling``, ``full``, or ``archive``)
- specify a vote for the :doc:`liquidity baking <../active/liquidity_baking>` feature (``on``, ``pass``, or ``off``)

For instance, to configure and run the node on the active protocol on Shadownet from a snapshot, in Rolling history mode::

    wget https://gitlab.com/tezos/tezos/-/raw/master/scripts/docker/bake.yml
    export LIQUIDITY_BAKING_VOTE=pass
    docker compose -f bake.yml run --rm -it node octez-node config init \
       --network https://teztnets.com/shadownet --history-mode rolling \
       --data-dir /var/run/tezos/node/data \
       --rpc-addr '[::]:8732' --allow-all-rpc '[::]:8732'
    wget -O $HOME/rolling https://snapshots.tzinit.org/shadownet/rolling
    docker compose -f bake.yml run --rm -it -v "$HOME/rolling:/snapshot:ro" \
      node octez-node snapshot import --data-dir /var/run/tezos/node/data /snapshot
    docker compose -f bake.yml up node

Note in the commands above that ``node`` is the name of the service running the ``octez-node`` executable.
You may ignore possible warnings about environment variable ``BAKER_ADDRESS``, that we will set later on for the DAL node.

The client and node data are stored in local subdirectories of your current directory, respectively ``./client_data/`` and ``./node_data/``.
You may want to start with empty (or non-existent) directories in the beginning, then reuse them to restart the services.

.. note::

    If the node complains that it is configured for another network, you'll have to remove the node configuration file before running it:

    ::

        rm ./node_data/data/config.json

You may check when your node is bootstrapped by running ``octez-client`` inside the node's container::

    docker compose -f bake.yml exec node octez-client bootstrapped

You may stop and restart the node as needed. For instance if the Octez version you are using requires to upgrade the version of the storage, you can restart the node after upgrading the storage::

    docker compose -f bake.yml stop node
    docker compose -f bake.yml run --rm node octez-node upgrade storage --data-dir /var/run/tezos/node/data
    docker compose -f bake.yml up node

To run the baker, you must configure a baking key (if you have one you may skip this step.
While the node is running, do (in another window if needed)::

    docker compose -f bake.yml exec node sh

and in the shell do::

    export TEZOS_CLIENT_DIR=/var/run/tezos/client
    octez-client gen keys mybaker
    octez-client show address mybaker
    # Note down the address of mybaker
    # Fund mybaker with > 6000 tez, e.g. at https://faucet.ghostnet.teztnets.com
    octez-client register key mybaker as delegate
    octez-client stake 6000 for mybaker

Once the baking key is configured, stop the node and then run all the services::

    export BAKER_ADDRESS=tz...
    docker compose -f bake.yml up

Now you should have running together: the node, the DAL node, the baker and the accuser.

Building Docker Images Locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The docker image used throughout the docker-compose files is fetched from upstream, but you can also
build one locally and reference it. Run the following command to build the image:

::

    ./scripts/create_docker_image.sh


And then update the docker-compose file (e.g., ``alpha.yml``) with the docker tag::

    node:
      image: tezos:latest
      ...

Docker Image Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~

Lastly, the entrypoint script (:src:`scripts/docker/entrypoint.sh`) provides the following configurable
environment variables:

- ``DATA_DIR``: The directory to store the node's data (defaults to ``/var/run/tezos``).
- ``NODE_HOST``: The name of the node container (defaults to ``node``).
- ``NODE_RPC_PORT``: The RPC port **inside the container** the node listens to (defaults to ``8732``).
- ``NODE_RPC_ADDR``: The RPC address **inside the container** the node binds to (defaults to ``[::]``).
- ``PROTOCOL``: The protocol used (if the protocol is not provided,
  ``octez-baker`` and ``octez-accuser`` will be used).

These variables can be set in the docker-compose file, as demonstrated in :src:`scripts/docker/alpha.yml`::

    octez-node:
      ...
      environment:
        PROTOCOL: alpha
      ...

If the above options are not enough, you can always replace the default ``entrypoint`` and ``command`` fields.

::

    version: "3"
    services:
      octez-node:
        container_name: octez-node-alpha
        entrypoint: /bin/sh
        command: /etc/my-init-script.sh
        volumes:
          - ./my-init-script.sh:/etc/my-init-script.sh
          - ...
        environment:
          PROTOCOL: alpha
     ...

.. _installing_binaries:

Installing binaries
-------------------

Depending on your operating system, you may install Octez (dynamically-linked)
binaries and their dependencies by first downloading the packages for your
distribution from the `Octez release page
<https://gitlab.com/tezos/tezos/-/releases>`__, browsing to your distribution
and then installing them with your package tool manager. Most of the
configuration options are accessible by the user in ``/etc/default/<package>``.

If you are upgrading from a different package distributor such as `Serokell's tezos-packaging <https://github.com/serokell/tezos-packaging>`__,
please pay attention to the possible differences between the two packages, in
particular regarding the home directory for the ``tezos`` user.

There are several packages:

- ``octez-client``: the client for manipulating wallets and signing items
- ``octez-node``: the Octez node
- ``octez-baker``: the Octez baking and VDF daemons
- ``octez-smartrollup``: the Octez Smart Rollup daemons
- ``octez-signer``: the remote signer, to hold keys on (and sign from) a different machine from the baker or client

.. _building_with_opam:

Building from sources via OPAM
------------------------------

The easiest way to build the binaries from the source code is to use the OPAM
source package manager for OCaml.

This is easier than setting up a complete development environment as described in :doc:`howtobuild`, like developers do.
However, this method is recommended for expert users as it requires basic
knowledge of the OPAM package manager and the OCaml packages
workflow. In particular, upgrading Octez from release to
release might require tinkering with different options of the OPAM
package manager to adjust the local environment for the new
dependencies.


.. _build_environment:

Environment
~~~~~~~~~~~

Currently Octez is being developed for Linux x86_64, mostly for
Ubuntu and Fedora Linux. The following OSes are also reported to
work: macOS (x86_64), Arch Linux ARM (aarch64), Debian Linux (x86_64). A Windows port is feasible and might be
developed in the future.

.. note::

    If you build the binaries by using the following instructions inside a
    Docker container, you have to give extended privileges to this container,
    by passing option ``--privileged`` to the ``docker run`` command.

.. warning::

   Mixing LLVM and GNU binutils toolchains can cause issues when building Octez. If you encounter
   an error like this, it may be that you have tools from both LLVM and GNU in scope.

   ::

     Error: ExternalToolError { reason: "Failed to create archive index with `ranlib`", tool: "ranlib", args: ["liboctez_rust_deps.a"], stdout: "", stderr: "LLVM ERROR: Invalid encoding\n" }

   In this case, refer to :ref:`Mixing LLVM and GNU binutils <mixing_llvm_gnu_binutils>`.

Install OPAM
~~~~~~~~~~~~

First, you need to install the `OPAM <https://opam.ocaml.org/>`__
package manager, at least version 2.1, that you can get by following the `install instructions <https://opam.ocaml.org/doc/Install.html>`__.

After the first install of OPAM, use ``opam init --bare`` to set it up
while avoiding to compile an OCaml compiler now, as this will be done in
the next step.

.. _install_opam_packages:

Install Octez OPAM packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The latest Octez release is available (as soon as possible after the
release) directly as OPAM packages in a specific repository (defined below).

.. note::

   Every file related to OPAM is (by default) in ``$HOME/.opam`` which
   means that, first, OPAM installs are user-specific and, second, you
   can get rid of everything by removing this directory (+ updating
   your rc files (``$HOME/.bashrc``, ``$HOME/.profile``,
   ``$HOME/.zshrc``, ``$HOME/.emacs``, ...) if you asked/allowed OPAM
   to add some lines in them).

The binaries need a specific version of the OCaml and Rust compilers (see the values
of variables ``$ocaml_version`` and ``$recommended_rust_version`` in file ``scripts/version.sh``). To get an environment with them do:

.. literalinclude:: install-opam.sh
  :language: shell
  :start-after: [install ocaml compiler]
  :end-before: [add octez repository]

.. note::

   The ``opam switch create`` command may fail if the switch already exists;
   you are probably re-installing or upgrading an existing installation.
   If the required compiler version has not changed since the last time, you
   may simply ignore this error. Otherwise, you are upgrading to a new compiler,
   so look at the :ref:`relevant section below <updating_with_opam>`.

   The command ``eval $(opam env)`` sets up required environment
   variables. OPAM will suggest to add it in your rc file. If, at any
   point, you get an error like ``octez-something: command not
   found``, first thing to try is to (re)run ``eval $(opam
   env --switch $ocaml_version)`` (replace ``$ocaml_version`` with its value
   in ``scripts/version.sh``) to see if it fixes the problem.

.. note::

   If an OPAM commands times out, you may allocate it more time for its
   computation by setting the OPAMSOLVERTIMEOUT environment variable (to a
   number of seconds), e.g. by adding ``OPAMSOLVERTIMEOUT=1200`` before the
   command. If no timeout occurs, you may omit this part.

The Octez packages are distributed in their own Opam repository, which you have to add:

.. literalinclude:: install-opam.sh
  :language: shell
  :start-after: [add octez repository]
  :end-before: [install tezos]

Now, install all the binaries by:

.. literalinclude:: install-opam.sh
  :language: shell
  :start-after: [install tezos]
  :end-before: [test executables]

You can be more specific and only ``opam install octez-node``, ``opam
install octez-baker``, ...

.. warning::

   Note that ``opam install octez-client`` and ``opam install
   octez-signer`` are "minimal" and do not install the support for
   Ledger Nano devices. To enable it, run ``opam install
   ledgerwallet-tezos`` in addition to installing the binaries. (The
   macro meta-package ``tezos`` installs ``ledgerwallet-tezos``.)

.. _updating_with_opam:

Updating via OPAM
~~~~~~~~~~~~~~~~~

Installation via OPAM is especially convenient for updating to newer
versions. Once some libraries/binaries are installed and new versions
released, you can update by:

::

   opam update
   opam upgrade

It is recommended to also run the command ``opam remove -a`` in order
to remove the dependencies installed automatically and not needed
anymore. Beware not uninstall too much though.

Identified situations where it will be more tricky are:

* When the OCaml compiler version requirement changes. In this case,
  you have several possibilities:

  - Be explicit about the "upgrade" and do ``opam upgrade --unlock-base
    ocaml.$new_version tezos``. Note that starting from OPAM version 2.1,
    this option is replaced by ``--update-invariant`` (see the `opam-switch
    manual <https://opam.ocaml.org/doc/man/opam-switch.html>`_).
  - Remove the existing switch (e.g., ``opam switch remove for_tezos``, but
    be aware that this will delete the previous installation), and replay
    :ref:`the installation instructions <install_opam_packages>`.
  - Replay :ref:`the installation instructions <install_opam_packages>` while
    creating a different switch (e.g. ``ocaml_${ocaml_version}_for_tezos``), but
    be aware that each switch consumes a significant amount of disk space.

* When there are Rust dependencies involved. The way to go is still
  unclear.
  The solution will be defined when delivering the first release with Rust
  dependencies.


Appendix
--------

.. toctree::
   :maxdepth: 2

   howtobuild
   cosign-verify
   get_troubleshooting
