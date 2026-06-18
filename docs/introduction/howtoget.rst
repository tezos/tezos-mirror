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
- ``ubuntu/jammy`` (up to Octez version 24 included), becoming ``ubuntu/22.04`` (starting from Octez version 25)
- ``ubuntu/noble`` (up to Octez version 24 included), becoming ``ubuntu/24.04`` (starting from Octez version 25)
- ``ubuntu/26.04`` (starting from Octez version 25)

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

The above commands download a bootstrap signing key, configure the repository,
and then install the ``octez-archive-keyring`` package which ships the signing
keys at ``/usr/share/keyrings/octez-archive-keyring.gpg``. The ``sed`` command
switches APT to use the managed keyring. Once set up, signing key rotations are
handled automatically through normal package updates -- there is no need to
manually download keys when they are rotated.

If you already have Octez packages installed with a manually downloaded key,
you can migrate to the managed keyring by running::

  sudo apt-get update
  sudo apt-get install octez-archive-keyring
  sudo sed -i 's|signed-by=[^ ]*.gpg|signed-by=/usr/share/keyrings/octez-archive-keyring.gpg|' \
    /etc/apt/sources.list.d/octez.list
  sudo apt-get update

Then, to install the binaries, run the following command to install the octez-baker and all its dependencies:

::

  sudo apt install octez-baker

Once the Octez binary packages are installed, you may want to :ref:`start a node <quickstart_node>`, and then perhaps :ref:`start a baker <quickstart_baker>`.
Alternatively, they can be set up as services as explained in :doc:`./services`.

To remove the Octez packages you can simply run the following command.

::

  sudo apt-get autopurge -y octez-baker

If migrating from Serokell packages you can check out migration documentation
:doc:`./serokell`.

To upgrade packages, use ``apt-get update`` and ``apt-get upgrade``.
If runnning Octez as services, see also how to :ref:`restart them <services_upgrade>`.

.. warning::

	If you installed Ubuntu packages for Octez v24 (or earlier) from the APT repository, the old
	codename-based path is no longer updated. Running ``apt-get update`` and
	``apt-get upgrade`` will therefore not upgrade you to v25 until you repoint the
	repository to the new version-number-based path.

	To upgrade, set the ``release`` to your Ubuntu version number and re-add the
	repository:

	::

	  export distribution=ubuntu
	  export release=24.04  # or 22.04, 26.04

	then follow the install instructions above to refresh
	``/etc/apt/sources.list.d/octez.list``, and finally run::

	  sudo apt-get update
	  sudo apt-get upgrade

	Debian users are not affected: the Debian repository keeps using codenames
	(``debian/trixie``, ``debian/bookworm``).

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

Install Octez using the downloaded formula, creating a new tap if necessary, with the following commands::

	# Create a local tap if not having one already:
	brew tap-new octez-user/octeztap
	# Move formula to the newly created tap:
	mv octez.rb $(brew --repository)/Library/Taps/octez-user/homebrew-octeztap/Formula/
	# Install formula from tap
	brew install octez-user/octeztap/octez

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

For instance, to run a node on :ref:`currentnet <network_aliases>`, starting :doc:`from a snapshot <../user/snapshots>`, in Rolling :doc:`history mode <../user/history_modes>`, start with a fresh directory and configure the node::

    mkdir $HOME/rolling-data-directory
    docker run -it --rm \
      --volume "$HOME/rolling-data-directory:/home/tezos/.tezos-node" \
      tezos/tezos-bare:latest \
      octez-node config init --network https://teztnets.com/currentnet --rpc-addr 127.0.0.1 \
        --history-mode rolling

(You may use another location than ``$HOME``, but note that option ``--volume`` requires absolute paths.)

Then, download and import a snapshot, and finally run the node::

    wget -O $HOME/rolling https://snapshots.tzinit.org/currentnet/rolling
    docker run -it --rm \
      --volume "$HOME/rolling-data-directory:/home/tezos/.tezos-node" \
      --volume "$HOME/rolling:/rolling:ro" \
      tezos/tezos-bare:latest \
      octez-node snapshot import /rolling
    docker run --name octez-local-node -it \
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
A predefined Docker compose file is available at :src:`scripts/docker/bake.yml`.
It aims at helping you launch **a testnet** baker quickly, providing services for an Octez node, a DAL node, a baker, and an accuser.
You may copy the compose file in the directory where you want to run the baker::

    wget https://gitlab.com/tezos/tezos/-/blob/master/scripts/docker/bake.yml

The client, node, and DAL data are stored in the following subdirectories of your current directory, respectively: ``./client_data/``, ``./node_data/``, and ``./dal_data/``.
You may want to start with empty (or non-existent) directories in the beginning, then reuse them to restart the services.

.. note::

    If the node complains that it is configured for another network, you'll have to remove the node configuration file before running it:

    ::

        rm ./node_data/data/config.json

First, you have to make some choices:

- choose a :doc:`network <../user/multinetwork>` to connect to (by default, ``shadownet``)
- specify a vote for the :doc:`liquidity baking <../active/liquidity_baking>` feature (``on``, ``off``, or ``pass``)

Create an environment file called ``.env`` in your current directory with a content of the following form::

    LIQUIDITY_BAKING_VOTE='pass'
    NETWORK='tallinnnet'

and build the compose file::

    docker compose --file bake.yml build

Note that you must have already:

- a ``baker`` key on the network you chose, which is sufficiently funded, staked enough tez, and is registered as a delegate,
- optionally but recommended, a consensus key named ``signing-key`` and a ``companion-key`` associated with the baker key.

This is the case, for instance, if you had already run the baker using this compose file.

.. note::

	If you don't have yet configured the baker keys, you can first do this::

		docker compose -f bake.yml up manual-config -d

	wait until the node is bootstraped from the snapshot, and then do::

		docker compose -f bake.yml exec manual-config sh

	and in the shell set up the baker keys::

		octez-client gen keys baker
		octez-client show address baker
		# Fund baker with > 6000 tez, e.g. at https://faucet.shadownet.teztnets.com
		octez-client register key baker as delegate
		octez-client stake 6000 for baker
		# Define auxiliary consensus and companion keys (recommended):
		octez-client gen keys signing-key -s bls
		octez-client set consensus key for baker to signing-key
		octez-client gen keys companion-key -s bls
		octez-client set companion key for baker to companion-key
		exit

	Alternatively, if you already have the baker and auxiliary keys, but you never ran this compose file, you can import them by rather doing this in the above shell::

		octez-client import secret key baker unencrypted:<secret-key>
		octez-client import secret key consensus-key unencrypted:<secret-key>
		octez-client import secret key companion-key unencrypted:<secret-key>
		# if your baker was deactivated, reactivate it:
		octez-client register key baker as delegate

	You can arbitrarily combine the two methods above, at your convenience.
	For example, if you have a baker key but not auxiliary keys, you can import the baker key and create the auxiliary keys.

Now, you just have to start all the services::

    docker compose --file bake.yml up -d

You can see the logs by doing in another terminal (in the same directory)::

    docker compose --file bake.yml logs -f

(add a service such as ``node`` or ``baker`` at the end of the command line to see only its log).

You should have now running together: the node, the DAL node, the baker and the accuser.

You can stop the services with::

    docker compose --file bake.yml down

Alternatively, you may stop and restart only one service. For instance if the Octez version you are using requires to upgrade the version of the storage, you can restart the node after upgrading the storage::

    docker compose -f bake.yml stop node
    docker compose -f bake.yml run --rm node octez-node upgrade storage --data-dir /var/run/tezos/node/data
    docker compose -f bake.yml up node

Further customization
^^^^^^^^^^^^^^^^^^^^^

Beyond the common usage shown above, you can use the following variables in the environment file to customize the behavior of the compose file, grouped by category.

Configuration:

- LIQUIDITY_BAKING_VOTE (mandatory): value to pass to the baker daemon in mandatory flag ``--liquidity-baking-toggle-vote``
- NETWORK (default: shadownet): network to connect to
- NETWORK_URL (default: ``https://teztnets.com/$NETWORK``): URL to get the network configuration
- HISTORY_MODE (default: rolling): set the :doc:`history mode <../user/history_modes>` of your node
- TEZOS_NODE_OPTIONS (default: none): extra options to be passed to the node

Snapshot management:

- IMPORT_SNAPSHOT (default: true): if false, don't import a snapshot, bootstrap from origin; if true, import a snapshot unless the node has recent data (but see FORCE_IMPORT_SNAPSHOT)
- FORCE_IMPORT_SNAPSHOT (default: false): import the snapshot even if the node has recent data (same day); has no effect when IMPORT_SNAPSHOT is false
- FORCE_DOWNLOAD_SNAPSHOT (default: false): if true, download the snapshot even if it has been found localy (see SNAPSHOT_URL)
- CHECK_SNAPSHOT (default: false): if false, import the snapshot with option --no-check
- SNAPSHOT_URL (default: use teztnet server): URL where download the snapshot if not found localy (see SNAPSHOT_NAME)
- SNAPSHOT_NAME (default: no local file): name of a local snapshot file

Baking keys management:

- BAKER_ADDRESS: the ``tz...`` address of the baker manager account (not needed if the key ``baker`` has been configured manually)
- SIGNING_KEY: the name of the signing key (defaults to ``signing-key``)
- COMPANION_KEY: the name of the companion key (defaults to ``companion-key``; optional, unless the signing key is a tz4 address)
- BAKER_SECRET_KEY: secret key of the baker (not needed if a baker or signing key has been configured manually, or if CONSENSUS_SECRET_KEY is defined)
- CONSENSUS_SECRET_KEY: secret key of the consensus key (not needed if a baker or signing key has been configured manually, or if BAKER_SECRET_KEY is defined)
- COMPANION_SECRET_KEY: secret key of the companion address (optional, unless a baker or signing key has been configured manually as a tz4 address or CONSENSUS_SECRET_KEY is defined as a tz4 address)

.. note::

	If you want to manually configure the node, use the same ``manual-config`` service as for configuring the baker keys::

		docker compose -f bake.yml exec manual-config sh

	then in the shell interact with the node directly::

		octez-node config show
		octez-node config update ...

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

Appendix
--------

.. toctree::
   :maxdepth: 2

   howtobuild
   cosign-verify
   get_troubleshooting
