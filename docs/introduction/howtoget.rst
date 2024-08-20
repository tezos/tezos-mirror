.. _howtoget:

Installing Octez
================

In this how-to we explain how to get up-to-date binaries to run Tezos
(more precisely, the "Octez" implementation of Tezos software)
on any network (either on the mainnet or on one of the test networks).
Octez consists of :ref:`several binaries <tezos_binaries>` (i.e., executable files), including: a client, a node, and a baker.

There are several options for getting the binaries, depending on how you plan to use Octez:

- :ref:`getting static binaries <getting_static_binaries>`.
  This is the easiest way to get native binaries for the latest stable release,
  requiring no dependencies, under Linux.
- :ref:`installing binaries <installing_binaries>`.
  This is the easiest way to install native binaries for the latest stable release, together with their dependencies, using a package manager.
- :ref:`using docker images <using_docker_images>`.
  This is the easiest way to run the latest stable release of the binaries in
  Docker containers, on any OS supported by Docker.
- :ref:`building the binaries via the OPAM source package manager <building_with_opam>`.
  Take this way to install the latest stable release in your native OS
  environment, automatically built from sources.
- :ref:`setting up a complete development environment <compiling_with_make>` by
  compiling the sources like developers do.
  This is the way to take if you plan to contribute to the source code.
  It allows to install any version you want (typically, the current
  development version on the master branch) by compiling it yourself from the
  sources.


These different options are described in the following sections.

Some Octez binaries also require some parameter files to run. Only some of the packaged distributions include such parameter files. Therefore, depending on the type of installation and your user profile, you may have to install some extra parameter files separately. Their installation is currently described in section :ref:`compiling_with_make`, but those instructions may be used for other installation types:

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


Ubuntu and Debian Octez packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you're using Ubuntu or Debian, you can also install packages with Octez binaries
using ``apt`` directly from our APT repository, instead of going to the Octez
release page as explained above.

.. warning::

    This APT repository is currently empty. For installing the latest release (:doc:`../releases/version-20`), you must still use the installation method above.

We support the following distribution/releases:
- ``debian/bookworm``
- ``ubuntu/noble``
- ``ubuntu/jammy``

both on ``amd64`` and ``arm64`` architectures.

In order to add the Tezos package repository to your machine, do:

::

    export distribution=debian
    export release=bookworm
    export bucket="tezos-linux-repo"

and run:

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [install prerequisites]
   :end-before: [ preeseed octez ]

We also maintain a separate repository for release candidates. To install
the last release candidate simply prepend ``RC/`` to the distribution name
as in ``export distribution=RC/debian``

Then, to install the binaries, run the following commands:

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [install tezos]
   :end-before: [install octez additional packages]

To remove the octez packages you can simly run the following command.

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [test autopurge]
   :end-before: [check autopurge]

Also there are some experimental packages:

- ``octez-experimental`` - binaries that are considered experimental including
  the Alpha baker
- ``octez-evm-node`` - the EVM endpoint node for Etherlink

The packages are set up to run under a dedicated user. The ``octez-node``,
``octez-baker`` and ``octez-smartrollup`` packages use a user and group called
tezos. The ``octez-signer`` package uses a user and group called tzsigner. It’s
possible to configure the software to use a different user (even root).

The documentation for these packages, originally developed by Chris Pinnock,
can be found here: https://chrispinnock.com/tezos/packages/

Fedora Octez packages
~~~~~~~~~~~~~~~~~~~~~

If you're using Fedora, you can install packages with Octez binaries
from the Octez release page indicated above
using ``rpm`` or ``dnf``. Currently it supports the latest LTS release for
Fedora and for RockyLinux.

Upgrading to a new or more recent release requires downloading again all the ``rpm``
packages and repeat the installation.

For example using ``yum``::

    yum install ./octez-client-19.1-1.x86_64.rpm

.. _using_docker_images:

Using Docker Images And Docker-Compose
--------------------------------------

For every change committed in the GitLab repository, Docker images are
automatically generated and published on `DockerHub
<https://hub.docker.com/r/tezos/tezos/>`_. This provides a convenient
way to run an always up-to-date ``octez-node``.

One way to run those Docker images is with `docker-compose <https://docs.docker.com/compose>`_.
We provide ``docker-compose`` files for all active
protocols. You can pick one and start with the following command (we'll assume alpha on this guide):

::

    cd scripts/docker
    export LIQUIDITY_BAKING_VOTE=pass # You can choose between 'on', 'pass' or 'off'.
    docker-compose -f alpha.yml up

The above command will launch a node, a client, a baker, and an accuser for
the Alpha protocol.

You can open a new shell session and run ``docker ps`` in it, to display all the available containers, e.g.::

    8f3638fae48c  docker.io/tezos/tezos:latest  octez-node            3 minutes ago  Up 3 minutes ago   0.0.0.0:8732->8732/tcp, 0.0.0.0:9732->9732/tcp  node-alpha
    8ba4d6077e2d  docker.io/tezos/tezos:latest  octez-baker --liq...  3 minutes ago  Up 31 seconds ago                                                  baker-alpha
    3ee7fcbc2158  docker.io/tezos/tezos:latest  octez-accuser         3 minutes ago  Up 35 seconds ago                                                  accuser-alpha


The node's RPC interface will be available on localhost and can be queried with ``octez-client``.

::

    docker exec node-alpha octez-client rpc list

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
- ``PROTOCOL``: The protocol used.

These variables can be set in the docker-compose file, as demonstrated in ``alpha.yml``::

    node:
      ...
      environment:
        PROTOCOL: alpha
      ...

If the above options are not enough, you can always replace the default ``entrypoint`` and ``command`` fields.

::

    version: "3"
    services:
      node:
        container_name: node-alpha
        entrypoint: /bin/sh
        command: /etc/my-init-script.sh
        volumes:
          - ./my-init-script.sh:/etc/my-init-script.sh
          - ...
        environment:
          PROTOCOL: alpha
     ...

.. _building_with_opam:

Building from sources via OPAM
------------------------------

The easiest way to build the binaries from the source code is to use the OPAM
source package manager for OCaml.

This is easier than :ref:`setting up a complete development environment <build_from_sources>`, like developers do.
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
release) directly as OPAM packages.

.. note::

   Every file related to OPAM is (by default) in ``$HOME/.opam`` which
   means that, first, OPAM installs are user-specific and, second, you
   can get rid of everything by removing this directory (+ updating
   your rc files (``$HOME/.bashrc``, ``$HOME/.profile``,
   ``$HOME/.zshrc``, ``$HOME/.emacs``, ...) if you asked/allowed OPAM
   to add some lines in them).

The binaries need a specific version of the OCaml compiler (see the value
of variable ``$ocaml_version`` in file ``scripts/version.sh``). To get an environment with it do:

.. literalinclude:: install-opam.sh
  :language: shell
  :start-after: [install ocaml compiler]
  :end-before: [install tezos]

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

Now, install all the binaries by:

.. literalinclude:: install-opam.sh
  :language: shell
  :start-after: [install tezos]
  :end-before: [test executables]

You can be more specific and only ``opam install octez-node``, ``opam
install octez-baker-alpha``, ...

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

.. _build_from_sources:
.. _compiling_with_make:

Setting up the development environment from scratch
---------------------------------------------------

If you plan to contribute to the Octez codebase, the way to go is to set up a
complete development environment, by cloning the repository and compiling the
sources using the provided makefile.

**TL;DR**: From a fresh Debian Bookworm x86_64, you typically want to select a source branch in the Octez repository, e.g.:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [select branch]
  :end-before: [end]

and then do:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [install packages]
  :end-before: [test executables]

.. warning::

   If you are updating to :doc:`Octez v14<../releases/version-14>`
   using a development environment which had been used to build Octez
   versions up to v13.x, and also you have previously exported the
   ``tezos`` directory to the ``$PATH`` environment variable, the
   following stanza is necessary to avoid potential issues with opam
   in the ``make build-deps`` step::

     PATH=${PATH##"$HOME"/tezos/:}

   Otherwise, it is possible for ``make build-deps`` to fail with the
   following (or a similar) error::

     make: opam: Permission denied
     Makefile:53: *** Unexpected opam version (found: , expected: 2.*).  Stop.

The following sections describe the individual steps above in more detail.

.. note::

  Besides compiling the sources, it is recommended to also :ref:`install Python and some related tools <install_python>`, which are needed, among others, to build the documentation and to use the Git :doc:`pre-commit hook <../developer/pre_commit_hook>`.

.. _setup_rust:

Install Rust
~~~~~~~~~~~~

Compiling Octez requires the Rust compiler (see recommended version in variable
``$recommended_rust_version`` in file ``scripts/version.sh``) and the
Cargo package manager to be installed. If you have `rustup
<https://rustup.rs/>`_ installed, it should work without any
additional steps on your side. You can use `rustup
<https://rustup.rs/>`_ to install both. If you do not have ``rustup``,
please avoid installing it from Snapcraft; you can rather follow the
simple installation process shown below:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [install rust]
  :end-before: [source cargo]

Once Rust is installed, note that your ``PATH`` environment variable
(in ``.profile``) may be updated and you will need to restart your session
so that changes can be taken into account. Alternatively, you can do it
manually without restarting your session:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [source cargo]
  :end-before: [get sources]

Note that the command line above assumes that rustup
installed Cargo in ``$HOME/.cargo``, but this may change depending on how
you installed rustup. See the documentation of your rustup distribution
if file ``.cargo`` does not exist in your home directory.

.. _setup_zcash_params:

Install Zcash Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

Octez binaries require the Zcash parameter files to run.
Docker images come with those files, and the source distribution also
includes those files. But if you compile from source and move Octez to
another location (such as ``/usr/local/bin``), the Octez binaries may
prompt you to install the Zcash parameter files. The easiest way is to
download and run this script::

   wget https://raw.githubusercontent.com/zcash/zcash/713fc761dd9cf4c9087c37b078bdeab98697bad2/zcutil/fetch-params.sh
   chmod +x fetch-params.sh
   ./fetch-params.sh

The node will try to find Zcash parameters in the following directories,
in this order:

#. ``$XDG_DATA_HOME/.local/share/zcash-params``
#. ``$XDG_DATA_DIRS/zcash-params`` (if ``$XDG_DATA_DIRS`` contains
   several paths separated by colons ``:``, each path is considered)
#. ``$OPAM_SWITCH_PREFIX/share/zcash-params``
#. ``./_opam/share/zcash-params``
#. ``~/.zcash-params``
#. ``~/.local/share/zcash-params``
#. ``/usr/local/share/zcash-params``
#. ``/usr/share/zcash-params``

If the node complains that it cannot find Zcash parameters, check that
at least one of those directories contains both files ``sapling-spend.params``
and ``sapling-output.params``. Here is where you should expect to find those files:

* if you are compiling from source, parameters should be in
  ``_opam/share/zcash-params`` (you may need to run ``eval $(opam env)``
  before running the node);

* if you used ``fetch-params.sh``, parameters should be in ``~/.zcash-params``.

.. note::

   Some operating systems may not be covered by the list of directories above.
   If Zcash is located elsewhere on your system (typically, on MacOS X), you may try creating a symbolic link such as: ``ln -s ~/Library/Application\ Support/ZcashParams ~/.zcash-params``.

Note that the script ``fetch-params.sh`` downloads a third file containing parameters for Sprout (currently called ``sprout-groth16.params``), which is not loaded by Sapling and can be deleted to save a significant amount of space (this file is *much* bigger than the two other files).

.. _setup_dal_crypto_params:

Install DAL trusted setup
~~~~~~~~~~~~~~~~~~~~~~~~~

Users running :doc:`DAL<../shell/dal>` as :ref:`slot producers<dal_profiles>`
need to have a set of cryptographic parameters (known as an SRS) installed in
order to run their :doc:`DAL node<../shell/dal_node>`. The parameters can be
retrieved via the following script::

  scripts/install_dal_trusted_setup.sh


Get the sources
~~~~~~~~~~~~~~~

Octez ``git`` repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens on GitLab.

Checkout the ``latest-release`` branch to use the latest release.
Alternatively, you can checkout a specific version based on its tag.

Install Octez dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install the OCaml compiler and the libraries that Octez depends on::

   make build-deps

Alternatively, if you want to install extra
development packages such as ``merlin``, you may use the following
command instead:

::

   make build-dev-deps

.. note::

   * These commands create a local OPAM switch (``_opam`` folder at the root
     of the repository) where the required version of OCaml and OCaml Octez
     dependencies are compiled and installed (this takes a while but it's
     only done once).

   * Be sure to ``eval $(scripts/env.sh)`` when you ``cd``
     into the repository in order to be sure to load this local
     environment.

   * As the opam hook would overwrite the effects of ``eval $(scripts/env.sh)``
     the script will disable the opam hook temporarily.

   * OPAM is meant to handle correctly the OCaml libraries but it is
     not always able to handle all external C libraries we depend
     on. On most systems, it is able to suggest a call to the system
     package manager but it currently does not handle version checking.

   * As a last resort, removing the ``_opam`` folder (as part of a ``git
     clean -dxf`` for example) allows to restart in a fresh environment.

Compile
~~~~~~~

Once the dependencies are installed we can update OPAM's environment to
refer to the new switch and compile the project:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [compile sources]
  :end-before: [optional setup]

Lastly, you can also add the Octez binaries to your ``PATH`` variable,
and after reading the Disclaimer a few
hundred times you are allowed to disable it with
``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y``.

You may also activate Bash autocompletion by executing::

  source ./src/bin_client/bash-completion.sh

.. warning::

  Note that if your shell is ``zsh``, you may need extra configuration to customize shell
  completion (refer to the ``zsh`` documentation).

.. _update_from_sources:

Update
~~~~~~

For updating to a new version, you typically have to
update the sources by doing ``git pull`` in the ``tezos/`` directory and replay
the compilation scenario starting from ``make build-deps``.
You may also use ``make clean`` (and ``rm -Rf _opam/`` if needed) before that, for restarting compilation in a
fresh state.

Appendix
--------

.. toctree::
   :maxdepth: 2

   get_troubleshooting
