.. _howtoget:

How to get Tezos
================

In this how-to we explain how to get up-to-date binaries to run Tezos
on any network (either on the mainnet or on one of the test networks).
Tezos consists of :ref:`several binaries <tezos_binaries>` (i.e., executable files), including: a client, a node, a baker, and an endorser.

There are several options for getting the binaries, depending on how you plan to use Tezos:

- :ref:`using docker images <using_docker_images>`.
  This is the easiest way to install the latest stable version, as a Docker
  container, on any OS supported by Docker.
- :ref:`getting static binaries <getting_static_binaries>`.
  This is the easiest way to get native binaries for the latest stable version,
  requiring no dependencies, under Linux.
- :ref:`installing binaries <installing_binaries>`.
  This is the easiest way to install native binaries for the latest stable version, together with their dependencies, using a package manager.
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

.. _using_docker_images:

Using Docker images
-------------------

For every change committed in the GitLab repository, Docker images are
automatically generated and published on `DockerHub
<https://hub.docker.com/r/tezos/tezos/>`_. This provides a convenient
way to run an always up-to-date ``tezos-node``.  The script
``tezos-docker-manager.sh`` (formally known as ``alphanet.sh``) is
provided to download the right image for each network and run a
simple node.  Its only requirement is a working installation of
`Docker <https://www.docker.com/>`__ (including both Docker Engine and Docker Compose) on a machine
with architecture **x86_64**.  Although we only officially support
Linux, the script has been tested with success in the past on
Windows, OS X, and Linux.

The same script can be used to run Tezos on Mainnet, on Delphinet, or on other network: it
suffices to rename it as it downloads a different image based on its
name.
For example, to run Tezos on the Delphinet test network with the latest release::

    wget -O delphinet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
    chmod +x delphinet.sh

Alternatively, to run on Mainnet::

    wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
    chmod +x mainnet.sh

In the following we assume you are running on the Delphinet test network.
You are now one step away from a working node::

    ./delphinet.sh start

This will download the right Docker image for your chosen network, launch 3
Docker containers running the node, the baker and the endorser. Keep in mind
that when a Tezos node is launched, it needs to connect to new peers and
synchronize the chain. This can be *lengthy* on the first launch
considering that the chain takes up several gigabytes of data. See
:ref:`how to use Tezos<howtouse>` for more details.

Every call to ``delphinet.sh`` will check for updates of the node and
will fail if your node is not up-to-date. For updating the node, simply
run::

    ./delphinet.sh restart

If you prefer to temporarily disable automatic updates, you just have to
set an environment variable::

    export TEZOS_ALPHANET_DO_NOT_PULL=yes

See ``./delphinet.sh --help`` for more information about the
script. In particular see ``./delphinet.sh client --help`` or the
:ref:`online manual<client_manual>` for more information about
the client. Every command to the ``tezos-client`` can be equivalently
executed by using ``./delphinet.sh client``, passing the needed arguments. Similarly, ``tezos-admin-client``
can be executed using ``./delphinet.sh admin-client``.

.. _getting_static_binaries:

Getting static binaries
-----------------------

You can get static Linux binaries from the
`latest release in the tezos-packaging repository <https://github.com/serokell/tezos-packaging/releases/latest>`__.

This repository provides static binaries for x86_64 and arm64 architectures. Since these binaries
are static, they can be used on any Linux distribution without any additional prerequisites.

.. _installing_binaries:

Installing binaries
-------------------

Depending on your operating system, you may install Tezos (dynamically-linked)
binaries and their dependencies using a package manager, as follows.

Ubuntu Launchpad PPA with Tezos packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you're using Ubuntu, you can install packages with Tezos binaries from the Launchpad PPA.
Currently it supports Focal and Bionic versions. In order to do that run the following commands:

::

   sudo add-apt-repository ppa:serokell/tezos && sudo apt-get update
   sudo apt-get install tezos-client
   sudo apt-get install tezos-node
   sudo apt-get install tezos-baker-007-psdelph1

Fedora Copr repository with Tezos packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you're using Fedora, you can install packages with Tezos binaries from the Copr repository.
Currently it supports Fedora 32 and 31. In order to do that run the following commands:

::

   dnf copr enable @Serokell/Tezos && dnf update
   dnf install tezos-client
   dnf install tezos-node
   dnf install tezos-baker-007-PsDELPH1

.. _build_from_sources:
.. _building_with_opam:

Building from sources via OPAM
------------------------------

The easiest way to build the binaries from the source code is to use the OPAM
source package manager for OCaml.

Environment
~~~~~~~~~~~

Currently Tezos is being developed for Linux x86_64, mostly for
Debian/Ubuntu and Arch Linux. The following OSes are also reported to
work: macOS (x86_64), Arch Linux ARM (aarch64), Debian Linux (buster),
Ubuntu Linux (focal). A Windows port is feasible and might be
developed in the future.

.. note::

    If you build the binaries by using the following instructions inside a
    Docker container, you have to give extended privileges to this container,
    by passing option ``--privileged`` to the ``docker run`` command.

Install OPAM
~~~~~~~~~~~~

First, you need to install the `OPAM <https://opam.ocaml.org/>`__
package manager, at least version 2.0, that you can get by following the `install instructions <https://opam.ocaml.org/doc/Install.html>`__.

After the first install of OPAM, use ``opam init --bare`` to set it up
while avoiding to compile an OCaml compiler now, as this will be done in
the next step.

Install Tezos OPAM packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The latest Tezos release is available (as soon as possible after the
release) directly as OPAM packages.

.. note::

   Every file related to OPAM is (by default) in ``$HOME/.opam`` which
   means that, first, OPAM installs are user specific and, second, you
   can get rid of everything by removing this directory (+ updating
   your rc files (``$HOME/.bashrc``, ``$HOME/.profile``,
   ``$HOME/.zshrc``, ``$HOME/.emacs``, ...) if you asked/allowed OPAM
   to add some lines in them).

The binaries need a specific version of the OCaml compiler (currently
4.09.1). To get an environment with it do:

::

   opam switch create for_tezos 4.09.1
   eval $(opam env)

.. note::

   The command ``eval $(opam env)`` sets up required environment
   variables. OPAM will suggest to add it in your rc file. If, at any
   point, you get an error like ``tezos-something: command not
   found``, first thing to try is to (re)run ``eval $(opam
   env --switch 4.09.1)`` to see if it fixes the problem.

In order to get the system dependencies of the binaries, do:

::

   opam install depext
   opam depext tezos

Now, install all the binaries by:

::

   opam install tezos

You can be more specific and only ``opam install tezos-node``, ``opam
install tezos-endorser-006-PsDelph1``, ... In that case, it is enough to install the system dependencies of this package only by running ``opam depext tezos-node`` for example instead of ``opam depext tezos``.

.. warning::

   Note that ``opam install tezos-client`` and ``opam install
   tezos-signer`` are "minimal" and do not install the support for
   Ledger Nano devices. To enable it, run ``opam install
   ledgerwallet-tezos`` in addition to installing the binaries. (The
   macro meta-package ``tezos`` installs ``ledgerwallet-tezos``.)

Updating via OPAM
~~~~~~~~~~~~~~~~~

Installation via OPAM is especially convenient for updating to newer
versions. Once some libraries/binaries are installed and new versions
released, you can update by:

::

   opam update
   opam depext
   opam upgrade

It is recommended to also run the command ``opam remove -a`` in order
to remove the dependencies installed automatically and not needed
anymore. Beware not uninstall too much though.

Identified situations where it will be more tricky are:

* When the OCaml compiler version requirement changes. In this case,
  be explicit about the "upgrade" and do ``opam upgrade --unlock-base
  ocaml.$new_version tezos``.

* When there are Rust dependencies involved. The way to go is still
  unclear.
  The solution will be defined when delivering the first release with Rust
  dependencies.

.. _compiling_with_make:

Setting up the development environment from scratch
---------------------------------------------------

If you plan to contribute to the Tezos codebase, the way to go is to set up a
complete development environment, by cloning the repository and compiling the
sources using the provided makefile.

**TL;DR**: From a fresh Debian Buster x86_64, you typically want to do:

.. code-block:: bash

   sudo apt install -y rsync git m4 build-essential patch unzip wget pkg-config libgmp-dev libev-dev libhidapi-dev libffi-dev opam jq
   git clone https://gitlab.com/tezos/tezos.git
   cd tezos
   git checkout latest-release
   opam init --bare
   make build-deps
   eval $(opam env)
   make
   export PATH=~/tezos:$PATH
   source ./src/bin_client/bash-completion.sh
   export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y

The following sections describe the individual steps above in more detail.

Get the sources
~~~~~~~~~~~~~~~

Tezos ``git`` repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens on GitLab.

Checkout the ``latest-release`` branch to use the latest release.
Alternatively, you can checkout a specific version based on its tag.


.. _setup_rust:

Install Rust
~~~~~~~~~~~~

For compiling pre-8.0 releases, you don't need Rust, so you can skip this
section.

Starting from version 8.0, compiling Tezos requires the Rust compiler,
version 1.44.0, and the Cargo package manager to be installed. You can use
`rustup <https://rustup.rs/>`_ to install both. If you do not have ``rustup``,
please avoid installing it from Snapcraft; you can rather follow the simple
installation process shown below::

   wget https://sh.rustup.rs/rustup-init.sh
   chmod +x rustup-init.sh
   ./rustup-init.sh --profile minimal --default-toolchain 1.44.0 -y

Once Rust is installed, note that your ``PATH`` environment variable
(in ``.profile``) may be updated and you will need to restart your session
so that changes can be taken into account. Alternatively, you can do it
manually without restarting your session::

   source $HOME/.cargo/env


Install Tezos dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install the OCaml compiler and the libraries that Tezos depends on::

   make build-deps

Alternatively, if you want to to install extra
development packages such as ``merlin``, you may use the following
command instead:

::

   make build-dev-deps

.. note::

   * These commands create a local OPAM switch (``_opam`` folder at the root
     of the repository) where the required version of OCaml and OCaml Tezos
     dependencies are compiled and installed (this takes a while but it's
     only done once).

   * Be sure to ``eval $(opam env)`` when you ``cd``
     into the repository in order to be sure to load this local
     environment.

   * OPAM is meant to handle correctly the OCaml libraries but it is
     not always able to handle all external C libraries we depend
     on. On most systems, it is able to suggest a call to the system
     package manager but it currently does not handle version checking.

   * As a last resort, removing the ``_opam`` folder (as part of a ``git
     clean -dxf`` for example) allows to restart in a fresh environment.


Compile
~~~~~~~

Once the dependencies are installed we can update OPAM's environment to
refer to the new switch and compile the project::

   eval $(opam env)
   make

Lastly you can also add the Tezos binaries to your ``PATH`` variable,
and after reading the Disclaimer a few
hundred times you are allowed to disable it with
``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y``.

You may also activate Bash autocompletion by executing::

  source ./src/bin_client/bash-completion.sh

.. warning::

  Note that if your shell is `zsh`, you may need extra configuration to customize shell
  completion (refer to the `zsh` documentation).
