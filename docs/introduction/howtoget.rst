.. _howtoget:

How to get Tezos
================

In this How To we explain how to get up-to-date binaries to run Tezos
for each network.  You can either use the docker images or install via
opam, which is easier, or build from sources like developers do.


Docker images
-------------

For every change committed in the Gitlab repository, docker images are
automatically generated and published on `DockerHub
<https://hub.docker.com/r/tezos/tezos/>`_. This provides a convenient
way to run an always up-to-date ``tezos-node``.  The script
``tezos-docker-manager.sh`` (formally known as ``alphanet.sh``) is
provided to help download the right image for each network and run a
simple node.  Its only requirement is a working installation of
`Docker <https://www.docker.com/>`__ and docker compose on a machine
with architecture **x86_64**.  Although we only officially support
Linux, the script has been tested with success in the past on
windows/mac/linux.

The same script can be used to run Mainnet, Carthagenet or Zeronet, it
suffices to rename it as it downloads a different image based on its
name.
For example, to run Carthagenet test network with the latest release::

    wget -O carthagenet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
    chmod +x carthagenet.sh

Alternatively, to run Mainnet::

    wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
    chmod +x mainnet.sh

In the following we assume you are running Carthagenet test network.
You are now one step away from a working node::

    ./carthagenet.sh start

This will download the right docker image for your chosen network, launch 3
docker containers running the node, the baker and the endorser. Keep in mind
that when a tezos node is launched, it needs to connect to new peers and
synchronize the chain. This can be *lengthy* on the first launch
considering the chain takes up several gigabytes of data. See
:ref:`how to use Tezos<howtouse>` for more details.

Every call to ``carthagenet.sh`` will check for updates of the node and
will fail if your node is not up-to-date. For updating the node, simply
run::

    ./carthagenet.sh restart

If you prefer to temporarily disable automatic updates, you just have to
set an environment variable::

    export TEZOS_ALPHANET_DO_NOT_PULL=yes

See ``./carthagenet.sh --help`` for more information about the
script. In particular see ``./carthagenet.sh client --help`` or the
:ref:`online manual<client_manual>` for more information about
the client. Every command to the ``tezos-client`` can be equivalently
executed using ``./carthagenet.sh client``. Similarly, ``tezos-admin-client``
can be executed using ``./carthagenet.sh admin-client``.

.. _build_from_sources:

Build from sources
------------------

Environment
~~~~~~~~~~~

Currently Tezos is being developed for Linux x86_64, mostly for
Debian/Ubuntu and Archlinux.

The following OSes are also reported to work:

- macOS/x86_64
- Linux/aarch64 (64 bits) (Raspberry Pi3, etc.)

A Windows port is feasible and might be developed in the future.

Additionally, the ``master`` branch requires the Rust compiler,
version 1.39.0, and the Cargo package manager to be installed. You can use
`rustup <https://github.com/rust-lang/rustup>`_ to install both.
Note that ``rustup`` can update your ``.profile`` to update your ``PATH``
environment variable, but this does not take effect until you restart
your desktop environment or window manager, so you may have to manually
update it for your current session::

    rustup set profile minimal
    rustup toolchain install 1.39.0
    rustup default 1.39.0
    source $HOME/.cargo/env


Install OPAM
~~~~~~~~~~~~

To compile Tezos, you need the `OPAM <https://opam.ocaml.org/>`__
package manager, at least version *2.0* that you can get by following the `install instructions <https://opam.ocaml.org/doc/Install.html>`__.

After the first install of OPAM, use ``opam init --bare`` to set it up
while avoiding to compile an OCaml compiler now as this will be done in
the next step.

Install via OPAM
~~~~~~~~~~~~~~~~

The latest release is available (as soon as possible after the
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
install tezos-endorser-006-PsCARTHA``, ... In that case, it is enough to install the system dependencies of this package only by running ``opam depext tezos-node`` for example instead of ``opam depext tezos``.

.. warning::

   Note that ``opam install tezos-client`` and ``opam install
   tezos-signer`` are "minimal" and do not install the support for
   Ledger Nano devices. To enable it, run ``opam install
   ledgerwallet-tezos`` in addition of installing the binaries. (The
   macro meta-package ``tezos`` installs ``ledgerwallet-tezos``.)

Updating via opam
~~~~~~~~~~~~~~~~~

Installation by opam is especially convenient for updating to newer
versions. Once some libraries/binaries are installed and new versions
released, you can update by:

::

   opam update
   opam depext
   opam upgrade

It is recommended to also run the command ``opam remove -a`` in order
to remove the dependencies installed automatically and not needed
anymore. Beware to not uninstall too much though.

Identified situations where it will be more tricky are

* When the OCaml compiler version requirement changes. In this case,
  be explicit about the "upgrade" and do ``opam upgrade --unlock-base
  ocaml.$new_version tezos``.

* When there are Rust dependencies involved. The way to go is still
  unclear.


Set up the development environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**TL;DR**: From a fresh Debian Buster x86_64, you typically want to do:

::

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


Get the sources
---------------

Tezos *git* repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens on GitLab.

Checkout the ``latest-release`` branch to use the latest release.
Alternatively, you can checkout a specific version from its tag.


Install Tezos dependencies
--------------------------

Install the OCaml compiler and the libraries which Tezos depends on::

   make build-deps

Alternatively, if you want to be able to install extra packages
(development packages such as `merlin`), you may use the following
command instead:

::

   make build-dev-deps

This command creates a local opam switch (``_opam`` folder at the ro
of the repository) where the right version of OCaml and OCaml tezos
dependencies are compiled and installed (this takes a while but it's
only done once).

.. note::

   * Be sure to ``eval $(opam env)`` when you ``cd``
     into the repository in order to be sure to load this local
     environment.

   * OPAM is meant to handle correctly the OCaml libraries but it is
     not always able to handle all external C libraries we depend
     on. On most system, it is able to suggest a call to the system
     package manager but it currently does not handle version check.

   * In last resort, removing the ``_opam`` folder (as part of a ``git
     clean -dxf`` for example) allows to restart in fresh environment.


Compile
-------

Once the dependencies are done we can update opam's environment to
refer to the new switch and compile the project::

   eval $(opam env)
   make

Lastly you can also add Tezos binaries to your ``PATH`` variable,
activate bash autocompletion and after reading the Disclaimer a few
hundred times you are allowed to disable it with
``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y``.
