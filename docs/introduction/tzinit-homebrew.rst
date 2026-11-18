===================================================================
Migrating from the Nomadic Labs Homebrew formula to Tzinit formulae
===================================================================

Octez v25.3 deprecates the :ref:`Homebrew formula published by Nomadic Labs
<using_homebrew>`, which compiles Octez on your machine. It is scheduled for
removal in Octez v26. Homebrew formulae built from the same sources, called *Tzinit
formulae*, are available instead: they install prebuilt Octez binaries for
macOS, with services managed by ``brew services``. They are published in the
Homebrew tap ``drchrispinnock/octez``, whose sources are at
https://github.com/drchrispinnock/homebrew-octez.

This page explains how to migrate an installation of the Nomadic Labs formula
for any Octez v25.x version (abbreviated as *NL formula* hereafter) to the
Tzinit formulae.

Chain data and wallets are left untouched: the Tzinit formulae use the same
default directories as the NL formula.

Supported platforms
===================

The Tzinit formulae are available for macOS 14 (Sonoma) and later, on both
Apple Silicon (``arm64``) and Intel (``x86_64``) Macs. However, the
``octez-smart-rollup-node`` formula is only available on Apple Silicon.

On Intel Macs, Homebrew no longer provides prebuilt libraries, so it compiles
the libraries that Octez depends on (such as ``gmp``, ``zstd`` and
``openssl@3``). The first installation can thus take a while. Only the Octez
binaries are prebuilt.

Contrary to the NL formula, the Tzinit formulae cannot be used on Linux. On
Linux, see :doc:`tzinit-packages` or :doc:`howtoget` for other installation
methods.

Differences with the NL formula
===============================

The NL formula is a single formula, ``octez``, installed from a local tap
(``octez-user/octeztap`` if you followed :ref:`using_homebrew`). It compiles
Octez on your machine and only installs executables.

The Tzinit tap splits Octez into several formulae:

.. list-table::
   :header-rows: 1

   * - Formula
     - Contents
     - Service
   * - ``octez-client``
     - ``octez-client``, ``octez-admin-client``, ``octez-codec``
     -
   * - ``octez-node``
     - ``octez-node``
     - yes
   * - ``octez-baker``
     - ``octez-baker``, ``octez-accuser``
     - yes
   * - ``octez-accuser``
     - service only (depends on ``octez-baker``)
     - yes
   * - ``octez-vdf``
     - service only (depends on ``octez-baker``)
     - yes
   * - ``octez-dal-node``
     - ``octez-dal-node``
     - yes
   * - ``octez-signer``
     - ``octez-signer``
     - yes
   * - ``octez-smart-rollup-node``
     - ``octez-smart-rollup-node``
     - yes
   * - ``octez-evm-node``
     - ``octez-evm-node``
     - yes
   * - ``zcash-params``
     - Sapling parameters (installed with ``octez-node`` and ``octez-baker``)
     -
   * - ``dal-trusted-setup``
     - DAL trusted setup (installed with ``octez-dal-node``)
     -

The main differences are:

- The protocol-specific executables ``octez-baker-<protocol>`` and
  ``octez-accuser-<protocol>`` installed by the NL formula are not provided.
  Use ``octez-baker`` and ``octez-accuser`` instead, which accept the same
  commands and switch protocols automatically.
- The Tzinit formulae provide ``octez-dal-node``, ``octez-admin-client`` and
  ``octez-codec``, which the NL formula did not install.
- The Tzinit formulae install the Sapling parameters and the DAL trusted
  setup, so you do not need to download them yourself.
- The Tzinit formulae provide services, managed with ``brew services``, and
  configured with files in ``$(brew --prefix)/etc/octez``.

Step-by-step procedure
======================

1. Back up your wallet
----------------------

The procedure does not modify your data, but it is good practice to save your
wallet (including the baker's high watermarks and nonces) before proceeding:

.. code:: shell

   tar czf ~/octez-migration-backup.tgz -C ~ .tezos-client

2. Stop all Octez daemons
-------------------------

Stop the Octez daemons (node, baker, accuser, signer...) that run from the NL
formula, however you started them (from a terminal, with ``launchd``...).
Check that none of them is still running:

.. code:: shell

   pgrep -fl octez-

3. Uninstall the NL formula
---------------------------

The Tzinit formulae install executables with the same names as the NL formula,
so Homebrew cannot install them while the NL formula is installed. Uninstall
it, and remove the local tap that contains it:

.. code:: shell

   brew uninstall octez
   brew untap octez-user/octeztap

If you used another name for the local tap, ``brew info octez`` shows it on
its first line (for instance ``==> octez-user/octeztap/octez: stable 25.3``).

Uninstalling the formula only removes the executables: it does not remove the
node data directory nor the wallet.

4. Install the Tzinit formulae
------------------------------

Add the tap and allow Homebrew to install formulae from it:

.. code:: shell

   brew tap drchrispinnock/octez
   brew trust drchrispinnock/octez

Then install the formulae that you need, for instance, to run a node and a
baker:

.. code:: shell

   brew install octez-client octez-node octez-baker octez-accuser

Add ``octez-dal-node`` if you run a DAL node, and ``octez-signer`` if you run
a signer.

Check that the executables in your ``PATH`` now come from the Tzinit formulae:

.. code:: shell

   which octez-node
   octez-node --version

The path should be in ``$(brew --prefix)/bin`` (that is, ``/opt/homebrew/bin``
on Apple Silicon and ``/usr/local/bin`` on Intel), and the version should be
25.3.

5. Configure the services
-------------------------

Skip this step if you prefer to keep running the Octez daemons yourself, as you
did with the NL formula. The executables are at the same place
(``$(brew --prefix)/bin``), so your scripts and ``launchd`` jobs keep working,
unless they referred to executables in ``$(brew --prefix)/Cellar/octez``, or to
the protocol-specific executables ``octez-baker-<protocol>`` and
``octez-accuser-<protocol>``.

Each service reads its configuration from
``$(brew --prefix)/etc/octez/<formula>.conf``. These files are shell scripts
that set variables.

Node
~~~~

The ``octez-node`` service runs ``octez-node run`` on the data directory given
by ``DATADIR`` (``~/.tezos-node`` by default). Other options, if any, go in
``RUNTIME_OPTS``. If you passed options such as ``--rpc-addr`` on the command
line rather than in the node configuration file (``config.json`` in the
data directory), either put them in ``RUNTIME_OPTS`` in
``$(brew --prefix)/etc/octez/octez-node.conf``, or add them to the node
configuration file using ``octez-node config update``.

The baker, the accuser and the DAL node connect to the RPC server of the node,
by default at ``http://127.0.0.1:8732``. Make sure that the node configuration
enables it, for instance with:

.. code:: shell

   octez-node config update --rpc-addr 127.0.0.1:8732

Baker
~~~~~

Edit ``$(brew --prefix)/etc/octez/octez-baker.conf`` as follows:

- Set ``LQ_VOTE`` to ``on``, ``off`` or ``pass``. This is **mandatory**: the
  baker does not start until it is set. It corresponds to option
  ``--liquidity-baking-toggle-vote``.
- Set ``KEYS`` to the aliases of the keys to bake with, for instance
  ``KEYS="mybaker"``. If ``KEYS`` is empty, the baker bakes for all the keys of
  the wallet.
- If you run a DAL node (which is recommended), set
  ``DAL_OPTIONS="--dal-node http://127.0.0.1:10732"``.
  The default is ``--without-dal``.
- If your node data directory is not ``~/.tezos-node``, set ``DATADIR``.
  If your wallet is not in ``~/.tezos-client``, set ``CLIENTDIR``.
- Other options go in ``RUNTIME_OPTS``.

By default (``MODE="local"``), the service runs
``octez-baker run with local node``, which reads the data directory of the
node. To use a node on another machine, set ``MODE="remote"`` and set
``ENDPOINT`` to the RPC endpoint of this node.

Accuser
~~~~~~~

The ``octez-accuser`` service connects to the node at ``ENDPOINT``
(``http://127.0.0.1:8732`` by default), set in
``$(brew --prefix)/etc/octez/octez-accuser.conf``.

DAL node
~~~~~~~~

The ``octez-dal-node`` service runs ``octez-dal-node run`` on the data
directory given by ``DATADIR`` (``~/.tezos-dal-node`` by default), with the
options of ``RUNTIME_OPTS``. If you have not configured the DAL node yet, do it
before starting the service, for instance with:

.. code:: shell

   octez-dal-node config init --endpoint http://127.0.0.1:8732

Signer
~~~~~~

The ``octez-signer`` service runs an HTTP signer listening on
``127.0.0.1:6732``, with keys in ``~/.tezos-signer``. If you run a signer in
another way (for instance over TCP or a Unix socket, see :doc:`services`), do
not start this service and keep running the signer yourself.

Otherwise, edit ``$(brew --prefix)/etc/octez/octez-signer.conf`` to match
the options you used. In particular, the service does not check high watermarks
and does not restrict the kinds of operations it signs by default: consider
setting ``HWM_CHECK="-W"`` and ``MAGIC_BYTES="-M 0x11,0x12,0x13"`` for a signer
used only for baking.

6. Start the services
---------------------

.. note::

   The commands in this section are basic examples of how to manage the
   services with ``brew services``. They assume that the node, the DAL node and
   the baker are already set up, which is the case if you are migrating an
   existing installation. To set them up from scratch, see for instance the
   instructions for :ref:`running a node <quickstart_node>` and
   :ref:`running a baker <quickstart_baker>`.

.. code:: shell

   brew services start octez-node

If you run a DAL node:

.. code:: shell

   brew services start octez-dal-node

If you run a baker:

.. code:: shell

   brew services start octez-baker
   brew services start octez-accuser

Check that the services are running:

.. code:: shell

   brew services list

``brew services start`` runs a service as the current user, and starts it again
when this user logs in. To run a service at boot, even when nobody is logged
in, you can run it as a dedicated user, for instance:

.. code:: shell

   sudo brew services start --sudo-service-user tezos octez-node

In this case, the default directories (such as ``~/.tezos-node``) are relative
to the home directory of this user, so you need to move your data there or to
set ``DATADIR`` and ``CLIENTDIR``.

A Mac that goes to sleep stops baking and misses its baking and attestation
rights. See ``man pmset`` and ``man caffeinate`` to prevent this.

7. Check the logs
-----------------

The detailed logs of the services are written by Octez itself, and kept for
7 days:

- in ``~/.tezos-node/daily_logs`` for the node, and
  ``~/.tezos-dal-node/daily_logs`` for the DAL node (if you changed
  ``DATADIR``, in subdirectory ``daily_logs`` of this directory);
- in ``~/.tezos-client/logs/octez-baker`` and
  ``~/.tezos-client/logs/octez-accuser`` for the baker and the accuser.

For instance, check that the node is running with:

.. code:: shell

   tail -f ~/.tezos-node/daily_logs/daily-$(date +%Y%m%d).log

The services also write the output of the Octez daemons in
``$(brew --prefix)/var/log/octez/<formula>.log``. These files are not rotated,
which is why they only contain warnings and errors by default (variable
``LOG_RULES`` in the configuration file of the service, which sets
``TEZOS_LOG``). In practice, they contain few lines, but they are useful to
investigate services that fail to start.

Upgrading
=========

New versions of the Tzinit formulae are installed with:

.. code:: shell

   brew update
   brew upgrade

Then restart the services that are running, for instance:

.. code:: shell

   brew services restart octez-node

When you have modified a configuration file in ``$(brew --prefix)/etc/octez``,
Homebrew does not overwrite it on upgrade, and installs the new default
version next to it with suffix ``.default``.
