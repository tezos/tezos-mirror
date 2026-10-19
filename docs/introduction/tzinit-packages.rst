=======================================================
Migrating from Nomadic Labs packages to Tzinit packages
=======================================================

Starting from Octez version 26, Nomadic Labs no longer publishes Debian and
Ubuntu packages for Octez. Packages built along the same lines, called *Tzinit
packages*, are available in an APT repository at ``https://apt.tzinit.org``.
Individual package files (including RPM packages) are also available at
https://packages.tzinit.org/. These packages are documented at
https://chrispinnock.com/tezos/packages/.

This page explains how to migrate an installation of the Nomadic Labs packages
for Octez v25.2 (abbreviated as *NL packages* hereafter) to the Tzinit
packages.

The migration is an in-place upgrade: chain data, wallets and baker state are
left untouched. However, the configuration of the baker changes, and some
services must be re-enabled manually.

.. warning::

   Do **not** uninstall or purge the NL packages to switch to the Tzinit
   packages. Purging the ``octez-node`` NL package can delete the node's data
   directory, and purging the ``octez-baker`` NL package deletes the baker's
   high watermarks and nonces.

Supported platforms
===================

The Tzinit packages are available for:

- Debian 13 (``trixie``),
- Ubuntu 24.04 (``noble``),

both on ``amd64`` and ``arm64`` architectures.
If you are running another distribution, there is no direct upgrade path using
``apt``; see :doc:`howtoget` for other installation methods.

Step-by-step procedure
======================

1. Back up your configuration and wallet
----------------------------------------

The procedure does not modify your data, but it is good practice to save your
configuration files and your wallet (including the baker's high watermarks and
nonces) before proceeding:

.. code:: shell

   sudo tar czf ~/octez-migration-backup.tgz /etc/default/octez-* /var/tezos/.tezos-client

2. Stop all Octez services
--------------------------

.. code:: shell

   sudo systemctl stop octez-smart-rollup-node octez-baker octez-dal-node octez-node

Stopping ``octez-baker`` also stops the bakers and accusers that it started.
Check that no Octez service is still running:

.. code:: shell

   systemctl list-units 'octez*'

Services may be listed as ``failed`` instead of being absent from the list:
this is expected, as Octez daemons exit with a non-zero status when they are
stopped.

3. Replace the APT repository
-----------------------------

Remove the NL repository:

.. code:: shell

   sudo rm /etc/apt/sources.list.d/octez.list

Add the signing key of the Tzinit repository:

.. code:: shell

   sudo apt-get install -y gpg curl
   curl -s https://apt.tzinit.org/keys/tzinit.asc |
     sudo gpg --dearmor -o /etc/apt/keyrings/tzinit.gpg

Check that the fingerprint of the key is
``181E 6294 A077 0AE9 AF01 73B2 1F68 8E12 01D8 19B4``:

.. code:: shell

   gpg --show-keys /etc/apt/keyrings/tzinit.gpg

Then add the Tzinit repository. On Debian 13:

.. code:: shell

   echo "deb [signed-by=/etc/apt/keyrings/tzinit.gpg] https://apt.tzinit.org/debian trixie main" |
     sudo tee /etc/apt/sources.list.d/tzinit-octez.list

On Ubuntu 24.04:

.. code:: shell

   echo "deb [signed-by=/etc/apt/keyrings/tzinit.gpg] https://apt.tzinit.org/ubuntu noble main" |
     sudo tee /etc/apt/sources.list.d/tzinit-octez.list

4. Upgrade the packages
-----------------------

Upgrade the packages while keeping your current configuration files:

.. code:: shell

   sudo apt-get update
   sudo apt-get -o Dpkg::Options::=--force-confold full-upgrade

Use ``full-upgrade`` rather than ``upgrade``: the NL packages depend on each
other's exact versions, and ``upgrade`` may keep some of them back (for
instance ``octez-node`` when ``octez-smart-rollup-node`` is installed). Before
confirming, check that ``apt-get`` does not plan to remove any package.

When a configuration file that you modified is kept, the default version
shipped by the Tzinit package is saved next to it, with suffix ``.dpkg-dist``.

.. warning::

   Files ``/etc/default/octez-accuser`` and ``/etc/default/octez-vdf`` are
   overwritten by the Tzinit ``octez-baker`` package, without keeping any
   previous version. If you created ``/etc/default/octez-accuser`` yourself,
   restore its contents from your backup and check it after each upgrade.

Check that all Octez packages now come from the Tzinit repository:

.. code:: shell

   dpkg -l 'octez*'

All packages should be at version ``25.2-10`` or later, except
``octez-smart-rollup-node`` which uses a date-based version such as
``20260724-10``.

The NL ``octez-baker`` package recommended ``octez-signer``, but the Tzinit one
does not. As a consequence, ``apt`` may now report ``octez-signer`` as
"automatically installed and no longer required". If you use the signer,
prevent ``apt autoremove`` from removing it:

.. code:: shell

   sudo apt-mark manual octez-signer

5. Adapt the baker configuration
--------------------------------

In the NL packages, service ``octez-baker`` started the bakers and accusers, as
well as the DAL node if ``--dal-node`` was configured. In the Tzinit packages,
service ``octez-baker`` only runs the baker; the accuser and the DAL node are
separate services.

The variables of ``/etc/default/octez-baker`` have also changed. Edit this file
(using ``/etc/default/octez-baker.dpkg-dist`` as a template, if it exists) as
follows:

.. list-table::
   :header-rows: 1

   * - NL packages
     - Tzinit packages
   * - ``LQVOTE="pass"``
     - ``LQ_VOTE="pass"`` (**mandatory**: the baker does not start until it is set)
   * - ``BAKER_KEY="mybaker"``
     - ``KEYS="mybaker"``
   * - ``RUNTIME_OPTS="--dal-node http://127.0.0.1:10732"``
     - ``DAL_OPTIONS="--dal-node http://127.0.0.1:10732"``, and remove ``--dal-node`` from ``RUNTIME_OPTS``
   * - no DAL node
     - ``DAL_OPTIONS="--without-dal"`` (the default)
   * - ``AGNOSTIC_BAKER=...``
     - remove (the protocol-agnostic baker is always used)

The Tzinit baker does not read ``/etc/default/octez-node``. If you changed
``DATADIR`` in ``/etc/default/octez-node``, also set ``DATADIR`` in
``/etc/default/octez-baker``. Similarly, if the base directory of
``octez-client`` (option ``--base-dir``) is not ``/var/tezos/.tezos-client``,
set ``CLIENTDIR``.

The accuser is configured separately, in ``/etc/default/octez-accuser``.

6. Adapt the smart rollup node configuration
--------------------------------------------

Skip this step if you do not run a smart rollup node.

The ``octez-smart-rollup-node`` service of the Tzinit packages does not read
file ``/etc/default/octez-smart-rollup-node``. To make it read this file, add
the following override:

.. code:: shell

   sudo mkdir -p /etc/systemd/system/octez-smart-rollup-node.service.d
   printf '[Service]\nEnvironmentFile=-/etc/default/octez-smart-rollup-node\n' |
     sudo tee /etc/systemd/system/octez-smart-rollup-node.service.d/override.conf

The Tzinit service already passes the data directory to the smart rollup node.
Do not put ``--data-dir`` in ``RUNTIME_OPTS``: set ``DATADIR`` instead.

7. Start the services
---------------------

.. code:: shell

   sudo systemctl daemon-reload
   sudo systemctl enable --now octez-node

If you run a DAL node:

.. code:: shell

   sudo systemctl enable --now octez-dal-node

If you run a baker:

.. code:: shell

   sudo systemctl enable --now octez-baker octez-accuser

If you run a smart rollup node:

.. code:: shell

   sudo systemctl enable --now octez-smart-rollup-node

8. Check the logs
-----------------

The services of the Tzinit packages write their logs in ``/var/log/tezos``, but
some file names have changed:

.. list-table::
   :header-rows: 1

   * - Service
     - NL packages
     - Tzinit packages
   * - ``octez-node``
     - ``node.log``
     - ``octez-node.log``
   * - ``octez-baker``
     - ``baker-<protocol>.log`` or ``baker.log``
     - ``octez-baker.log``
   * - ``octez-accuser``
     - ``accuser-<protocol>.log`` or ``accuser.log``
     - ``octez-accuser.log``
   * - ``octez-dal-node``
     - ``octez-dal-node.log``
     - ``octez-dal-node.log`` (unchanged)
   * - ``octez-smart-rollup-node``
     - ``smart-rollup-node.log``
     - ``octez-smart-rollup.log``

For instance, check that the node is running with:

.. code:: shell

   tail -f /var/log/tezos/octez-node.log

Once all services run correctly, you may delete the old log files.

Context and explanations
========================

Data
----

Both the NL and Tzinit packages run the Octez daemons as user ``tezos``, whose
home directory is ``/var/tezos``. The Tzinit packages reuse the existing
``tezos`` user and its home directory. As a consequence, all data stays in
place:

- the node data directory, ``/var/tezos/.tezos-node`` by default (variable
  ``DATADIR`` of ``/etc/default/octez-node``);
- the wallet and the baker state (high watermarks, nonces),
  ``/var/tezos/.tezos-client``;
- the DAL node data directory and configuration,
  ``/var/tezos/.tezos-dal-node``;
- the smart rollup node data directory,
  ``/var/tezos/.tezos-smart-rollup-node``.

The ``octez-node`` binary of the Tzinit packages for v25.2 is built from the
same commit as the NL one, so the node data needs no conversion. The Tzinit
``octez-smart-rollup-node`` package ships a standalone release of the smart
rollup node instead of the one from Octez v25.2, but both use the same storage
format.

Configuration files
-------------------

Except for ``/etc/default/octez-accuser`` and ``/etc/default/octez-vdf``, files
``/etc/default/octez-*`` are configuration files: when you have modified one of
them, the upgrade does not overwrite it (thanks to option
``--force-confold``). Files ``/etc/default/octez-node`` and
``/etc/default/octez-dal-node`` use the same variables in the NL and Tzinit
packages. File ``/etc/default/octez-baker`` does not, hence step 5.

Services
--------

The Tzinit packages ship systemd services with the same names as the NL packages
for ``octez-node``, ``octez-baker``, ``octez-dal-node`` and
``octez-smart-rollup-node``; services that were enabled remain enabled.
The per-protocol services ``octez-baker@<protocol>`` and
``octez-accuser@<protocol>`` of the NL packages, as well as
``octez-agnostic-baker`` and ``octez-agnostic-accuser``, are replaced by
``octez-baker`` and ``octez-accuser``.

The upgrade does not restart the services, which is why they are stopped
before the upgrade and started again afterwards.

The Tzinit packages also provide services that were not present in the NL
packages: ``octez-baker-remote`` (a baker using a node on another machine),
``octez-vdf``, ``octez-signer``, as well as packages ``octez-evm-node`` and
``octez-teztale-archiver``. See https://chrispinnock.com/tezos/packages/ for
details.

Signer
------

The NL packages did not provide a system-wide service for ``octez-signer``.
If you followed :doc:`services`, you run the signer as a systemd *user*
service, defined in ``~/.config/systemd/user/octez-signer.service``. This
service keeps working after the migration.

The Tzinit ``octez-signer`` package provides a system-wide ``octez-signer``
service running as a dedicated user ``tzsigner``, with its own keys in
``/var/tzsigner/.tezos-signer``, and listening over HTTP. This service is not
enabled by default; do not enable it in addition to your existing signer.
