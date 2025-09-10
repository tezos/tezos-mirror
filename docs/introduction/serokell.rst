===================================================
Migrating from Serokell's to Nomadic Labs' packages
===================================================

This guide explains how to migrate your Octez setup from the legacy packages provided by Serokell to the packages provided by Nomadic Labs (abbreviated as NL packages hereafter).

Why Backups Are Crucial
=======================
The migration involves removing old packages, relocating data, and reconfiguring services. Mistakes during this process can result in irreversible data loss. **Always back up your node and client data before proceeding.**

How This Guide is Structured
=============================
1. **Step-by-Step Procedure**: A simple guide for users with standard setups. Follow this if you just want instructions.
2. **Context and Explanations**: Additional details to understand the migration process or adapt it to customized setups.

Step-by-Step Procedure
======================

1. Backup Data
--------------
Save a complete backup of your node and client data::

    sudo cp -a /var/lib/tezos /backup/tezos-data

2. Remove Serokell Packages
---------------------------
Uninstall Serokell packages while preserving data::

    sudo apt autoremove tezos-client tezos-node tezos-baking

If you were using RPM packages, use ``dnf`` instead of ``apt``.

3. Add NL Repository
--------------------
**If using Debian** (see other :ref:`supported platforms <installing_deb>`), add the NL package repository and install the ``octez-node`` package:

.. code:: shell

   export distribution=debian
   export release=bookworm

   sudo apt-get install -y gpg curl

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [add repository]
   :end-before: [end add repository]

4. Install NL Packages
----------------------

**If using Debian packages**, install them so::

    sudo apt install octez-node

When prompted, answer **Yes** to skip automatic configuration.

If needed, install the Dialog interface with ``sudo apt install dialog``.

**If using RPM packages** rather then Debian ones, refer to :ref:`installing_rpm`.

5. Relocate Data
----------------
The default data directory for NL packages is ``/var/tezos`` . To use a different
location, change the value of the ``DATADIR`` variable in ``/etc/default/octez-node``,
and relocate the home of the ``tezos`` user using the command ``usermod -m -d
/custom tezos``. We recommend to keep the default.

You can copy your node and client data to the new directory structure::

    sudo mkdir -p /var/tezos
    sudo cp -a /var/lib/tezos/<network> /var/tezos/.tezos-node
    sudo cp -a /var/lib/tezos/.tezos-client /var/tezos/

where ``<network>`` is the network you choose for running Octez.

6. Update User Configuration
----------------------------
Change the ``tezos`` user’s home directory and shell::

    sudo usermod -d /var/tezos tezos
    sudo chsh -s /bin/bash tezos

7. Start Node Service
---------------------
First, check the configuration file inherited from the old packages::

    sudo -u tezos octez-node config show

If the RPC port is not defined (no field ``listen-addrs``), open it, e.g.::

    sudo -u tezos octez-node config update --rpc-addr=127.0.0.1:8732

Now enable and start the ``octez-node`` service::

    sudo systemctl enable octez-node
    sudo systemctl start octez-node

You may then inspect its trace with::

    sudo tail -f /var/log/tezos/node.log

8. Migrate the Baker
--------------------
Install the baker::

    sudo apt install octez-baker

During installation, depending on the interaction settings, the configuration dialog may offer you the choice between running the unique baker executable or several protocol-suffixed executables.

If you want to install RPM packages rather then Debian ones, refer to :ref:`installing_rpm`.

Then enable and start the baking service::

    sudo systemctl enable octez-baker
    sudo systemctl start octez-baker

You may then inspect its trace with::

    sudo tail -f /var/log/tezos/baker.log

If you configured the package to use several protocol-suffixed binaries, the log names would bear the protocol suffixes (``baker-<PROTO>.log``).

Context and Explanations
========================

The migration transitions from Serokell packages, which use custom scripts and configurations, to NL packages, which adhere to standard tools such as ``systemd``. This change simplifies configuration and improves maintainability but requires adapting your existing setup.

Understanding the differences between the legacy packages and the new packages helps adjusting the procedure for edge cases, such as multi-network setups.

Relocating Data
---------------
Serokell stores node data in network-specific subdirectories, while NL uses ``.octez-node`` for all networks:

- **Serokell**: Custom scripts support multiple nodes and networks per machine. Data resides in ``/var/lib/tezos/<network>``.
- **NL**: The unique directory for node data ``/var/tezos`` allows to run the node only on a single network at a time.

Copy your node data into this new structure.

Also copy the client data (which is presumably not network-dependent).

Baker Configuration
-------------------

The installation procedure of ``octez-baker`` will ask a few questions.
Since the configuration of the baker keys lives in the Octez client data, we don't have to migrate these, but check that the keys are still relevant.

The baker binary or binaries ran by the ``octez-baker`` service for the active and next protocols can be changed by reconfiguring the package::

    sudo dpkg-reconfigure octez-baker

or manually editing the file ``/etc/default/octez-baker``.

After the migration is complete, if you are using Debian packages consider removing all leftover configuration files from the Serokell packages, by doing ``apt purge tezos-client tezos-node tezos-baking``.
