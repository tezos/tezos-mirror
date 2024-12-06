===========================================
Serokell to Nomadic Labs packages Migration
===========================================

This guide explains how to migrate your Octez setup from the legacy Serokell packages to the Nomadic Labs packages (abbreviated as NL packages hereafter).

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

3. Reconfigure ``dpkg``
-----------------------

**If using Debian**, reconfigure ``dpkg``:

- if needed, install the Dialog interface with ``sudo apt install dialog``
- reconfigure debconf to force it to ask few interactive questions with ``sudo dpkg-reconfigure debconf``, by setting the priority to *medium*. The priority decides when debconf should ask configuration questions to the user, or rather use the maintainer default values.


4. Add NL Repository
--------------------
**If using Debian**, add the NL package repository and install the ``octez-node`` package:

.. code:: shell

   export distribution=next/debian
   export release=bookworm

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [add repository]
   :end-before: [end add repository]

5. Install NL Packages
----------------------
**If using Debian packages**, install them so::

    sudo apt install octez-node

When prompted, answer **Yes** to skip automatic configuration.

**If using RPM packages** rather then Debian ones, refer to :ref:`installing_rpm`.

6. Relocate Data
----------------
Copy your node and client data to the NL directory structure::

    sudo mkdir -p /var/tezos
    sudo cp -a /var/lib/tezos/<network> /var/tezos/.tezos-node
    sudo cp -a /var/lib/tezos/.tezos-client /var/tezos/

where ``<network>`` is the network you choose for running Octez.

7. Update User Configuration
----------------------------
Change the ``tezos`` user’s home directory and shell::

    sudo usermod -d /var/tezos tezos
    sudo chsh -s /bin/bash tezos

8. Start Node Service
---------------------
Enable and start the ``octez-node`` service::

    sudo systemctl enable octez-node
    sudo systemctl start octez-node

You may then inspect its trace with::

    sudo tail -f /var/log/tezos/node.log

9. Migrate the Baker
--------------------
Install the baker::

    sudo apt install octez-baker

If you want to install RPM packages rather then Debian ones, refer to :ref:`installing_rpm`.

Then enable and start the baking service::

    sudo systemctl enable octez-baker-active
    sudo systemctl start octez-baker-active

You may then inspect its trace with::

    sudo tail -f /var/log/tezos/baker-active.log

Context and Explanations
========================

The migration transitions from Serokell packages, which use custom scripts and configurations, to NL packages, which adhere to standard tools such as ``systemd``. This change simplifies configuration and improves maintainability but requires adapting your existing setup.

Understanding the differences between the legacy packages and the new packages helps adjusting the procedure for edge cases, such as multi-network setups.

Backup Importance
-----------------
Serokell’s package removal does not delete data, but errors during migration may corrupt or misplace it. A verified backup ensures recoverability.

Relocating Data
---------------
Serokell stores node data in network-specific subdirectories, while NL uses ``.octez-node`` for all networks:

- **Serokell**: Custom scripts support multiple nodes and networks per machine. Data resides in ``/var/lib/tezos/<network>``.
- **NL**: Standardized approach supports one network per node, using ``/var/tezos`` for storage.

Adjust your data migration to fit this structure::

    sudo cp -a /var/lib/tezos/<network> /var/tezos/.octez-node

Also copy the client data (which is presumably not network-dependent).

Reconfiguring ``dpkg``
----------------------
NL Debian packages use standard tools (``debconf``) for configuration. Skipping automatic configuration during installation is recommended to reuse your existing data.
For ensuring that the user has a chance to disable automatic configuration, we have to reconfigure the package manager and set the priority level.

Baker Configuration
-------------------

The installation procedure of ``octez-baker`` will ask a few questions.
Since the configuration of the baker keys lives in the Octez client data, we don't have to migrate these, but check that the keys are still relevant.

The ``octez-baker`` package manages two services:

- ``octez-baker-active``: For the current protocol.
- ``octez-baker-next``: For the upcoming protocol.

The protocol associated with these two services can be changed by reconfiguring the package::

    sudo dpkg-reconfigure octez-baker

or manually editing the files in ``/etc/default/octez-baker*``.
