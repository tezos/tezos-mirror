===============================
Serokell → Nomadic Labs Migration
===============================

This guide explains how to migrate your Octez setup from Serokell to Nomadic Labs packages.

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

3. Install Nomadic Labs Packages
---------------------------------
Add the Nomadic Labs repository and install the ``octez-node`` package::

    export distribution=next/debian
    export release=bookworm
    wget -qO- https://example.com/install-bin-deb.sh | bash
    sudo apt install octez-node

When prompted, answer **Yes** to skip automatic configuration.

4. Relocate Data
----------------
Move your node and client data to the Nomadic Labs directory structure::

    sudo mkdir -p /var/tezos
    sudo cp -a /var/lib/tezos/<network> /var/tezos/.octez-node
    sudo cp -a /var/lib/tezos/.tezos-client /var/tezos/

5. Update User Configuration
----------------------------
Change the ``tezos`` user’s home directory and shell::

    sudo usermod -d /var/tezos tezos
    sudo chsh -s /bin/bash tezos

6. Start Node Service
---------------------
Enable and start the ``octez-node`` service::

    sudo systemctl enable octez-node
    sudo systemctl start octez-node

7. Migrate the Baker
--------------------
Install and enable the baker services::

    sudo apt install octez-baker
    sudo systemctl enable octez-baker-active
    sudo systemctl start octez-baker-active

Context and Explanations
========================

Purpose of Migration
--------------------
The migration transitions from Serokell packages, which use custom scripts and configurations, to Nomadic Labs packages, which adhere to Debian standards. This change simplifies configuration and improves maintainability but requires adapting your existing setup.

Backup Importance
-----------------
Serokell’s package removal does not delete data, but errors during migration may corrupt or misplace it. A verified backup ensures recoverability.

Package Philosophy
------------------
- **Serokell**: Custom scripts support multiple nodes and networks per machine. Data resides in ``/var/lib/tezos/<network>``.
- **Nomadic Labs**: Standardized approach supports one network per node, using ``/var/tezos`` for storage.

Understanding these differences helps adjust the procedure for edge cases, such as multi-network setups.

Relocating Data
---------------
Serokell stores node data in network-specific subdirectories, while Nomadic Labs uses ``.octez-node`` for all networks. Adjust your data migration to fit this structure::

    sudo cp -a /var/lib/tezos/<network> /var/tezos/.octez-node

Client data paths (``.tezos-client``) remain largely the same.

Reconfiguring Services
----------------------
Nomadic Labs packages use ``systemd`` and standard Debian tools (``debconf``) for configuration. Skipping automatic configuration during installation is recommended to ensure compatibility with your existing data.

Baker Configuration
-------------------
The ``octez-baker`` package manages two services:

- ``octez-baker-active``: For the current protocol.
- ``octez-baker-next``: For upcoming protocols.

These services are configured based on ``.tezos-client`` data. Use::

    sudo dpkg-reconfigure octez-baker

or manually edit configuration files in ``/etc/default`` to customize.
