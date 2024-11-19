=========================================
Serokell -> Nomadic-lab package migration
=========================================

This page explains the process for migrating your Octez installation and setup
from the legacy Serokell packages to the Nomadic-Labs Debian packages.

TL;DR
=====


The typical key steps for migration are:

- Remove Serokell Packages: Use ``sudo apt autoremove tezos-baking`` to remove
  Serokell-specific packages while retaining the data and configuration files.

- Install Nomadic Labs Packages: Add the Nomadic Labs repository and install
  the **octez-node** package. Reconfigure the system to ensure minimal interaction
  during installation.

- Data Backup and Relocation: Move the existing node data from
  ``/var/lib/tezos/<network>`` to ``/var/tezos/.octez-node`` and update the
  user directories for a smooth transition.

- Configure and Start Services: Enable and start the **octez-node** service,
  ensuring the node is properly configured and running.

- Migrate Baker Configuration: Install the **octez-baker** package, update baker
  keys, and enable the baker services (octez-baker-active and octez-baker-next).

Details of Serokell packages installation
=========================================

The Octez node installed by the legacy Serokell keeps data under
``/var/lib/tezos/<network>``.

Serokell packages use a configuration file and a service file for each network
under ``/etc/default``.

The service file uses a ``prestart`` script ``/usr/bin/tezos-node-prestart`` to do
an unconditional ``octez-node upgrade storage`` before starting the node.

Logs are stored in ``/var/lib/tezos/<network>/daily_logs``

The user ``tezos`` is created and owns the data.

While installing Serokell **tezos-baking**, the following additional packages (+
sys dependencies) are installed:

- tezos-node
- tezos-client
- tezos-baker

The **tezos-baking** post-installation script also sets up the baker, offering
different configuration options (ledger, remote, local). The package also
provides a script ``/usr/bin/tezos-baker-proxford-start`` to start the baker
according to the configuration.

The keys are stored in ``$TEZOS_CLIENT_DIR/config``, by default in
``/var/lib/tezos/.tezos-client/config``

Upgrade to Nomadic-Labs Debian packages
=======================================

The Serokell packages and Nomadic-Labs packages follow two different
philosophies. The Serokell packages provide a custom script to configure the
node and the baker that is tightly coupled with their Debian packages and
scripts. It also allows running multiple nodes on the same machine at the same
time (on distinct networks), each with a different systemd service and
dedicated configuration.

The Nomadic-Labs packages are more minimalistic. They provide
the infrastructure to run on only one network at a time. Instead of using a
custom script, they use ``debconf``, the standard Debian configuration system,
and make it easy for the user to configure the node using the Octez documentation.

Migration of the octez-node
---------------------------

First, we need to remove all Serokell packages using ``sudo apt autoremove
tezos-client tezos-node tezos-baking``. This command removes only the packages,
leaving the data and configuration files intact.

We install the octez packages (e.g., debian/bookworm):

.. code:: shell

   export distribution=next/debian
   export release=bookworm

.. literalinclude:: install-bin-deb.sh
   :language: shell
   :start-after: [add repository]
   :end-before: [end add repository]

Refer to the :doc:`howtoget` for more details.

Notice that in some systems, we need to install the Dialog interface with
``sudo apt install dialog`` and reconfigure debconf to force it to ask few
interactive questions with ``sudo dpkg-reconfigure debconf``, and set the
priority to **medium**. The *priority* decides when debconf should ask
configuration questions to the user, or rather use the maintainer default
values.

Now we can install the **octez-node** package with ``sudo apt install
octez-node`` and answer **yes** when asked if we want to skip the node
configuration. This will provide a clean installation of the package and allow
us to proceed with the migration.

We need to make sure to keep a backup of all the data of the node from the
previous Serokell installation, and copy it to the new location (``/var/tezos``
is the default datadir for Nomadic-Labs' **octez-node** package).

We also have to copy the data maintained by the client.

.. code:: shell

  sudo mkdir -p /var/tezos
  sudo cp -a /var/lib/tezos/node-ghostnet /var/tezos/.tezos-node
  sudo cp -a /var/lib/tezos/.tezos-client /var/tezos/

Now we have to change the home directory and shell for the user tezos. This is
necessary to allow for a smooth transition and configuration of the octez
packages.

.. code:: shell

  sudo usermod -d /var/tezos tezos
  sudo chsh -s /bin/bash tezos
  # to verify
  cat /etc/passwd | grep tezos
  tezos:x:994:993::/var/tezos:/bin/bash

Now we have to enable the service for the octez node.

.. code:: shell

  sudo systemctl enable octez-node

If everything went according to plan, we can restart the node.

.. code:: shell

  sudo systemctl start octez-node
  sudo systemctl status octez-node

Migration for the octez-baker
-----------------------------

Install the baker package:

.. code:: shell

  sudo apt install octez-baker

The installation procedure will ask a couple of questions. Since the
configuration of the baker keys lives in the octez client data in
``/var/tezos/.tezos-client``, we can directly enable and restart the baker.

.. code:: shell

  sudo systemctl enable octez-baker-active
  sudo systemctl start octez-baker-active

Note that the octez baker package offers two services: the **octez-baker-active**
and **octez-baker-next**. The protocol associated with these two services can be
changed by reconfiguring the octez baker package with ``sudo dpkg-reconfigure
octez-baker`` or manually editing the env files in ``/etc/default/octez-baker*``

For further guidance on configuring the Octez services, check :doc:`services`.
