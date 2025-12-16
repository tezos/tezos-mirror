Setting up Octez Services
=========================

.. warning::

   **Experimental Tool Available**

   An experimental tool called `octez-manager <https://github.com/trilitech/octez-manager>`_ is available to simplify the setup and management of Octez services. This tool is currently in beta and provides an alternative way to configure and run Octez daemons. While it may streamline the setup process, please be aware that it is still under active development and may have limitations or bugs. For production environments, we recommend carefully evaluating your requirements before using beta software.

The Octez suite consists of :ref:`several executables <tezos_binaries>`, some to be run interactively, while others are to be run as daemons.

Previous tutorials in this section showed how to :doc:`get started with the different executables <./howtouse>`, and different :doc:`options for participating to Tezos <./howtorun>` such as delegating, staking, and baking.
However, in these tutorials, daemons are just run in background or left running in another terminal.

This page shows how Octez daemons can be safely run from the official binary packages, as Unix services, which can ensure that they are started automatically and restarted in case of failures.

This is a intermediate-level baking howto assuming some familiarity with the previous tutorials mentioned above, on installing, on setting up Octez, and on the notion of baking.
For a more accessible and detailed step-by-step tutorial on running an Octez baker, including the complete set up of a baking account and thorough manual verifications upon each step, see
`Run a Tezos node in 5 steps <https://docs.tezos.com/tutorials/join-dal-baker>`__.

Installing Octez
----------------

First of all, you must :ref:`install Octez from binary packages <installing_packages>`, as explained in the installation tutorial.

To make things concrete, we are assuming here that you installed Octez Debian packages, but most commands should apply to other distributions.

Verify Installation
~~~~~~~~~~~~~~~~~~~

After installation, verify that Octez components are installed correctly:

.. code:: shell

   octez-node --version
   octez-client --version

These binaries can be used by any user. However, to run Octez in
production our packages set up a special user named ``tezos`` to run all
daemons via ``systemd`` and without direct user intervention.

Configuring the node
--------------------

Octez packages provide a very basic configuration in
``/etc/default/octez-*`` files. These are to be read as configuration
files for the ``systemd`` services. We took a non opinionated approach
regarding the basic configuration provided by default. We do not make
assumptions on the setup of the machine or try to imagine all possible
use cases. To create more advanced configurations, the user can directly
configure each daemon using commands such as:

.. code:: shell

   sudo su tezos -c "octez-node config ..."

For more details on configuring the Octez node, see :doc:`../user/setup-node`.

The default for the octez-node is to connect to ``mainnet``. You may want to
connect to a test network if your goal is learning, developing, or testing.


.. _node_as_service:

Running the Octez node
~~~~~~~~~~~~~~~~~~~~~~

Once the node is configured, we can use ``systemd`` to start the daemon:

.. code:: shell

   sudo systemctl start octez-node

If configured to do so during package installation, this will automatically download a snapshot
and import it before starting the ``octez-node`` service (starting with v22 packages).
In this case, the command can take quite a few minutes to execute, for downloading and importing the snapshot.

We can check the status of the daemon in the logs of the node that
are stored by default in ``/var/log/tezos/node.log``. Logs are
automatically rotated using ``logrotate``.

If you need to relocate the datadir of the octez-node, you would first need to change the home
directory of the ``tezos`` user with the command ``usermod -m -d /custom tezos``
and then configure the ``DATADIR`` variable in ``/etc/default/octez-node``
accordingly.

Setting up baking keys
----------------------

The most important preliminary step for running a baker is setting up a baker account having enough :doc:`baking power <../active/baking_power>` (typically, possessing more than 6000 tez).
We don't cover here the optional set up of an associated :ref:`consensus key <consensus_key_details>` and/or :ref:`companion key <companion_key>`.

If you intend to bake on a testnet, you can simply create a key as follows, fund it, and configure the :doc:`DAL node <../shell/dal_node>` to use it::

   $ sudo su tezos -c "octez-client gen keys mybaker"
   $ sudo su tezos -c "octez-client show address mybaker"
   Hash: tz1Ti8WHvfp3XKsTCKWLFv4TrER2HDofXG39
   Public Key: edpkuGRKH8oZDP2PH2EULw9PGzJHdf5g2zCiHnaYG7tapeePUNAWeC

   $ ... # Fund mybaker with > 6000 tez, e.g. at https://faucet.currentnet.teztnets.com
   $ sudo su tezos -c "octez-client register key mybaker as delegate"
   $ sudo su tezos -c "octez-client stake 6000 for mybaker"
   $ sudo su tezos -c "octez-dal-node config init --endpoint http://127.0.0.1:8732 --attester-profiles=tz1Ti8WHvfp3XKsTCKWLFv4TrER2HDofXG39"

The baker will use the baking key automatically. Indeed, the baker bakes for all the keys for which it has the private key. If you want to avoid this behavior, you can specify a specific baking key by editing the file /etc/default/octez-baker and assigning a value to variable BAKER_KEY.

When baking on a testnet, you may skip the following section on configuring the signer, and directly start the baker.

Configuring the signer
~~~~~~~~~~~~~~~~~~~~~~

If you envision to bake on mainnet some day, we highly recommend to train yourself using the Octez signer, because of the sensitive nature of the private keys needed by the baker to function (note that we still show the instructions for a testnet in this training).

To configure the octez-signer, first, logged as the user chosen to run the
signer, we must create a set of keys. These are the private keys that will be
entrusted to the signer to actually sign operations on behalf of the baker. The
signer will run in a different process (possibly on a separate host), and
ideally using a hardware enclave such as a :ref:`hardware ledger <ledger>`. For
the sake of brevity, in this example, the keys will be simply stored on the
disk, but **this is not a recommended setting for a production baker**.

We create an authentication key that is going to be used to authenticate
the baker with the signer, and a signing key to sign the operations.

The signer secret key is stored in the current user directory and
we will configure the baker using the ``tz1`` address for this key.

The signer authentication key is stored in the ``tezos`` user space
and we will configure the signer using the public key associated to
the auth key.

.. code:: shell

   # create a signing key ( as current user )
   $ octez-signer gen keys mybaker

   # create an authentication key for signer authorization
   $ sudo su tezos -c "octez-client gen keys auth"

   $ sudo su tezos -c "octez-client show address auth"
   Hash: tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs
   Public Key: edpk123456789....

   # add the auth key to the octez-signer. This is the default
   # options set in the octez-signer service file
   $ octez-signer add authorized key edpk123456789... --name auth

Now we need to configure the ``octez-signer`` service. We use again ``systemd``
and we run it as a user service. The ``octez-signer`` service file can be
customized by the user if needed to allow for more complex and secure
scenarios.

.. code:: shell

   # customize the octez-signer service if needed
   $ mkdir -p ~/.config/systemd/user/
   $ cp /usr/share/doc/octez-signer/octez-signer.service \
        ~/.config/systemd/user/

   # start the octez-signer service
   $ systemctl --user start octez-signer

   # examine the logs
   $ journalctl --user-unit octez-signer

For more advanced configurations, see the :ref:`signer guide <signer>`.

Now that the signer is running, we need to fund the baking address and configure the baker and the :doc:`DAL node <../shell/dal_node>` to use it.
Since the baker runs as the user ``tezos``, we use ``sudo su tezos -c`` to wrap
the configuration commands below:

.. code:: shell

   # Get the tz1 address of our signing key
   $ octez-signer show address mybaker
   Hash: tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs
   Public Key: edpkvGAz71r8SZomcvF7LGajXT3AnhYX9CrmK3JWgA2xk8rf8CudY8

   # Configure the baker to use the remote signer
   $ sudo su tezos -c "octez-client -R tcp://localhost:7732 \
      import secret key mybaker remote:tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs"
   $ ... # Fund mybaker with > 6000 tez, e.g. at https://faucet.currentnet.teztnets.com
   $ sudo su tezos -c "octez-client -R tcp://localhost:7732 register key mybaker as delegate"
   $ sudo su tezos -c "octez-client -R tcp://localhost:7732 stake 6000 for mybaker"
   $ sudo su tezos -c "octez-dal-node config init --endpoint http://127.0.0.1:8732 --attester-profiles=tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs"

Starting the baker
------------------

Now that everything is in place, we can start the Octez baker.

.. code:: shell

   sudo systemctl start octez-baker

This service will automatically start all accusers and bakers for all protocols
shipped with the package.
If ``--dal-node`` is configured in ``/etc/default/octez-baker``, the DAL node will also be started alongside the baker, which is the recommended setting for all bakers.
The DAL node is run in a default configuration, that is, in controller mode without any profile; you may edit file ``/etc/default/octez-dal-node`` or use commands such as ``sudo su tezos -c "octez-dal-node config ..."`` to set up another configuration, refer to page :doc:`../shell/dal_node`.

The logs of the baker are available in ``/var/log/tezos/baker*.log``.


.. _services_upgrade:

Upgrading Octez
---------------

To upgrade Octez to the latest version, see the corresponding section in
the :doc:`installation guide <./howtoget>`.

In our case, we can simply proceed as follows:

.. code:: shell

   sudo apt-get update
   sudo apt-get upgrade octez-node octez-client octez-baker

When necessary, the upgrade scripts will make the user aware of breaking
changes and required actions such as new configuration parameters or
changes in governance.

Mind reloading the new services using ``sudo systemctl daemon-reload`` and then
restarting the running services using ``sudo systemctl restart <service>``.
When doing so:

- Always make sure that all binaries or packages are on the same version of Octez.
- The restart order should follow the dependency order: (i) Octez node, (ii) DAL node, (iii) baker, (iv) accuser (if running one).
