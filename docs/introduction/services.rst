Setting up Octez Services
=========================

The Octez suite consists of :ref:`several executables <tezos_binaries>`, some to be run interactively, while others are to be run as daemons.

Previous tutorials in this section showed how to :doc:`get started with the different executables <./howtouse>`, and different :doc:`options for participating to Tezos <./howtorun>` such as delegating, staking, and baking.
However, in these tutorials, daemons are just run in background or left in another terminal.
This page shows how Octez daemons can be safely run from the official binary packages, as Unix services, which can ensure that they are started automatically and restarted in case of failures.

Why Use Binary Packages?
------------------------

When it comes to installing software, especially for critical
applications like Tezos/Octez, it’s crucial to ensure a secure and
stable environment. While compiling from source can provide
customization options, it often introduces complexities and risks.
Instead, opting for binary packages from a trusted source simplifies
installation and enhances security.

Binary packages compiled for a specific platform should be always
preferred over source or statically compiled packages. These packages
can be used to simplify the creation of OCI images or deployed on bare
metal.

Using the official binary packages offers several advantages:

-  **Security**: Binary packages are pre-compiled and thoroughly tested,
   reducing the risk of vulnerabilities introduced during compilation.
   All our packages are signed and our supply chain is strictly
   monitored to make sure the packages that we deliver only use
   components that were vetted by our engineering team.

-  **Stability**: Binaries from a trusted repository undergo rigorous
   testing, ensuring stability and compatibility with the target system.
   We make sure to compile our binaries in a clean environment and
   using an up-to-date software distribution. We use LTS (long-term service) distributions to
   enhance stability and reduce the attack surface.

-  **Ease of Installation**: Binary packages can be installed using
   standard package management tools, streamlining the process. For instance, ``apt`` is
   ubiquitous in the Debian world. These tools allow us to sign our packages
   that can be automatically verified by the end user during installation. We
   provide packages that allow the end user to easily tailor their
   installation for different use cases.

-  **Reduced Downtime**: With reliable binaries and straightforward
   installation, system downtime due to installation errors or
   compatibility issues is minimized. We carefully test the upgrade
   process of our packages to make sure that end users can enjoy a click and go
   upgrade process with near to zero downtime.

Installing Octez
----------------

First of all, you must :ref:`install Octez from binary packages <installing_binaries>`, as explained in the installation tutorial.

To make things concrete, we are assuming here that you installed Octez Debian packages, but most commands should apply to other distributions.

Verify Installation
~~~~~~~~~~~~~~~~~~~

After installation, verify that Octez components are installed correctly:

.. code:: shell

   octez-node --version
   octez-client --version

These binaries can be used by any user. However, to run Octez in
production our package setup a special user named ``tezos`` to run all
daemons via ``systemd`` and without direct user intervention.

Enabling Daemons
----------------

To ensure that Octez services start automatically on system boot and can
be managed using ``systemd``, we need to enable the services.
Indeed, these services are installed by ``apt`` but left disabled by default.

For example, to enable the Octez node ``systemd`` service:

.. code:: shell

   sudo systemctl enable octez-node

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
In particular:

- to accelerate the node's bootstrap, you usually :ref:`import a snapshot file <importing_a_snapshot>` before starting the node;
- you may want to connect to a test network if your goal is learning, developing, or testing.

Running the Octez node
~~~~~~~~~~~~~~~~~~~~~~

Once the node is configured, we can use ``systemd`` to start the daemon:

.. code:: shell

   sudo systemctl start octez-node

We can also check the status of the daemon in the logs of the node that
are stored by default in ``/var/log/tezos/node.log``. Logs are
automatically rotated using ``logrotate``.

The Octez baker can be configured in a similar way. However, because of
the sensitive nature of the private keys needed by the baker to
function, we suggest hereafter a slightly more involved configuration procedure
using the Octez signer.

Configuring the signer
----------------------

First, logged as the user chosen to run the signer, we must create a set of
keys. These are the private keys that will be entrusted to the signer to
actually sign operations on behalf of the baker. The signer will run in a
different process (possibly on a separate host), and ideally using a hardware
enclave such as a :ref:`hardware ledger <ledger>`. For the sake of brevity, in
this example, the keys will be simply stored on the disk, but this is not a
recomended setting for a production baker.

We create an authentication key that is going to be used to authenticate
the baker with the signer, and a signing key to sign the operations.

The signer secret key is stored in the current user directory and
we will configure the baker using the ``tz1`` address for this key.

The signer authentication key is stored in the ``tezos`` user space
and we will configure the signer using the public key associated to
the auth key.

.. code:: shell

   # create a signing key ( as current user )
   $ octez-signer gen keys alice

   # create an authentication key for signer authorization
   $ sudo su tezos -c "octez-client gen keys auth"

   $ sudo su tezos -c "octez-client show address auth"
   Hash: tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs
   Public Key: edpk123456789....

   # add the auth key to the octez-signer. This is the default
   # options set in the octez-signer.service file
   $ octez-signer add authorized key edpk123456789... --name auth

Now we need to configure the ``octez-signer`` service. We use again ``systemd``
and we run it as a user service. The ``octez-signer.service`` file can be
customized by the user if needed to allow for more complex and secure
scenarios.

.. code:: shell

   # customize the signer service if needed
   $ mkdir -p ~/.config/systemd/user/
   $ cp /usr/share/doc/octez-signer/octez-signer.service \
        ~/.config/systemd/user/

   # start the service
   $ systemctl --user start octez-signer

   # examine the logs
   $ journalctl --user-unit octez-signer

For more advanced configurations, see the :ref:`signer guide <signer>`.

Configuring the baker
---------------------

Now that the signer is running, we need to configure the baker.
Since the baker runs as the user ``tezos``, we use ``sudo su tezos -c`` to wrap the configuration command below:

.. code:: shell

   # Get the tz1 address of our signing key
   $ octez-signer show address alice
   Hash: tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs
   Public Key: edpkvGAz71r8SZomcvF7LGajXT3AnhYX9CrmK3JWgA2xk8rf8CudY8

   # Configure the baker to use the remote signer
   sudo su tezos -c "octez-client -R tcp://localhost:7732 \
      import secret key alice remote:tz1V7TgBR52wAjjqsh24w8y9CymFGdegt9qs"

Now that everything is in place, as for the node, we can first enable,
then start the Octez baker.

.. code:: shell

   sudo systemctl enable octez-baker-active.service
   sudo systemctl start octez-baker-active.service

The logs of the baker are available in ``/var/log/tezos/baker-active.log``.

Notice that the Octez baker package defines two services,
``octez-baker-active`` and ``octez-baker-next`` respectively associated
with the active protocol and the next proposed protocol upgrade. The
names of the protocols associated with these daemons are specified in
``/etc/default/octez-baker-*`` files. ``octez-baker-next``
should be used for testing and during a protocol upgrade. Running
``octez-baker-next`` together with ``octez-baker-active`` is
possible and recommended to avoid downtime.

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

Mind restarting the running services using ``systemctl restart <service>``.
