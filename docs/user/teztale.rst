Monitoring the consensus protocol
=================================

Monitoring the :doc:`consensus protocol <../active/consensus>` on a Tezos network (either Mainnet or a test network, see :doc:`./multinetwork`) can be useful to measure the overall health of the network -- how fast consensus is achieved for each new block --, or the performance of individual bakers, in order to spot and fix eventual performance issues.

Teztale is a tool for monitoring the Tezos consensus protocol, named Tenderbake.
Teztale is regularly used by network maintainers to prevent and manage network incidents, but can also be used by bakers or anyone interested in understanding and inspecting the process of creating new blocks.

This page explains how to set up Teztale in a simple configuration, running on a single machine.

Teztale is composed by:

- 2 backend components: the server and the archiver, respectively gathering and storing data, and
- a data visualization interface, displaying the data as an interactive dashboard (e.g., https://nomadic-labs.gitlab.io/teztale-dataviz/)

A Teztale archiver is connected to an Octez node, and feeds one or many Teztale server(s)
with information extracted from this node.

A Teztale server gathers data from one or many archiver(s), but only one network. Another
way to say that is that if you feed a Teztale server with multiple archivers, all of them
must be connected to nodes that participate to the same network.

The data visualization tool connects to a Teztale server through its public Tetzale RPC API.

.. figure:: images/teztale-arch.svg

Let's see how to set up and connect these components in order to visually monitor the consensus protocol with Teztale.

Run a Tezos node
----------------

First, you need to setup an ``octez-node``. The rest of the tutorial will
assume that the node uses the ``--rpc-addr 127.0.0.1:8732`` option. If
you want to use a different address/port, adapt the rest accordingly.

Run a Teztale server
--------------------

The first thing to set up is the Teztale server. The server is the one that will:

#. Receive updates from Teztale archiver(s)
#. Return data via its ``GET`` API

Download the ``octez-teztale-server`` binary from gitlab. In this example, we will put it in
``/home/user/bin/``.

Create a configuration file for your server (e.g. ``/home/user/.teztale-server/config.json``)

.. code-block:: json

    {
        "db": "sqlite3:/home/user/.teztale-server/mainnet.sqlite",
        "interfaces": [
            {
                "address": "127.0.0.1",
                "port": 8880
            }
        ],
        "users": [
            {
                "login": "TEZTALE_LOGIN",
                "password": "TEZTALE_PASSWORD"
            },
        "admins": [
            {
                "login": "TEZTALE_ADMIN_LOGIN",
                "password": "TEZTALE_ADMIN_PASSWORD"
            }
        ]
    }

Teztale currently support two DBMS: SQLite and PostgreSQL. Using PostgreSQL looks like
something like ``postgres://DB_USER:DB_PASSWORD@DB_HOST:DB_PORT/DB_BASE``.

Create a service description like this
``/etc/systemd/system/teztale-server.service`` file, and use it to launch ``teztale-server``:

.. code-block::

    [Unit]
    Description      = Teztale Server Service
    Documentation    = https://gitlab.com/nomadic-labs/teztale
    Wants            = network-online.target
    After            = network-online.target

    [Service]
    User             = user
    Group            = user
    WorkingDirectory = /home/user/
    ExecStart        = /home/user/bin/octez-teztale-server /home/user/.teztale-server/config.json
    Restart          = on-failure

    [Install]
    WantedBy         = multi-user.target

.. code-block::

    $ sudo systemctl enable teztale-server.service
    $ sudo systemctl start teztale-server.service

Run a Teztale archiver
----------------------

Download the ``octez-teztale-archiver`` binary. In this example, we will put it in
``/home/user/bin/``.

Create the ``teztale-archiver`` service:

.. code-block::

    [Unit]
    Description      = Teztale Archiver Service
    Documentation    = https://gitlab.com/nomadic-labs/teztale

    After            = octez-node.target
    Wants            = octez-node.target
    PartOf           = octez-node.target

    [Service]
    User             = user
    Group            = user
    WorkingDirectory = /home/user/
    ExecStart        = /home/user/bin/octez-teztale-archiver --endpoint http://127.0.0.1:8732 feed http://TEZTALE_LOGIN:TEZTALE_PASSWORD@127.0.0.1:8880
    Restart          = always
    RestartSec       = 10s

    [Install]
    WantedBy         = multi-user.target

Enable and start the service:

.. code-block::

    $ sudo systemctl enable teztale-archiver.service
    $ sudo systemctl start teztale-archiver.service

Visualize your data
-------------------

Once you have Teztale server and archiver up and running,
you may want to also host the data visualization interface.

Download the zip file from the release page of the `teztale-dataviz repository <https://gitlab.com/nomadic-labs/teztale-dataviz>`__ and extract it somewhere
accessible by the Teztale server, for instance in our case: ``/home/user/teztale-dataviz``.

After that, you will need to edit your server configuration in order to tell ``teztale-server`` to serve static files from your directory.

.. code-block:: diff

    {
        "db": "sqlite3:/home/user/.teztale-server/mainnet.sqlite",
        "interfaces": [
            {
                "address": "127.0.0.1",
                "port": 8880
            }
        ],
        "users": [
            {
                "login": "TEZTALE_LOGIN",
                "password": "TEZTALE_PASSWORD"
            }
    -     ]
    +     ],
    +     "public_directory": "/home/user/teztale-dataviz/"
    }

You can restart your server, and visit: ``<ADDRESS_OF_YOUR_SERVER>:8880/index.html``

You can find a quick presentation of the ``teztale-dataviz`` interface in this blog post:
https://research-development.nomadic-labs.com/introducing-teztale.html