Client Configuration
====================

The Octez client can be configured in flexible ways to control various
aspects of its behavior, such as running in different running modes (:doc:`./sandbox`, :doc:`./mockup`, ...), connecting to a public Tezos node, selecting the directory for storing data, and so on.

All these aspects
can be customized by supplying **options** on the command line when running the client. Refer to :ref`the client manual <client_manual>` for details.

A subset of these aspects can be customized by specifying parameters in a **configuration file** for the client.
These include, for example:

- the address and port of a Tezos node to connect to, as an RPC endpoint (by default, the local node; but it can be another node, see `a list of public RPC nodes <https://docs.tezos.com/architecture/rpc#public-and-private-rpc-nodes>`__)
- the directory where the client stores data
- the number of confirmation blocks needed before an operation is considered included
- the files defining bootstrap accounts and protocol constants, when running in :doc:`mockup mode <./mockup>`.

When the same parameter is set both in the configuration file and using a command line option, the value on the command line is taken into account (and the configuration file is not updated).

Finally, a few aspects of the client behavior can be customized by a set of **environment variables**.

.. _client_conf_file:

Client configuration file
-------------------------

.. note::

    The format of the client configuration file (and the associated commands to manipulate it) is understood not only by the Octez client, but also by several other Octez executables, such as ``octez-admin-client``, the baker, and the accuser. For details, refer to the manual of each tool.

Parameters in the configuration file can be specified in two different ways:

- by creating and updating the configuration file using the ``config`` command of ``octez-client``.

- by directly editing the configuration file.

The config command
~~~~~~~~~~~~~~~~~~

::

   ./octez-client config init

This will initialize a configuration file for the client in
``$HOME/.tezos-client/config``, using default values. For instance, it
specifies that the client will use the local node as an RPC endpoint.

The easiest way to amend this default configuration is to use commands such as:

::

   # Update the config file:
   octez-client <options...> config update
   # Check your new values:
   octez-client config show
   # If you want to restart from an empty cfg file:
   octez-client config reset

Editing the configuration file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may also edit the configuration file directly (``$HOME/.tezos-client/config`` by default).

To run the client in multiple configurations on the same machine, you can duplicate and edit
``$HOME/.tezos-client/config`` while making sure they don't share
the same ``base-dir``. Then run your client with ``./octez-client run --base-dir=</path/to/alternate_cfg>``.

.. _client_variables:

Environment variables for the client
------------------------------------

The behavior of the client can be configured using the following environment variables:

- ``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER``: Setting this variable to "YES" (or: "yes", "Y", "y") disables the warning displayed by the client at startup when it is not launched on Mainnet.
- ``TEZOS_CLIENT_DIR``: This variable may be used to supply the client data directory (by default, ``~/.tezos-client``).
  Its value is overridden by option ``-d``.
- ``TEZOS_SIGNER_*``: These variables are used for connecting the client to a remote :ref:`signer <signer>` (see there for details).
- ``TEZOS_CLIENT_RPC_TIMEOUT_SECONDS``: This variable controls how long (in seconds, as an integer)
  the client will wait for a response from the node, for each of the two RPC calls made during startup.
  If this variable is not set, or otherwise cannot be parsed as a positive integer, a default value of ``10`` seconds is used for each call.
  The two RPC calls this variable affects are queries that the client makes to the node in order to determine:
  (1) the protocol version of the node it connects to, and (2) the commands supported in that version.
- ``TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS``: This variable specifies
  custom HTTP headers to use with the ``--operations-pool`` option. Only the Host
  header is supported as of now (see description in `rfc2616, section 14.23
  <https://datatracker.ietf.org/doc/html/rfc2616#section-14.23>`_
