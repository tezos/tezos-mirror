Node Configuration
==================

The Tezos node can be configured in flexible ways to control various
aspects of its behavior, such as :ref:`RPC <configure_rpc>`, :ref:`P2P <configure_p2p>`, or :ref:`shell <configure_shell>` parameters, the directory for storing data, :ref:`logging levels <configure_logging>`, and so on. These aspects can be customized in two different ways:

- by supplying **options** on the command line when running the node
- by specifying **parameters** in a configuration file for the node

When the same parameter is set both in the configuration file and using a command line option, the value on the command line is taken into account (and the configuration file is not updated).

The list of configurable options on the command line interface (CLI) can be
obtained using the following command::

    tezos-node run --help

.. _node-conf-file:

Configuration file
------------------

Parameters in the configuration file can be specified in two different ways:

- by creating and updating the configuration file using the ``config`` command of ``tezos-node``. This covers a subset of the CLI of the ``run`` command of ``tezos-node`` mentioned above.
- by directly editing the configuration file. This allows to specify all the available configuration parameters, including some that cannot be set using the options of the ``config`` and ``run`` commands of ``tezos-node``, for example network parameters such as connection or authentication timeouts.

The list of configurable options and parameters for the node can be obtained using the following command::

    tezos-node config --help

This command also explains the role of each option and parameter and the range of possible values.

The config command
~~~~~~~~~~~~~~~~~~

::

   ./tezos-node config init

This will initialize a configuration file for the node in
``$HOME/.tezos-node/config.json``, using default values. It only
specifies that the node will listen to incoming connections on socket
address ``[::]:9732``.

The easiest way to amend this default configuration is to use

::

   # Update the config file
   ./tezos-node config update <…>
   # Start from an empty cfg file
   ./tezos-node config reset <…>

However, note that the ``network`` configuration parameter, needed to run the node on a network other than the default one (Mainnet), can only be defined when the configuration file is initialized (using ``init``), and cannot be updated later (using ``update``). See the instructions for :ref:`running the node in test networks <multinetwork>`.

For example, the following script initializes and fills a configuration file using many command-line options:

.. literalinclude:: node-config.sh
    :language: shell
    :start-at: [remove config file if exists]

Editing the configuration file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All blockchain data is stored under ``$HOME/.tezos-node/``.  You can
change this by doing `./tezos-node config update --data-dir
</somewhere/in/your/disk>`.

To run multiple nodes on the same machine, you can duplicate and edit
``$HOME/.tezos-node/config.json`` while making sure they don't share
the same ``data-dir``. Then run your node with ``./tezos-node
run --config-file=</path/to/alternate_cfg>``.

Here is an example configuration file with many parameters specified.
Most of the time it uses default values, except for cases where the
default is not explanatory enough (i.e. ``bootstrap-peers`` is an empty
list by default). Comments are not allowed in JSON, so this
configuration file would not parse. They are just provided here to help
writing your own configuration file if needed.

.. literalinclude:: node-config.json
   :language: javascript

The rest of this page provides more details about the use of some selected
configuration options/parameters.

.. _configure_rpc:

RPC parameters
--------------

RPC parameters allow to customize the :doc:`JSON/RPC interface <../developer/rpc>`, by defining for instance hosts to listen for RPC requests, or a certificate/key file necessary when TLS is used.

Access to RPC endpoints can be restricted using an Access Control List. The
default policy grants full access to clients connecting from local machine,
but only allows *safe* endpoints if accessed remotely (see :ref:`default_acl`
for details). A custom list can be put in the configuration file. In the ``rpc``
object a key ``acl`` can be included, containing a list of policies for
different listening addresses:

.. code-block:: json

   {
      "rpc": {
        "acl": [
          {
            "address": "localhost:8732",
            "blacklist": ["GET /chains/**", "GET /network/points/*/unban"]
          }
        ]
      }
   }

The ``address`` field specifies a binding address this rule applies to. Port can be omitted,
in which case the rule applies to any port on that address. Note that this does not automatically
enable RPC on that address, to do that the address must be included in ``listen-addrs`` or passed
by command-line argument ``--rpc-addr`` when starting the node.

Additionally either the ``whitelist`` **or** the ``blacklist`` field must be specified
(but not both), containing a list of paths which should be black-listed or
white-listed. Each element in the list is an API-endpoint (that can be passed to e.g. the ``tezos-client rpc``
command). It may be preceded by a capitalized HTTP method name. In the absence of
the method, the rule applies to all methods. The path can include an ``*``
character, which stands for any whole path segment (i.e. it's not allowed to mix ``*``
with other characters in a path segment).
A ``**`` stands for any possible path suffix.

Additionally ``--allow-all-rpc`` CLI option to ``tezos-node`` can be used to
simply allow all RPC endpoints on a given address. When passed to
``tezos-node config`` command, this option modifies the ``config.json`` file,
putting an appropriate ACL there. When passed to ``tezos-node run``, it
overrides the settings of the ``config.json`` for this particular run, without
modifying the file.

.. danger::
   Exposing all RPCs over the public network is extremely dangerous and
   strongly advised against. It opens the door widely to DoS attacks
   and allows anyone to manipulate the node's configuration, inject blocks
   and in general harm the node's state.

.. warning::
   Rules are always searched from the beginning of the list to the end and
   the first matching address is returned. Therefore if one wants to put one
   rule on a specific port on a given host and another rule for all other ports
   on the same host, then more specific rules should always be written *first*
   Otherwise they'll be shadowed by the more general rule.

.. _default_acl:

Default ACL for RPC
-------------------

The default ACL for RPC depends on the listening address that the node is using.

If the listening address resolves to the loopback network interface, then full
access to all endpoints is granted. Note that it does not matter from which
machine the client is really making a request, but only what the listening
address is. This guarantees that insecure endpoints can only be accessed from
``localhost``.

If the listening address is a network address, then a more restrictive policy
applies. Its main purpose is to protect nodes from attacks. These attacks can
take two main forms:

  - spamming the node with costly requests (denial of service attack)
  - breaking the node by forcing it to perform a risky operation

Thus all costly or risky endpoints are blocked by default. This can be
relaxed or tightened by modifying the configuration file. It's
worth noting that this default policy among other things disallows baking and
endorsing by bakers and endorsers running on remote servers.

The following is the default ACL policy for the node,
hard-coded in :src:`src/lib_rpc_http/RPC_server.ml` (remember to replace
``any.public.address`` with an IP address or a domain name that you'll be
actually listening on):

.. literalinclude:: default-acl.json

The endpoints specifically required for baking can be found in
:src:`vendors/flextesa-lib/tezos_node.ml`.

.. _configure_p2p:

P2P parameters
--------------

P2P parameters allow to customize aspects related to the :doc:`peer-to-peer network layer <../shell/p2p>`, such as:

- defining the bootstrap peers (and ports),
- defining the ports listening to peers,
- setting the greylist timeout,
- running the node in private mode.

.. _private-mode:

Listening ports
~~~~~~~~~~~~~~~

By default, the node listens to incoming connections from peers on port ``9732``, on any of the available network interfaces on the node's host.
This behavior can be changed by passing an IP address and optionally a port number (``addr:port``), using either the ``--net-addr`` configuration option or the P2P ``listen-addr`` configuration parameter.
Note that the IP address is only used for selecting an active network interface (technically, it is only passed to the ``bind()`` function of the socket API). Thus, it is currently not possible to advertise to peers a different port than the binding port.

Private node
~~~~~~~~~~~~

The node can be set in private mode with the option ``--private-mode``
so that:

- it doesn't connect to any peer other than those provided with
  ``--peer`` or in ``bootstrap-peers``
- the peers connected to a private node don't advertise it in the list
  of peers sent to their neighborhood

This feature is especially useful to hide a sensitive node that signs
operations.

For example we could have a set up with two nodes, a private one
connected uniquely with a public one.
The public node runs on a VPS, connects normally to the network and
keeps an up to date state of the network while the private node runs at
your home and is in charge of injecting and signing operations with a
hardware wallet.

::

   tezos-node run --rpc-addr [::] --private-mode \
                                  --no-bootstrap-peers \
                                  --synchronisation-threshold=1 \
                                  --connections 1 \
                                  --peer <public-node-ip>


.. _configure_logging:

Logging
-------

It is possible to set independent log levels for different components in the Tezos node, as well as to specify an output file for logging.
This can be done in two different ways:

- by defining log parameters in the configuration file (see :ref:`node-conf-file`)
- by setting the environment variable ``TEZOS_LOG`` before starting the node.

The list of the Tezos components that can be logged and the syntax for the logging options can be found in the DEBUG section displayed by ``tezos-node run --help``:

.. literalinclude:: ../api/tezos-node-run.txt
    :start-after: DEBUG
    :end-before: OPTIONS

Additionally, specific logs can be activated for the context part, see the :ref:`storage layer <context_component>` documentation.

Event-based logging
~~~~~~~~~~~~~~~~~~~

A more advanced interface for logging, based on events generated by the different components, is available using the environment variable ``TEZOS_EVENTS_CONFIG``. Its value must be a list of URIs separated by spaces.
Each URI defines a *sink* for log events consisting in a Unix output stream and possibly some logging options, and has one of the following forms:

- ``file-descriptor-path://<path>``
- ``file-descriptor-stdout://``
- ``file-descriptor-stderr://``

The URIs may further append options in the form of a query ``?<opt1>=<val1>&<opt2>=<val2>...``.

For example, the following definition sends all the events to file ``/the/path/to/write.log``, formatted as one event per line::

  export TEZOS_EVENTS_CONFIG=file-descriptor-path:///the/path/to/write.log?format=one-per-line

You may find all the available options in the `file descriptor sink API <https://tezos.gitlab.io/api/odoc/_html/tezos-stdlib-unix/Tezos_stdlib_unix/File_descriptor_sink/index.html>`__.

.. note::
  Note that, as the Tezos codebase is in the process of moving from the legacy logging framework to the events-based logging framework, some interferences are observed between the two.
  In particular, the configuration option ``level-at-least`` documented in the above API does not currently work as expected, so you should avoid using it.

.. _configure_shell:

Shell parameters
----------------

Configuration options/parameters for the shell allow tuning the working of the :doc:`validation subsystem <../shell/validation>`.

In particular, the synchronization heuristics implemented by the chain validator can be controlled using parameters such as the synchronization threshold or the latency, described in the documentation of the  :doc:`synchronization heuristics <../shell/sync>`.
