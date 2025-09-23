Node Configuration
==================

The Octez node can be configured in flexible ways to control various
aspects of its behavior, such as :ref:`RPC <configure_rpc>`, :ref:`P2P
<configure_p2p>`, or :ref:`shell <configure_shell>` parameters, the directory
for storing data, :doc:`logging <./logging>`, and so on. These aspects
can be customized in two different ways:

- by supplying **options** on the command line when running the node
- by specifying **parameters** in a configuration file for the node

When the same parameter is set both in the configuration file and using a command line option, the value on the command line is taken into account (and the configuration file is not updated).

The list of configurable options on the command line interface (CLI) can be
obtained using the following command::

    octez-node run --help

.. _node-conf-file:

Node configuration file
-----------------------

Parameters in the configuration file can be specified in two different ways:

- by creating and updating the configuration file using the ``config`` command of ``octez-node``. This covers a subset of the CLI of the ``run`` command of ``octez-node`` mentioned above.

  The list of parameters configurable via the ``config`` command can be obtained using the following command::

    octez-node config update --help

- by directly editing the configuration file. This allows to specify all the available configuration parameters, including some that cannot be set using the options of the ``config`` and ``run`` commands of ``octez-node``, for example network parameters such as connection or authentication timeouts.

  The list of configurable parameters in the configuration file can be obtained using the following command::

    octez-node config --help

  This command also explains the role of each option and parameter and the range of possible values.

The config command
~~~~~~~~~~~~~~~~~~

::

   ./octez-node config init

This will initialize a configuration file for the node in
``$HOME/.tezos-node/config.json``, using default values. It only
specifies that the node will listen to incoming connections on socket
address ``[::]:9732``.

.. warning::
   In most of the cases, the default configuration file is sufficient as is, or slightly adjusted.
   The following instructions detail how to adjust the node parameters, but you are advised to only change parameters that you fully understand.
   Don't blindly use the values in the examples below; they are just examples!

The easiest way to amend this default configuration is to use

::

   # Update the config file:
   octez-node config update <…>
   # Check your new values:
   octez-node config show
   # If you want to restart from an empty cfg file:
   octez-node config reset <…>

However, note that the ``network`` configuration parameter, needed to run the node on a network other than the default one (Mainnet), can only be defined when the configuration file is initialized (using ``init``), and cannot be updated later (using ``update``). See the instructions for :doc:`running the node in test networks <./multinetwork>`.

For example, the following script initializes and fills a configuration file using several command-line options:

.. literalinclude:: node-config.sh
    :language: shell
    :start-at: [remove config file if exists]

Editing the configuration file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All blockchain data is stored under ``$HOME/.tezos-node/`` by default.
You can
change this by doing ``./octez-node config update --data-dir </somewhere/in/your/disk>``.

To run multiple nodes on the same machine, you can duplicate and edit
``$HOME/.tezos-node/config.json`` while making sure they don't share
the same ``data-dir``. Then run your node with ``./octez-node run --config-file=</path/to/alternate_cfg>``.

Here is an example configuration file with several parameters specified.
Incidentally, it is exactly the one resulting from the command sequence above.

.. literalinclude:: node-config.json
   :language: javascript

Thus, you can start with the initial configuration file, and eventually modify it using ``config update`` commands and/or using a text editor, to define any set of node parameters that are suitable for you (recall that the former covers a subset of parameters, while the latter covers the full set).

The rest of this page provides more details about the use of some selected
configuration options/parameters.

.. _configure_rpc:

RPC parameters
--------------

RPC parameters allow to customize the :doc:`JSON/RPC interface
<../developer/rpc>`, by defining for instance network addresses (corresponding
to node's network interfaces) to listen for RPC requests on, or a
certificate/key file necessary when TLS is used.

.. _node_access_control:

Access Control Lists
~~~~~~~~~~~~~~~~~~~~

Access to RPC endpoints can be restricted using an Access Control List. The
default policy grants full access to clients connecting from the local machine,
but only allows *safe* endpoints if accessed remotely (see :ref:`default_acl`
for details). One important exception to this rule is when the node listens on
the special address ``0.0.0.0``, which means roughly "listen to requests
addressed to any address" and hence all requests coming to such a node are
treated as coming from a remote host. It is worth remembering that it's
unimportant where the request actually comes from. The policy is defined for an address
on which the node is configured to listen to incoming RPC requests. A custom
list can be put in the configuration file. In the ``rpc`` object a key ``acl``
can be included, containing a list of policies for different listening
addresses:

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

.. note::
   Both listening addresses and ACL addresses are resolved at the node's startup
   (they're not re-resolved at any point after that), before matching. This
   means that only IP addresses are ever matched, so if e.g. "localhost" is
   written in the policy and listening address is set "127.0.0.1", it *does* match. The converse is also true.

.. warning::
   Note that "0.0.0.0" is a specific address, distinct from any other. It is
   usually treated specially by operating systems to mean "any address".
   However, the ACL configuration **does not** follow this rule, because
   matching addresses to rules would have become difficult and potentially very
   confusing. Therefore if a node is configured to listen on address ``0.0.0.0``,
   a policy may be defined for ``0.0.0.0`` specifically (otherwise, the default
   policy will apply to it). If one wishes
   to apply different policies to different addresses, multiple ``rpc-addr``
   switches can be used to listen on those specific addresses and then separate
   policies can be configured for them.

Additionally either the ``whitelist`` **or** the ``blacklist`` field must be specified
(but not both), containing a list of paths which should be black-listed or
white-listed. Each element in the list is an API-endpoint (that can be passed to e.g. the ``octez-client rpc``
command). It may be preceded by a capitalized HTTP method name. In the absence of
the method, the rule applies to all methods. The path can include an ``*``
character, which stands for any whole path segment (i.e. it's not allowed to mix ``*``
with other characters in a path segment).
A ``**`` stands for any possible path suffix.

Additionally ``--allow-all-rpc`` CLI option to ``octez-node`` can be used to
simply allow all RPC endpoints on a given address. When passed to
``octez-node config`` command, this option modifies the ``config.json`` file,
putting an appropriate ACL there. When passed to ``octez-node run``, it
overrides the settings of the ``config.json`` for this particular run, without
modifying the file.

.. danger::
   Exposing all RPCs over the public network is extremely dangerous and
   strongly advised against. It opens the door widely to DoS attacks
   and allows anyone to manipulate the node's configuration, inject blocks
   and in general harm the node's state. Even more discouraged is exposing
   all RPCs on the special ``0.0.0.0`` address.

.. warning::
   Rules are always searched from the beginning of the list to the end and
   the first matching address is returned. Therefore if one wants to put one
   rule on a specific port on a given host and another rule for all other ports
   on the same host, then more specific rules should always be written *first*.
   Otherwise they'll be shadowed by the more general rule.

Examples
""""""""

::

  $ octez-node run --rpc-addr localhost:8732

In this case the RPC is only available for requests coming from ``localhost``
(i.e. ``127.0.0.1``). There's no need configure the ACL, as an allow-all policy
is applied to the local host by default.

::

    $ octez-node run --rpc-addr localhost:8732 --rpc-addr 192.168.0.3:8732

In this example the RPC is available to both ``localhost`` and to the local
network (assuming the node does have address ``192.168.0.3`` in that network).
However, different policies apply to each address. For ``localhost`` an allow-all
policy will be selected as before, but requests addressed to ``192.168.0.3`` will
be filtered by the default ACL (:ref:`see below <default_acl>`).

In this last case, to listen on both ``localhost`` and local network, it might
be tempting to listen on the special ``0.0.0.0`` address::

  $ octez-node run --rpc-addr 0.0.0.0:8732

``0.0.0.0`` is a special address, not attached to any particular networking
interface. Instead it tells the OS to route messages incoming to **all**
interfaces to the node. However, the ACL mechanism does not attach such special
significance to this address. It will apply to this listening address a policy
written for it specifically and in the absence thereof – a default policy for
**remote addresses**. Thus, even if a request is coming through a local
interface, it does not matter, it'll still be treated as if it came from a
remote address.

A common situation is when one wants to accept only safe RPC requests coming from
remote hosts, but enable all RPCs for localhost (which is for instance necessary
to perform baking and attesting). Since all RPCs are available to localhost by
default, it is sufficient to open another listening address::

  $ octez-node run --rpc-addr localhost --rpc-addr 0.0.0.0

The ``--allow-all-rpc`` switch can be used to open all RPCs on a specific address::

  $ octez-node run --rpc-addr 192.168.0.3 --allow-all-rpc 192.168.0.3

Note that the addresses of both ``--rpc-addr`` and ``--allow-all-rpc`` switches
should match. In particular remember that ``0.0.0.0`` is a specific address
and won't match anything else except for itself, even though the underlying OS
might treat it differently. Also be advised that using this option is discouraged
as dangerous, especially when applied to the special ``0.0.0.0`` address.

Both ``--rpc-addr`` and ``--allow-all-rpc`` switches can be used multiple times
in order to accommodate each specific setup.


.. _default_acl:

Default ACL for RPC
"""""""""""""""""""

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
attesting by bakers running on remote servers,
because endpoints such as ``/injection/block`` are not open remotely.
Rather than opening them remotely, the recommended practice for baking is to run a node locally listening to ``localhost``, with the default ACL policy.

The following is the default ACL policy for the node,
hard-coded in :src:`src/lib_rpc_http/RPC_server.ml` (remember to replace
``any.public.address`` with an IP address or a domain name that you'll be
actually listening on):

.. literalinclude:: default-acl.json
   :language: json

HTTP Caching Headers
~~~~~~~~~~~~~~~~~~~~

It is possible to enable http caching headers in the RPC responses with the
``--enable-http-cache-headers`` option. This feature is disabled by default.

When enabled, the RPC server will support `max-age <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Cache-Control#response_directives>`_
header. The header ``Cache-control: public, max-age: <duration>`` will be included in the response headers of head related
queries (``/chains/main/blocks/head*``) for responses that are cacheable. This also works on paths
that are relative to ``head`` such as ``head-n`` and ``head~n``. The response is cacheable throughout the ``<duration>``
of the head block's consensus round where ``<duration>`` is the remaining time until the :ref:`estimated end time<time_between_blocks>`
of the consensus round. If a response should not be cached, the RPC server will not include any cache control headers.

The RPC server will also support the conditional request header `If-None-Match <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/If-None-Match>`_
and include the ``ETag`` field in every head related query. The value of the ``ETag`` will be set to the block hash that the query
is related to. If the client sends a request with ``If-None-Match: <comma-separated list of ETag values>`` header and the block hash
is included in the list of etag values, the RPC server will respond with a ``304 Not Modified`` status code with an empty body.

This feature is useful when running the RPC server behind a reverse proxy that supports automatic
content caching (eg. `NGINX's proxy_cache setting <https://blog.nginx.org/blog/nginx-caching-guide>`_). Beware that
enabling this feature adds a non-negligible performance overhead (up to 10-15% slower) to every head related query
as the RPC server needs to perform additional checks and calculations. Consequently, it is advised to enable thie feature
only when operating the RPC server behind appropriate caching infrastructure.

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

By default, the node listens to incoming connections from peers on port ``9732``,
on any of the available network interfaces on the node's host.
This behavior can be changed by passing an IP address and optionally a port number
(``addr:port``), using either the ``--net-addr`` configuration option or the P2P
``listen-addr`` configuration parameter.
Note that the IP address is only used for selecting an active network interface
(technically, it is only passed to the ``bind()`` function of the socket API).
It is also possible to advertise to peers a different port than
the binding port using ``--advertised-net-port`` configuration option or the P2P
``advertised-net-port`` configuration parameter. Currently it is only possible to
specify an advertised port, but not an IP address.

.. note::

    If the node is run on a machine ``M`` not disposing of a public IP address,
    subject to NAT, a port forwarding rule has
    to be added on the NAT server ``S`` on the listening port towards
    machine ``M``: ``S:p1 -> M:p2``, where ``p1`` is ``advertised-net-port`` and
    ``p2`` is the port specified by ``listen-addr``. Alternatively, if the
    ``advertised-net-port`` is not configured, ``p1`` must be the same as ``p2``.

    As a consequence, if a second node has to be run behind the
    same server on a machine ``M'`` (possibly the same as ``M``),
    it should be configured to listen on a different port ``p2'``, to
    allow defining another forwarding rule for it: ``S:p1' -> M':p2'``, where
    ``p1'`` is ``advertised-net-port`` configured for the second node ``M'``.

    Many routers can be configured with UPnP to open ports dynamically, so the
    port forwarding can be initiated by the internal host without any manual
    modification on the router. This is not possible for corporate networks with
    UPnP disabled, but is typically handy for home routers, or other networks
    where this option is available. See :ref:`Mapping ports with UPnP<mapping_upnp>`.

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

   octez-node run --rpc-addr [::] --private-mode \
                                  --no-bootstrap-peers \
                                  --synchronisation-threshold=1 \
                                  --connections 1 \
                                  --peer <public-node-ip>


.. _configure_shell:

Shell parameters
----------------

Configuration options/parameters for the shell allow tuning the
working of the :doc:`validation subsystem <../shell/validation>`.

In particular, the synchronization heuristics implemented by the chain
validator can be controlled using parameters such as the
synchronization threshold or the latency, described in the
documentation of the :doc:`synchronization heuristics
<../shell/sync>`.

Configuration parameters for the context's storage can also be done
through environment variables, see :ref:`context_component`.
