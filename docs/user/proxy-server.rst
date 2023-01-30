Proxy server
------------

This page describes the *proxy server*, a readonly frontend to ``octez-node``
which is designed to lower the load of full nodes. It can be run separately from
a node and will handle some RPC requests by itself. It is named after two
things:

* The :doc:`proxy mode<proxy>` of the client on which it builds upon.
* Regular HTTP proxies, as proxy servers are meant to be deployed
  in front of a node, to lower the node's load.

Even though the proxy server only serves a subset of the RPCs a full node can serve (detailed :ref:`below <what_the_proxy_server_serves>`), it can transparently act as a replacement for a full node, by redirecting to the node the HTTP requests that it cannot handle itself, as described :ref:`below <unsupported_rpcs>`.

Launching a proxy server
~~~~~~~~~~~~~~~~~~~~~~~~

The proxy server is implemented by the ``octez-proxy-server`` executable.
The minimal arguments to the proxy server are ``--endpoint``
and ``--rpc-addr``:

* ``--endpoint`` specifies the URL of the RPC server of the underlying node, that is uses
  for obtaining data (via RPCs of the form
  ``/chains/<chain_id>/blocks/<block_id>/context/raw/bytes``),
  and for redirecting the requests it cannot handle.
* ``--rpc-addr`` specifies the URL that the proxy server should serve.

An important option is ``--data-dir``:

* ``--data-dir`` specifies the directory of the node from
  which to obtain data directly (without RPCs). Of course, this option can only be used when the proxy server
  has access to the node's data directory.
  In this case, the proxy server will reduce the number of RPC calls to the
  node, thereby reducing its I/O consumption.

The full command-line API is detailed :ref:`below <additional_arguments>`.

.. _sandbox_example:

Examples with the sandbox
~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we show examples of using a proxy server in
the :doc:`sandboxed node<sandbox>`. For convenience, we repeat
instructions for the sandboxed mode here, but refer the reader to the
sandboxed mode page for further details.

In a first terminal, start a sandboxed node:

::

    $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1
      April 21 11:05:32.789 - node.config.validation: the node configuration has been successfully validated.
      Created /tmp/octez-node.Uzq5aGAN/config.json for network: sandbox.
      ...

Note in the trace above that the sandbox node wrote some configuration file in a working directory, that you may need to know later on.

Leave this first terminal running. In a second terminal, prepare the appropriate
environment for running a proxy server:

::

    $ eval `./src/bin_client/octez-init-sandboxed-client.sh 1`
    $ octez-activate-alpha
    $ octez-client bake for bootstrap1

To avoid warnings being printed in upcoming commands (optional):

::

    $ export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y

You're now ready to run a proxy server, as shown below.
We specify debug variables in ``TEZOS_LOG`` to see what the proxy server
is doing (see the :doc:`proxy mode<proxy>` page for more details).

::

    $ export TEZOS_LOG="proxy_rpc_ctxt->debug; proxy_rpc->debug; proxy_server_run->debug; proxy_getter->debug; proxy_services->debug"
    $ ./octez-proxy-server --endpoint http://127.0.0.1:18731 --rpc-addr http://127.0.0.1:18732
      Apr 21 11:09:22.092 - proxy_server_run: starting proxy RPC server on 127.0.0.1:18732

Now, start a third terminal, and ask the client to request data from the proxy server:

::

    $ export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y
    $ ./octez-client --endpoint http://127.0.0.1:18732 rpc get /chains/main/blocks/head/context/contracts
      [ "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv" ]

In the proxy server's terminal, you should see this output (tree sizes may vary):

::

    Apr 21 11:10:07.474 - proxy_rpc: chains/<main>/blocks/<head>/header
    Apr 21 11:10:07.474 - proxy_rpc: proxy cache created for chain main and block head
    Apr 21 11:10:07.476 - proxy_getter: Cache miss: (v1/constants)
    Apr 21 11:10:07.476 - proxy_getter: split_key heuristic triggers, getting v1 instead of v1/constants
    Apr 21 11:10:07.476 - proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/v1
    Apr 21 11:10:07.477 - proxy_rpc: received tree of size 2
    Apr 21 11:10:07.477 - proxy_getter: Cache hit: (v1/cycle_eras)
    Apr 21 11:10:07.477 - proxy_getter: Cache miss: (pending_migration_balance_updates)
    Apr 21 11:10:07.477 - proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/pending_migration_balance_updates
    Apr 21 11:10:07.477 - proxy_getter: Cache miss: (pending_migration_operation_results)
    Apr 21 11:10:07.477 - proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/pending_migration_operation_results
    Apr 21 11:10:07.478 - proxy_getter: Cache miss: (contracts/index)
    Apr 21 11:10:07.478 - proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/contracts/index
    Apr 21 11:10:07.479 - proxy_rpc: received tree of size 115

Lines of the form ``proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/...``
show requests that the proxy server sends to the node to obtain data.

Now, in the third terminal, retrieve the contracts again:

::

    $ ./octez-client --endpoint http://127.0.0.1:18732 rpc get /chains/main/blocks/head/context/contracts
      [ "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv" ]
    $ ./octez-client --endpoint http://127.0.0.1:18732 rpc get /chains/main/blocks/head/context/contracts
      # ... same output ...

In the meantime, in the proxy server's terminal, you should see:

::

    Apr 21 11:14:06.511 - proxy_getter: Cache hit: (v1/constants)
    Apr 21 11:14:06.512 - proxy_getter: Cache hit: (v1/cycle_eras)
    Apr 21 11:14:06.512 - proxy_getter: Cache hit: (pending_migration_balance_updates)
    Apr 21 11:14:06.512 - proxy_getter: Cache hit: (pending_migration_operation_results)
    Apr 21 11:14:06.512 - proxy_getter: Cache hit: (contracts/index)

This show that the proxy server is answering the request
without delegating anything to the node: there is no ``proxy_rpc`` line.
The proxy server is reusing the data it obtained for ``<head>`` from
the first request.

Reducing RPC calls: ``--data-dir``
""""""""""""""""""""""""""""""""""

To make the proxy server read the node's data-dir instead of doing
``/chains/<main>/blocks/<head>/context/raw/bytes`` RPC calls, kill
the proxy server you have launched :ref:`above <sandbox_example>`),
and restart it as follows:

::

    $ ./octez-proxy-server --endpoint http://127.0.0.1:18731 --rpc-addr http://127.0.0.1:18732 --data-dir <node_data_dir>
      protocol of proxy unspecified, using the node's protocol: ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
      Apr 21 11:09:22.092 - proxy_server_run: starting proxy RPC server on 127.0.0.1:18732

You can obtain the value for the ``--data-dir`` argument by looking at the
output of the terminal where ``octez-node`` was launched
(see :ref:`above <sandbox_example>`).

Now, in the third terminal (the client's terminal), redo the request
to retrieve contracts:

::

    $ ./octez-client --endpoint http://127.0.0.1:18732 rpc get /chains/main/blocks/head/context/contracts
      # ... same output as above ...

Now the output in the proxy server terminal should be:

::

    Apr 21 11:22:44.359 - proxy_rpc: chains/<main>/blocks/<head>/header
    Apr 21 11:22:44.360 - proxy_rpc: proxy cache created for chain main and block head

There are far fewer ``proxy_rpc`` lines! That is because the proxy
server obtained its data by reading the node's data-dir, instead of performing RPC calls.

.. _additional_arguments:

Additional arguments
~~~~~~~~~~~~~~~~~~~~

We describe the entire list of options and arguments of the proxy server, also available in more concise form in the :ref:`proxy_server_manual`:

* ``-c`` and ``--config`` specify the JSON file to use an input
  for the configuration. This JSON file is an object like this:
  ``{"endpoint": "http://127.0.0.1:18731", "rpc_addr": "http://127.0.0.1:18732", "sym_block_caching_time": 60}``.
  This file can specify all command line arguments except ``-l``/``--log-requests``.
  If an argument if specified both in the configuration file and on the command line,
  the command line takes precedence.
* ``-d`` and ``--data-dir`` specify the path of the data directory of
  the node. If specified, the proxy server obtains data by reading the disk
  instead of performing the ``/chains/<chain_id>/blocks/<block_id>/context/raw/bytes``
  RPC. If possible (i.e. if the proxy server can access the node's
  disk), this option should be used, because it reduces IO consumption
  of the node.

  Note that this argument doesn't make ``--endpoint`` optional, because the
  proxy server still needs to do RPC calls to obtain block headers. Further
  work removing all RPC calls is described in issue
  `2502 <https://gitlab.com/tezos/tezos/-/issues/2502>`_.
* ``-E`` and ``--endpoint`` specify the URL of the RPC server of the node
  to do requests to obtain data (RPCs of the form
  ``/chains/<chain_id>/blocks/<block_id>/context/raw/bytes``).
* ``-l`` and ``--log-requests`` specify to print the requests that are
  delegated to the node, in a verbose manner.
* ``--rpc-addr`` specifies the URL that the proxy server should serve.
* ``--rpc-tls`` specifies that the proxy server must use TLS. It should
  be a string of the form ``crt_file,key_file`` where ``crt_file`` is the path
  to the TLS certificate to use and ``key_file`` is the path to the key
  to use.
* ``--sym-block-caching-time`` specifies
  the duration during which data for a symbolic block identifier
  (like ``head``, ``head~1``) is kept. Smaller values increase the endpoint's
  load but yield more up-to-date to clients. Higher values
  decrease the endpoint's load but make clients observe slightly deprecated
  values. If omitted, the value is defaulted to ``time_between_blocks``. As
  ``time_between_blocks`` is hence regularly requested from the node, this incurs
  a higher load of the node.

All these options can either be specified in the configuration
file or on the command line. However, the union of the configuration file
and the command line should specify the endpoint to use and the RPC address to serve.

.. _what_the_proxy_server_serves:

Supported RPCs
~~~~~~~~~~~~~~

The proxy server itself only serves protocol-specific RPCs (listed
`here <https://tezos.gitlab.io/alpha/rpc.html#protocol-alpha>`_ for protocol Alpha),
but not all of them: since the proxy server is a readonly frontend for the
underlying node, it only serves the readonly requests (``GET`` requests, as
well as a subset of the ``POST`` requests).

Because computations done by the proxy server are protocol-dependent, the proxy mode must choose a specific protocol: the same as the underlying node.
However, the proxy mode does not support all protocols.
Execute ``octez-client list proxy protocols`` to see the supported protocols.
It is expected that, at any
given time, the proxy server supports ``Alpha``, the current protocol
of Mainnet and the current protocol proposal on Mainnet at the time of release.

.. _unsupported_rpcs:

Unsupported RPCs
~~~~~~~~~~~~~~~~

Requests that are not readonly can only be handled by a full node. However, the proxy server accepts any RPC: if the RPC is not supported
by the proxy server, it will redirect clients to the appropriate endpoint on the
underlying node using an HTTP redirect (``301 Moved Permanently``), and the node
will then handle the request.

This can be easily demonstrated with a simple test: start a proxy server, and
make a request to it with ``curl -vL <proxy server endpoint>/<any node-only RPC>``.
For example::

    curl -vL http://127.0.0.1:18732/chains/main/blocks/head/header

The output
from ``curl`` will show that the proxy server asks curl to follow a redirect to
the node's endpoint, which it will do because of the ``-L`` flag, and
then it is finally responded to by the node. Any RPC that can be handled by the
proxy server itself will of course not show this behaviour.

Clearly, making such requests to the proxy server does not decrease the load of
the node. However, it does allow the
use of a single endpoint for all RPC requests, which may be convenient for
some use cases.
In turn, it adds a slight delay to the HTTP
request, unless the redirect is cached by the client.

Deployment
~~~~~~~~~~

As a proxy server is a readonly frontend to a node, you can spawn multiple
proxy servers in front of a single node.

As described above, the proxy server configures himself to the same protocol as the underlying node. As a consequence,
when the underlying node changes protocol, the proxy server will also switch to the new protocol, **unless** the proxy server executable does not contain the new protocol.
This may happen, for instance, if the executable was compiled before this protocol was even injected.
As there is no dynamic linking of new protocols in the proxy server, it will
start failing for RPCs
concerning blocks of the new protocol.
The solution in this case is to restart the proxy server using a more recent executable.

More generally, we recommend to automatically
restart proxy servers that have a high ratio of failures.
Restarting a proxy server is always fine; they can be thrown away at any
moment.

Heuristics
~~~~~~~~~~

The proxy server uses several heuristics to optimize its own work and/or decrease the node's load.
For example, there is a heuristic
to make big map queries faster, which is useful when many queries to siblings keys of a given
big map are done in burst.

The heuristics for protocol Alpha are implemented in file
:src:`proxy.ml <src/proto_alpha/lib_client/proxy.ml>`, in function ``split_key`` and associates. For example,
any request of the form ``big_maps/index/i/contents/tail`` is transformed
into a request of the form ``big_maps/index/i/contents`` to obtain data for all
possible values of ``tail`` at once.
For the moment, the heuristics cannot be specified on the command line, but
this can be implemented in the future.
