RPC handling architecture
=========================

.. figure:: images/rpc_components.png
   :alt: Components of Local and External RPC servers

The Cohttp library is a vendored component responsible
for handling HTTP requests and responses, providing the core functionality
for HTTP communication in both the Local and External RPC servers.

:src:`Resto<resto>` is a library for declaratively defining services,
binding them to given paths, and then either starting an RPC server
to serve the RPCs on these paths or making RPC calls to these services.
For monitoring requests, a stream is created and updates are sent out
as soon as new data is available.
Resto is responsible for the following:

- Providing primitives to describe services.
- Assembling the services into directories which are essentially maps of paths
  and methods to services.
- Spinning up a Cohttp server that serves the chosen directory.
- Making requests to services as a client. The client automatically builds
  paths based on parameters to the service, assembles other HTTP details,
  and parses the response.

Additionally, Resto provides features for configuring ACL and for serving
a self-description service - a service that describes all services of a directory.

In the rest of the document, we will use the term "Resto server" (as it is
called in the ``resto`` library), but it is worth clarifying that the Cohttp
server is the actual server which listens for events. Resto server is sitting
on top of Cohttp server, providing its handlers for events. Resto server is
not listening for events on its own but it handles the events.
So they split the functionality:

- Cohttp listens and involves event handlers
- Resto provides handlers for events

The :src:`RPC middleware<src/lib_rpc_http/RPC_middleware.ml>` module in
the External RPC server receives accepted connections
from the Resto server. Depending on the RPC type, it either handles the
underlying RPC request locally or forwards it to the Local RPC server next to the Tezos
node (they share the same PID) by initiating a connection to it. When forwarding, the RPC middleware
maintains a mapping between the accepted and the initiated connections. If
the client of the initial RPC request dies or closes a connection, the RPC middleware is notified by
Resto and then closes the corresponding initiated connection to the Local
RPC server.

RPC server initialization
~~~~~~~~~~~~~~~~~~~~~~~~~

Local RPC server and External RPC server are initialized at Tezos node start in
:src:`src/bin_node/node_run_command.ml`. Each of them calls ``launch`` in
:src:`src/lib_rpc_http/RPC_server.ml` for a Resto server.

An RPC server is declared in :src:`src/lib_rpc_http/RPC_server.ml` by ``Resto_cohttp_server.Server.Make``.
At launch, Resto RPC server takes 2 callbacks:

- ``callback`` - the main callback for accepted connections
- ``conn_closed`` - the callback notifying about the connection closed
  externally, e.g. by EOF

Resto server launches Cohttp server which also takes ``callback`` and
``conn_closed`` plus one more callback:

- ``sleep_fn`` - a callback implementing sleep with desired timeout which
  is used to listen for EOF while the main callback is unresponsive handling
  the request.

Resto provides a default ``callback`` implementation, ``resto_callback`` in
:src:`resto/src/server.ml`, which serves Resto directory.

The forwarding logic is handled in file :src:`src/lib_rpc_http/RPC_middleware.ml`
which defines ``proxy_server_query_forwarder``.

Connection management
~~~~~~~~~~~~~~~~~~~~~

The Client, Baker or Indexer can initiate RPC requests. As a result, a TCP connection is established to the Tezos Node or to the External RPC server depending on the settings in the Client, Baker or Indexer.

The External RPC server decides based on the RPC endpoint type if the request has to be handled locally or should be forwarded to the Tezos node. For performance reasons External RPC server tries to offload Tezos node.

If the request is forwarded, RPC middleware initiates a new request that is sent to the Tezos node via an initialized Unix connection, using Cohttp client. From now on, Cohttp client needs to monitor the connection health to notify the Client/Baker/Indexer if the connection died.

This becomes critical as some requests are endless (e.g. ``/monitor/heads/main`` or ``/chains/main/mempool/monitor_operations``). The Client/Baker/Indexer have no timeout for that and the responsibility of keeping these connections healthy is on External RPC server.

The Cohttp server monitors the health of the Unix connection. If EOF is received, RPC middleware is notified. Then it closes the connection towards the Client/Baker/Indexer. See Cohttp functions ``handle_request()`` in :src:`cohttp/cohttp-lwt/src/server.ml` and ``wait_eof_or_closed`` in :src:`cohttp/cohttp-lwt-unix/src/io.ml`.

Cohttp uses Conduit library for low-level operation with connections. The Unix file descriptor of the connection is kept within Cohttp and is hidden for higher levels of the stack which makes debugging quite complicated. Cohttp server passes the closing function to the Resto server allowing it to close the connection if the forwarding operation failed.

The RPC server provides its callbacks to Cohttp which actually starts a server, receives requests and involves the provided callbacks. RPC server launches Cohttp as ``RPC_server.launch`` in :src:`src/lib_rpc_process/main.ml`. The callback ``conn_closed`` takes a connection ID. This connection ID is provided by Cohttp when the connection is created and then Resto stores it in forwarder resources. Resto creates a new forwarded connection in ``make_transform_callback`` and stores the connection ID and a closing function in ``forwarder_resources`` in :src:`src/lib_rpc_http/RPC_middleware.ml`.

The number of callbacks is confusing. So let’s take a closer look to :src:`src/lib_rpc_http/RPC_middleware.ml`:

- Cohttp runs a server. So a connection request received from Client/Baker/Indexer/ arrives at Cohttp and then passed to Resto via  ``make_transform_callback``.
- Resto passes the callback to Cohttp at server start to deal with a connection which has to be closed.
  As it is provided at server start, it takes connections storage and a connection ID as an input.
  The provided callback is essentially ``forwarding_conn_closed`` as it handles dead forwarded Unix connection.
  Therefore, Resto keeps a mapping of connections from Client/Baker/Indexer to the shutdown function on the corresponding forwarded connections towards the Tezos node.
  So if a client dies, the connection towards Tezos Node is also closed.

So when a connection from Client/Baker/Indexer is received, Resto is involved via  ``make_transform_callback``. And if that connection dies, Cohttp invokes ``forwarding_conn_closed``. If the connection is handled locally by External RPC server, Resto does nothing. If the request was forwarded, Resto will call ``shutdown`` for the Unix connection towards Tezos node.

Debugging
~~~~~~~~~

If you want to learn more about the exchange of RPCs between node and
client you can pass the option ``-l`` and the client will print all the
calls with their input/output.

A useful tool to manipulate JSON is `jq <https://stedolan.github.io/jq/>`_.

To enable the logs for RPC-related components, prepend Tezos scripts
with ``TEZOS_LOG="*->debug"`` and ``COHTTP_DEBUG=true``.
