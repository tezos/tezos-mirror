Proxy mode
----------

The ``octez-client``, described in
:ref:`a dedicated tutorial <howtouse_tezos_client>`, heavily relies on node RPCs to implement its features. Thus, when a client need to perform some computation which cannot be done entirely locally, but which is implemented by a node RPC, it will simply call the corresponding RPC.

The current page describes the *proxy* mode, an execution mode where the client
avoids some RPC calls to the node, especially computation-intensive RPCs.
Thus, for computations that cannot be done entirely locally,
the proxy client only requests the data it needs from the node using RPCs (that are not computation-intensive), and uses
this data locally to perform computations by itself, whenever possible.

For an entirely local mode, which never calls node RPCs, see the :doc:`mockup mode <mockup>`.

Typical examples of potentially long computations the proxy mode relieves
the node of include estimating gas required by a smart contract call or asking for baking rights.

Executing commands in proxy mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The CLI interface of the client in proxy mode (the *proxy client* in short)
is the same as the default client. To turn proxy mode ON,
simply pass option ``--mode proxy`` to ``octez-client``.

Because some computations usually done by the node are protocol-dependent, the proxy mode has to be configured for a specific protocol.
However, the proxy mode does not support all protocols.
Execute ``octez-client list proxy protocols`` to see the supported protocols.
It is expected that, at any given time, the proxy mode supports ``Alpha``,
the current protocol of Mainnet, and the current protocol proposal on Mainnet
at the time of release, if any.

Examples with the sandbox
~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we show examples of using the proxy mode with
a :doc:`sandboxed node <sandbox>`. For convenience, we repeat
instructions for the sandboxed mode here, but we refer the reader to the
sandboxed mode page for further details. In a terminal,
start a sandboxed node:

::

    $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1

Leave that terminal running and, in another terminal, prepare the appropriate
environment for using the proxy client:

::

    $ eval `./src/bin_client/octez-init-sandboxed-client.sh 1`

Then upgrade the node to protocol alpha:

::

    $ octez-activate-alpha
    $ octez-client bake for bootstrap1

To avoid warnings being printed in upcoming commands (optional):

::

    $ export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y

You're now ready to use the proxy client. For example, request baking rights:

::

    $ octez-client --mode proxy rpc get /chains/main/blocks/head/helpers/baking_rights
    [ { "level": 3, "delegate": "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv",
        "round": 0, "estimated_time": "2022-11-17T14:20:17Z",
        "consensus_key": "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv" },
      { "level": 3, "delegate": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "round": 1, "estimated_time": "2022-11-17T14:20:18Z",
        "consensus_key": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" },
      { "level": 3, "delegate": "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "round": 2, "estimated_time": "2022-11-17T14:20:20Z",
        "consensus_key": "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv" },
      { "level": 3, "delegate": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "round": 3, "estimated_time": "2022-11-17T14:20:23Z",
        "consensus_key": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" },
      { "level": 3, "delegate": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        "round": 4, "estimated_time": "2022-11-17T14:20:27Z",
        "consensus_key": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" } ]

Well, that doesn't seem very different from what the default client would return.
Indeed, it's the same; that was the point! To see what the proxy client
is doing differently, you may set up debugging on the following event sections
(see :doc:`./logging`):

* ``proxy_rpc_ctxt`` shows whether the proxy client is delegating RPCs
  to the node or doing them locally. This is the main debug section.
* ``proxy_rpc`` shows the RPCs that the proxy client is performing to retrieve
  up-to-date data from the node.
  This debug section is useful to understand
  if the proxy is performing slowly. It might be because it is performing
  a lot of such RPCs.

For convenience, let's define an alias before continuing, to save
keystrokes and the ``protocol of proxy unspecified`` warning:

::

    $ alias proxy-client="octez-client --mode proxy --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

Now configure ``proxy_rpc_ctxt`` to have more information:

::

    $ export TEZOS_LOG="proxy_rpc_ctxt->debug"

And redo the same RPC as before:

::

    $ proxy-client rpc get /chains/main/blocks/head/helpers/baking_rights
    Nov 17 15:21:19.959 - proxy_rpc_ctxt: delegating to http: GET call_service version
    Nov 17 15:21:19.967 - proxy_rpc_ctxt: locally done: GET call_service chains/<chain_id>/blocks/<block_id>/protocols
    Nov 17 15:21:19.969 - proxy_rpc_ctxt: locally done: GET call_service describe/<string>
    Nov 17 15:21:19.976 - proxy_rpc_ctxt: locally done generic media type call: GET
    Nov 17 15:21:19.976 - proxy_rpc_ctxt:   /chains/main/blocks/head/helpers/baking_rights
    [ { "level": 3, "delegate": "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv",
        "round": 0, "estimated_time": "2022-11-17T14:20:17Z",
        "consensus_key": "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv" },
      { "level": 3, "delegate": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "round": 1, "estimated_time": "2022-11-17T14:20:18Z",
        "consensus_key": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" },
      { "level": 3, "delegate": "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "round": 2, "estimated_time": "2022-11-17T14:20:20Z",
        "consensus_key": "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv" },
      { "level": 3, "delegate": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "round": 3, "estimated_time": "2022-11-17T14:20:23Z",
        "consensus_key": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" },
      { "level": 3, "delegate": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        "round": 4, "estimated_time": "2022-11-17T14:20:27Z",
        "consensus_key": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" } ]

In this case, the bulk of the computation is done locally.

If you also want to see the data requests to the node, do the following before running your commands::

    $ export TEZOS_LOG="proxy_rpc_ctxt->debug; proxy_rpc->debug"

How to deploy to relieve nodes from some RPCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using proxy clients can reduce the load of nodes, by having clients
perform more computations locally. For this to work, however,
deployment should be done in a specific manner, which this section describes.

While the proxy mode has been designed to reduce the load of nodes,
it doesn't suffice on its own to achieve this goal. Because proxy clients
perform many calls to the
``/chains/<chain>/blocks/<block>/context/raw/bytes/`` RPC
(and because the payload of doing an RPC call is a bit too high at the
moment), the node's load doesn't decrease. It pretty much stays the
same: for example, when benchmarking the time spent honoring
``rpc get /chains/main/blocks/head/helpers/baking_rights?&all=true``,
a node serving proxy clients spends its time serving ``../raw/bytes``
instead of serving ``../baking_rights?&all=true``.

To reduce the load of a node in presence of proxy clients,
deployment should be done as follows (we suppose there's a single node
for simplicity):

* Deploy the node as usual
* In front of the node, put multiple HTTP caches (let's avoid the
  term proxy here, to disambiguate with the proxy client) that cache
  the following RPCs:

  * ``/chains/<chain>/blocks/<block_id>/context/raw/bytes/``
  * ``/chains/<chain>/blocks/<block_id>/protocols``
  * ``/chains/<chain>/blocks/<block_id>/header``

  Intercepting ``../raw/bytes`` is required because proxy clients
  call it a lot, as described above.

  Intercepting ``../header`` is recommended, because the proxy client
  calls this RPC when it starts honoring a request locally, i.e.
  when it starts performing a computation that would happen
  on the node with a regular client.

  Note that it is safe to cache these RPCs, because the corresponding data
  is immutable (if it's there it won't change in the future).

Regarding clients, either:

* Use proxy clients
* Or intercept requests of regular clients, and honor them by spawning
  proxy clients on the fly, in front of the setup described in the previous
  list.

We refer to the proxy mode's
`merge request <https://gitlab.com/tezos/tezos/-/merge_requests/1943>`_
for details regarding how we did the benchmarks that led us to the conclusions
of this section.
