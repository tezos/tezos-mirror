JSON/RPC interface
==================

The Octez node provides a JSON/RPC interface. Note that it is an RPC
interface, and it is JSON based, but it does not follow the “JSON-RPC”
protocol. It is not active by default and it must be explicitly
activated with the ``--rpc-addr`` option. In that case, an RPC server
is started by the node.

As an example, if you are not trying to run a public RPC node, but you
just want to explore the RPC interface on your own, you would run:

::

    ./octez-node run --rpc-addr localhost

The RPC interface is self-documented and the ``octez-client`` executable
is able to pretty-print the RPC API. For instance, to see the API
provided by the Octez Shell:

::

    ./octez-client rpc list

To get API attached to the “genesis” block, including the remote
procedures provided by the associated economic protocol version:

::

    ./octez-client rpc list /chains/main/blocks/genesis

You might also want the JSON schema describing the expected input and
output of a RPC. For instance:

::

    ./octez-client rpc schema get /chains/main/blocks/genesis/hash

Note: you can get the same information, but as a raw JSON object, with a
simple HTTP request:

::

   curl -s localhost:8732/chains/main/blocks/head~10
   wget -O - http://localhost:8732/describe?recurse=true
   wget -O - http://localhost:8732/describe/chains/main/blocks/genesis?recurse=true
   wget -O - http://localhost:8732/describe/chains/main/blocks/genesis/hash


An online :doc:`index <../active/rpc>` of RPC calls is
also available.

The general call of an RPC from the client is ``octez-admin-client rpc
(get|post) <url>``.
For instance, if you wish to request the current balance of a given
block and contract, you can call the associated RPC via the command :
``$ octez-admin-client rpc get
/blocks/<block_id>/proto/context/contracts/<contract_id>/balance``.

An RPC may take an *input* and generate an *output* both in JSON
format. For example, the previous RPC call, that does not require an
input, would display on the standard output : ``{ "balance":
"4000000000000" }``. When calling a RPC that requires an input
through command-line, you will be prompted to provide the JSON input
in your default configured text editor. Alternatively, you can provide
the JSON input using command
``$ octez-admin-client rpc post <url> with <JSON>``. Don't forget to quote
the JSON according to your shell rules.

If you want to learn more about the exchange of RPCs between node and
client you can pass the option ``-l`` and the client will print all the
calls with their input/output.

A useful tool to manipulate JSON is `jq <https://stedolan.github.io/jq/>`_.

Running the RPC server in a side process (experimental feature)
---------------------------------------------------------------

It is also possible to run an RPC server that is external to the node,
as a separate process. To do so, simply use ``--external-rpc-addr``
(instead of ``--rpc-addr``) in the command lines.

Thanks to this feature that is particularly suitable for service
providers, the node won't experience slowdowns on computationally
intensive RPC calls. This prevents the node from stopping
synchronization with its peers or becoming out of sync. Additionally,
performance is expected to be slightly increased in terms of requests
handled per second. A benchmark framework, that can be found in the
:src:`devtools/benchmarks-tools/bench_RPS/rps.sh` script of the Octez
repository, allows to run performance evaluations easily. Along to
this benchmark framework, former results are stored in the dedicated
:src:`devtools/benchmarks-tools/bench_RPS/results.json` file.

In turn, this comes with an increased latency for RPCs related to
block injection, operation simulation or mempool operations.

Thus, the ``--external-rpc-addr`` option is particularly suitable for
service providers, but should nod be activated in baking setups.

.. warning::
   It is not recommended to use ``--external-rpc-addr`` yet, as the
   external RPC server is still an experimental feature.

RPC versions
------------

See :doc:`../introduction/versioning` and :ref:`RPC-versioning-dev`.
