Various
=======

.. _private-mode:

Private node
------------

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


.. _node-conf:

Configuration options for the node
----------------------------------

::

   ./tezos-node config init

This will initialize a configuration file for the node in
`$HOME/.tezos-node/config.json`, using default values. It only
specifies that the node will listen to incoming connections on socket
address ``[::]:9732``.

The easiest way to amend this default configuration is to use

::

   # Update the config file
   ./tezos-node config update <…>
   # Start from an empty cfg file
   ./tezos-node config reset <…>


All blockchain data is stored under ``$HOME/.tezos-node/``.  You can
change this by doing `./tezos-node config update --data-dir
</somewhere/in/your/disk>`.

To run multiple nodes on the same machine, you can duplicate and edit
``$HOME/.tezos-node/config.json`` while making sure they don't share
the same ``data-dir``. Then run your node with `./tezos-node
run --config-file=</path/to/alternate_cfg>`.

Here is an example configuration file with all parameters specified.
Most of the time it uses default values, except for cases where the
default is not explanatory enough (i.e. “bootstrap-peers” is an empty
list by default). Comments are not allowed in JSON, so this
configuration file would not parse. They are just provided here to help
writing your own configuration file if needed.

::

    {

      /* Location of the data dir on disk. */

      "data-dir": "/home/tezos/my_data_dir"

      /* Configuration of net parameters */

      "net": {

        /* Floating point number between 0 and 256 that represents a
        difficulty, 24 signifies for example that at least 24 leading
        zeroes are expected in the hash. */

        "expected-proof-of-work": 24.5,

        /* List of hosts. Tezos can connect to both IPv6 and IPv4
        hosts. If the port is not specified, default port 9732 will be
        assumed. */

        "bootstrap-peers": ["::1:10732", "::ffff:192.168.1.3:9733", "mynode.tezos.com"],

        /* Specify if the node is in private mode or not. A node in
        private mode only opens outgoing connections to peers whose
        addresses are in [trusted_peers] and only accepts incoming
        connections from trusted peers. In addition, it informs these
        peers that the identity of the node should not be revealed to
        the rest of the network. */

        "private-mode": false,

        /* Network limits */

        "limits": {

          /* Delay granted to a peer to perform authentication, in
          seconds. */

          "authentication-timeout": 5,

          /* Strict minimum number of connections (triggers an urgent
          maintenance). */

          "min-connections": 50,

          /* Targeted number of connections to reach when bootstrapping /
          maintaining. */

          "expected-connections": 100,

          /* Maximum number of connections (exceeding peers are
          disconnected). */

          "max-connections": 200,

          /* Number above which pending incoming connections are
          immediately rejected. */

          "backlog": 20,

          /* Maximum allowed number of incoming connections that are
          pending authentication. */

          "max-incoming-connections": 20,

          /* Max download and upload speeds in KiB/s. */

          "max-download-speed": 1024,
          "max-upload-speed": 1024,

          /* Size of the buffer passed to read(2). */

          "read-buffer-size": 16384,
        }
      },

      /* Configuration of rpc parameters */

      "rpc": {

        /* Host to listen to. If the port is not specified, the default
        port 8732 will be assumed. */

        "listen-addr": "localhost:8733",

        /* Cross Origin Resource Sharing parameters, see
        https://en.wikipedia.org/wiki/Cross-origin_resource_sharing. */

        "cors-origin": [],
        "cors-headers": [],

        /* Certificate and key files (necessary when TLS is used). */

        "crt": "tezos-node.crt",
        "key": "tezos-node.key"
      },

      /* Configuration of log parameters */

      "log": {

        /* Output for the logging function. Either "stdout", "stderr" or
        the name of a log file . */

        "output": "tezos-node.log",

        /* Verbosity level: one of 'fatal', 'error', 'warn', 'notice',
        'info', 'debug'. */

        "level": "info",

        /* Fine-grained logging instructions. Same format as described in
        `tezos-node run --help`, DEBUG section. In the example below,
        sections "net" and all sections starting by "client" will have
        their messages logged up to the debug level, whereas the rest of
        log sections will be logged up to the notice level. */

        "rules": "client* -> debug; net -> debug; * -> notice",

        /* Format for the log file, see
        http://ocsigen.org/lwt/dev/api/Lwt_log_core#2_Logtemplates. */

        "template": "$(date) - $(section): $(message)"
      },

      /* Configuration for the validator and mempool parameters */

      "shell": {

         /* The number of peers to synchronize with
            before declaring the node 'bootstrapped'. */

         "synchronisation_threshold": 4,

         /* Latency in seconds used for the synchronisation
         heuristic. */

         "latency": 120,

         /* The history mode configuration you want to run. */
         "history_mode": "full"

      }
    }


Environment for writing Michelson contracts
-------------------------------------------

Here is how to setup a practical environment for
writing, editing and debugging Michelson programs.

Install `Emacs <https://www.gnu.org/software/emacs/>`_ and configure
it to use the `MELPA <https://melpa.org/#/getting-started>`_ package
repository.

Inside Emacs, install the ``michelson-mode`` package and its
dependency `deferred <https://github.com/kiwanami/emacs-deferred>`_ by
running ``M-x package-install-file``; the package file is located in
the ``emacs`` folder of the Tezos code base.

Set up the `Michelson mode
<https://gitlab.com/tezos/tezos/tree/master/emacs>`_ to use the Tezos
client in :ref:`mockup mode<mockup-mode>` (to typecheck Michelson
scripts without interacting with a Tezos node) by adding in your
``.emacs`` file:

::

   (setq michelson-client-command "tezos-client --base-dir /tmp/mockup --mode mockup --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK")
   (setq michelson-alphanet nil)

Note that the Michelson mode will be chosen automatically by Emacs for
files with a ``.tz`` or ``.tez`` extension.

We can now open our favourite contract ``emacs
./src/bin_client/test/contracts/attic/id.tz`` and, when moving the cursor on
a Michelson instruction, in the bottom of the windows Emacs should
display the state of the stack before (left) and after (right) the
application of the instruction.
The Emacs mode automatically type-checks your program and reports
errors; once you are happy with the result you can ask the client to
run it locally:

::

   tezos-client run script ./src/bin_client/test/contracts/attic/id.tz \
                on storage '"hello"' and input '"world"'

.. _configure_logging:

Logging
-------

It is possible to set independent log levels for different components in the Tezos node, as well as specifying an output file for logging.
This can be done in two different ways:

- by defining log parameters in the configuration file (see :ref:`node-conf`)
- by setting the environment variable ``TEZOS_LOG`` before starting the node.

The list of the Tezos components that can be logged and the syntax for the logging options can be found in the DEBUG section displayed by ``tezos-node run --help``:

.. literalinclude:: ../api/tezos-node-run.txt
    :start-after: DEBUG
    :end-before: OPTIONS

.. _tezos-admin-client:

Admin Client
------------

The admin client enables you to interact with the peer-to-peer layer in order
to:

- check the status of the connections
- force connections to known peers
- ban/unban peers

A useful command to debug a node that is not syncing is:

::

   tezos-admin-client p2p stat

.. _tezos_binaries_signals_and_exit_codes:

Tezos binaries: signals and exit codes
--------------------------------------

Signals:
Upon receiving ``SIGINT`` (e.g., via Ctrl+C in an interactive session) or
``SIGTERM`` (e.g., via ``systemctl stop``) the process will exit (with code 64 or
255, see details below). Note that sending the same signal a second time (after
a one (1) second grace period) will terminate the process immediately,
interrupting the normal clean-up functions of clean-up (in this case the exit
code will be 255).

Exit codes:
The meaning of exit codes is presented in the following table. The action column
indicates a recommended course of action.

+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| exit code   |  meaning                                                                         | action                                                           |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 0           | the process exited successfully                                                  | nothing                                                          |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 1–125       | something went unexpectedly                                                      | check output/log to see if you forgot an argument or some such   |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 126         | an exception was not handled                                                     | report a bug                                                     |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 127         | the process received a signal (e.g., via Ctrl-C)                                 | nothing                                                          |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 128         | the process was about to exit successfully but an error occurred during exit     | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 129–253     | like 1–125 and an error occurred during exit                                     | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 254         | like 126 and an error and an error occurred during exit                          | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 255         | like 127 but an error and an error occurred during exit (e.g., ``kill -9``)      | check output/logs, clean-up leftover files                       |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+

Failing_noop operation
----------------------

Starting with protocol 009 a `Failing_noop` operation is added. This operation
is not executable in the protocol and will always fail when injected. It allows
to sign an arbitrary string that cannot be misinterpreted in the protocol.

The client has commands to sign a message with a given key or to check that
message has been signed by a given key. These commands create a `failing_noop`
operation from the message that is being signed or checked.

::

   tezos-client sign message "hello world" for <account>

   tezos-client check that message "hello world" was signed by <account> to
   produce <signature>
