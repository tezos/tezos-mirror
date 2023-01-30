Light mode
----------

The proxy mode, described in :doc:`a dedicated tutorial <./proxy>`,
is an execution mode where the :ref:`Octez client <howtouse_tezos_client>`
avoids some RPC calls to the node, especially computation-intensive RPCs.
It does so by requesting the data it needs from the node using RPCs (that are not computation-intensive), and uses
this data locally to perform computations by itself, whenever possible.

This page describes the *light* mode, a variant of the proxy mode,
where the client also
performs RPCs locally whenever possible.
However, contrary to the proxy mode, the light mode provides
a high level of security. For that, it obtains its data from multiple
(hopefully unrelated) node endpoints, and makes sure that all endpoints send
the same data, by using *Merkle proofs*. Such proofs make it very hard
for unrelated endpoints to craft fake data.

This mode is akin to a light client or *thin client* in Bitcoin terms.

While the existing implementation of the light mode is entirely functional,
it still has room for improvement. For instance, communications over
the network can be reduced. Users are encouraged to share their experience,
by submitting issues `here on GitLab <https://gitlab.com/tezos/tezos/-/issues>`_.

Executing commands in light mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The CLI interface of the client in light mode (the *light client* for short)
is the same as the default client. To turn the light mode on, you must
pass two arguments to ``octez-client``:

* ``--mode light``, and
* ``--sources sources.json``.

The ``sources.json`` file contains:

* The list of endpoints to contact for retrieving data, as a list of URIs in the ``uris`` field.
  These endpoints are added to the main endpoint, either specified by option ``--endpoint``,
  if given, or to the default endpoint (``localhost:8732``), otherwise.
  To avoid being surprised by the automatic addition of the default endpoint,
  it is recommended to supply option ``--endpoint`` whenever ``--sources`` is supplied,
  valued as one member of the ``uris`` field in ``--sources`` (e.g., the first member).
* An optional ``min_agreement`` field, which must be a float from ``0.0`` (excluded) to ``1.0`` (included).
  This field specifies the ratio of endpoints that must agree for data
  to be accepted. The default value is ``1.0``, which means that
  all endpoints must agree for data to be considered valid (no rogue
  endpoint is tolerated).


Here is an example of a valid ``--sources`` file:

::

    { "min_agreement": 1.0,
      "uris": [
        "http://localhost:19733",
        "http://localhost:19735"
      ]
    }

Because computations done locally are protocol dependent, the light mode has to be configured for a specific protocol.
However, the light mode does not support all protocols.
Execute ``octez-client list light protocols`` to see the supported protocols.
It is expected that, at any given time, it should support ``Alpha``,
the current protocol of Mainnet, and the current protocol proposal on Mainnet at
the time of release, if any.

Examples with the sandbox
~~~~~~~~~~~~~~~~~~~~~~~~~

In this section, we show examples of usage of the light mode when using
the :doc:`sandboxed node<sandbox>`. For convenience we repeat
instructions for the sandboxed mode here, but refer the reader to the
sandboxed mode page for further details.

In a terminal, start a sandboxed node:

::

    $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1
      # This node listens to p2p events on localhost:19731
      # RPC server of the node is reachable at localhost:18731


Leave that terminal running. In a second terminal, start another node:

::

    $ ./src/bin_node/octez-sandboxed-node.sh 2 --connections 1
      # This node listens to p2p events on localhost:19732
      # RPC server of the node is reachable at localhost:18732

Leave that terminal running. In a third terminal, prepare the appropriate
environment for using the light client (from now on, all commands happen
in this terminal):

::

    $ eval `./src/bin_client/octez-init-sandboxed-client.sh 1`

Then upgrade the node to protocol alpha:

::

    $ octez-activate-alpha  # Triggers output in terminal of first node
    $ octez-client bake for bootstrap1  # Triggers output in terminal of first node

To avoid warnings being printed in upcoming commands (optional):

::

    $ export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y

The last step before being able to use the light client is to prepare
the JSON file passed to ``--sources``. In our scenario, this file
specifies the two endpoints to use:

::

    $ echo '{ "uris": [ "http://localhost:18731", "http://localhost:18732" ] }' > sources.json

You're now ready to use the light client. For example, bake a block (note that this command may take up to a few minutes to complete):

::

    $ octez-client --endpoint http://localhost:18731 --mode light --sources sources.json bake for bootstrap1
    Apr  8 16:42:24.202 - alpha.baking.forge: found 0 valid operations (0 refused) for timestamp 2021-04-08T14:42:24.000-00:00 (fitness 01::0000000000000004)
    Injected block BMAHozsNCos2

As you may have noticed, the block is baked when the ``boostrap1`` baker has rights to bake, which explains the possible delay of up to a few minutes.
If you are in a hurry, you may want to bake for any baker, by replacing ``bake for bootstrap1`` with ``bake for --minimal-timestamp``.
We will use this option for the next times.

Well, that doesn't seem very different from what the default client would return.
Indeed, it's the same; that was the point! To see what the light client
is doing differently, you may use the environment variable ``TEZOS_LOG``.
Set it as follows:

::

    $ export TEZOS_LOG="light_mode->debug"

Variable ``light_mode`` shows how the light mode is obtaining data from
the different endpoints.

For convenience, let's define an alias before continuing, to save
keystrokes and the ``protocol of light mode unspecified`` warning:

::

    $ alias light-client="octez-client --endpoint http://localhost:18731 --mode light --sources sources.json"

And then bake a new block:

::

    $ light-client bake for --minimal-timestamp
    Apr  8 16:49:28.172 - light_mode: light mode's core created for chain main and block head
    Apr  8 16:49:28.173 - light_mode: API call: do_rpc v1
    Apr  8 16:49:28.175 - light_mode: integrated data for key v1 from one endpoint, about to validate from 1 other
    Apr  8 16:49:28.175 - light_mode:   endpoints
    Apr  8 16:49:28.177 - light_mode: API call: get v1;constants
    Apr  8 16:49:28.177 - light_mode: API call: get v1;first_level
    Apr  8 16:49:28.177 - light_mode: API call: do_rpc pending_migration_balance_updates
    Apr  8 16:49:28.179 - light_mode: integrated data for key pending_migration_balance_updates from one endpoint,
    Apr  8 16:49:28.179 - light_mode:   about to validate from 1 other endpoints
    Apr  8 16:49:28.180 - light_mode: API call: get pending_migration_balance_updates
    ...
    ... A lot of output prefixed with light_mode: ...
    ...
    Injected block BMdbKufTymQJ

Here is the meaning of these lines:

* Line ``light mode's core created`` indicates that the light
  mode was initialized. It should be printed once per block being inspected.
* Line ``API call: do_rpc v1`` indicates that the light mode needs the
  data associated to the low-level storage's key ``v1``
* Line ``integrated data for key v1 ...`` indicates that the light mode
  obtained data for ``v1`` from a single endpoint and that it is about
  to fetch Merkle proofs for this key from other endpoints.
* Lines ``API call: get ...`` indicate that ``octez-client`` is requesting
  data from the light mode's cache. In this snippet, after the light mode
  gathered data for key ``v1``, the client is requesting data for the children
  keys ``v1;constants`` and ``v1;first_level`` (the ``;`` indicates  nesting).
  This example shows how the light mode sometimes batches requests, to avoid
  querying many keys in a row. Here it did a single request for ``v1`` instead
  of doing one request for ``v1;constants`` and a second one
  for ``v1;first_level``.

To see that computations are done locally by the light client,
we refer to the :doc:`proxy mode<proxy>`'s documentation. Debug
variables of the proxy mode apply to the light mode, as internally, the light
mode is a more complex instance of the proxy mode.

How to deploy to relieve nodes from some RPCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Again, we refer to the corresponding section in the page of
the :doc:`proxy mode<proxy>`. The exact same recommendations
apply for the light mode.
