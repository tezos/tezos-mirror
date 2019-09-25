Flexible Network Sandboxes
==========================

Build
-----

Use:

::

    make build-sandbox

(or ``make build-test``).

There are testing-only ``opam`` dependencies: ``dum`` and ``genspio``
(``0.0.2``), make sure you have done ``make build-deps``.

Usage
-----

See ``./tezos-sandbox --help``.

When running (semi-)interactive tests, it is recommended to wrap the
call with ``rlwrap`` or ``ledit``.

Examples
--------

Mini-Network
~~~~~~~~~~~~

One can run a mini-network advancing fast with accusers, bakers, and
endorsers:

::

    rlwrap ./tezos-sandbox mini-network \
           --root-path /tmp/zz-mininet-test \
           --tezos-node-binary ./tezos-node \
           --tezos-baker-alpha-binary ./tezos-baker-alpha \
           --tezos-endorser-alpha-binary ./tezos-endorser-alpha \
           --tezos-accuser-alpha-binary ./tezos-accuser-alpha \
           --tezos-client-binary ./tezos-client

Once the network is started this test scenario becomes interactive:

::

    Flextesa.mininet: Please enter command:

Just try ``h`` (or ``help``) to see the available commands.

Double Endorsement Accusation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 3 “accusation scenarios” so far, see
``./tezos-sandbox accusation --help``. For instance, the following command
starts a small 3-node network, forces one baker to endorse two
concurrent branches, and then makes another baker inject (and bake) the
double-endorsement-evidence operation. The option ``--base-port=20_000``
tells ``tezos-sandbox`` to start allocating P2P/RPC ports from 20 000 and
``--pause-at-end=true`` tells ``tezos-sandbox`` to enter an interactive
command prompt at the end of the test to give a chance to explore the
sandbox before killing all the nodes.

::

    rlwrap ./tezos-sandbox accusations simple-double-endorsing \
         --root $PWD/double-endorsing-test \
         --base-port=20_000 \
         --pause-at-end=true


This test among other ones can generate configuration files for
`Kiln <https://gitlab.com/obsidian.systems/tezos-bake-monitor/>`__
to run alongside the *Ꜩ-sandbox*, for instance:

Voting With a Ledger Nano Device
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The voting test tries to do a full round of voting and protocol switch,
including baking on the test-chain, see documentation in
``./tezos-sandbox voting --help``.

The test can run in a simpler-to-setup, or “degraded,” mode of operation
(cf. call in ``./src/bin_flextesa/dune`` for the version which
run in Gitlab-CI pipelines). In this example, we run instead a full test
with a Ledger Nano device as one of the bakers/voters. The test automatically
becomes **interactive** because the user has to press buttons on the
device, including for changing between apps.

To make the test work, you need to provide it with a ``tezos-client``
which knows about the protocol which is tested and then wins the voting
period.

One example is this branch:
```obsidian.systems/tezos#zeronet-with-proto042`` <https://gitlab.com/obsidian.systems/tezos/tree/zeronet-with-proto042>`__
which allows one to build an Apr2019-Zeronet-like code base with an extra
protocol, lets assume this is built at path ``$zeronet_042``.

Also, get an URI for your ledger (the test requires both the Wallet and
Baking apps):

::

    tezos-client list connected ledgers

And use the URI (no need to import it) for the ``--with-ledger`` option:

::

    rlwrap ./tezos-sandbox voting \
         $zeronet_042/src/proto_042_Pt1GS1Zi/lib_protocol/src \
         ./src/bin_client/test/proto_test_injection/ \
         --with-ledger "ledger://crouching-tiger-hidden-dragon/ed25519/0'/0'" \
         --serialize-proposals \
         --base-port=20_000 \
         --current-node-binary $zeronet_042/tezos-node \
         --current-client-binary $zeronet_042/tezos-client \
         --winner-client-binary $zeronet_042/tezos-client \
         --current-admin-client-binary $zeronet_042/tezos-admin-client \
         --pause-on-error=true

-  The first path argument has to be the path to a valid protocol which
   can be switched to from the current (``proto_alpha``) one.
-  The second protocol, the looser, only needs to be valid for the
   protocol compilation.
-  The option ``--serialize-proposals`` tells the test to call
   ``tezos-client submit proposals for ...`` one proposal at a time
   which is the only method the ledger Baking app can really understand.
-  The ``*-binary`` options allow to set the paths to the executables
   for the different protocols: ``current`` and ``winner``.

The test becomes interactive and guides you through the interactions
with the ledger, e.g.:

::

   Flextesa.voting:
     Ledger-prompt

         Setting up "ledger://crouching-tiger-hidden-dragon/ed25519/0'/0'" for
         baking. The ledger should be showing the setup parameters (Address,
         Main chain, HWMs).

        Please hit “✔” on the ledger.

Implementation Considerations
-----------------------------

``Running_processes`` is very high-level (actually agnostic to Tezos).
Most processes are actually calls to ``sh -c <script>`` where
``<script>`` is the result of a ``Genspio`` compilation, this leaves the
option to later easily run some processes over SSH (without OCaml
dependencies on the destination host) or in special containers (e.g.
``docker run --cpu-shares ...``).

The prompt commands for interactive use use ``Base.Sexp.t`` parsers
(because already a dependency, and we need a good string literal parser
so we cannot use ``Clic`` nor ``Cmdliner``).

Special Coding Style
--------------------

A fresh “just for testing project” is a good occasion to experiment a
bit …

See ``./src/lib_network_sandbox/internal_pervasives.ml``:

-  ``EF``: we try to use combinators on top of
   `Easy-format <https://mjambon.github.io/mjambon2016/easy-format.html>`__
   for most pretty-printing (it is still compatible with ``Format`` but
   it is much more functional/composable and does not rely on
   ``@[<2,3>@{crazy}@ @<acronym>EDSLs@n@]``).
-  Many standard modules are taken from Jane St Base (already a
   dependency of Tezos): List, String, Option, Int, Float.
-  Error monad uses *more typed* errors (polymorphic variants),
   cf. module ``Asynchronous_result`` (and note that ``bind`` also calls
   ``Lwt_unix.auto_yield 0.005 ()``).
-  All state is kept in a (*non-global*) value passed as argument
   everywhere needed. To simplify the dependency management the state
   variables are objects (cf. ``Base_state``, then ``Paths``,
   ``Console``, etc).

Also, everything uses OCamlFormat instead of ``ocp-indent`` (see
``./src/lib_network_sandbox/.ocamlformat``).
