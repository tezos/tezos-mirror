Flexible Network Sandboxes
==========================

The binary ``tezos-sandbox`` uses the Flextesa library to provide
sandbox networks with baker and endorser daemons, with various test
scenarios.

Some of those scenarios run in the CI, see ``./src/bin_sandbox/dune`` (although
it is not recommended to experiment with sandboxes using ``dune`` which can
leave unkilled processes easily, see issue
`#2445 <https://github.com/ocaml/dune/issues/2445>`__).


Build
-----

The application is not built by default, one needs:

::

    make build-sandbox

Usage
-----

See ``./tezos-sandbox --help`` and all the examples below.

When running (semi-)interactive tests, it is recommended to wrap the
call with ``rlwrap`` or ``ledit``.

MacOSX Users
------------

At runtime, sandboxes usually depend on a couple of linux utilities.

If you are on Mac OS X, you can do ``brew install coreutils util-linux``. Then run
the tests with:

::

    export PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/util-linux/bin:$PATH"


See Also
--------

``tezos-sandbox`` based on the Flextesa library which is being developed
at `gitlab.com/tezos/flextesa <https://gitlab.com/tezos/flextesa>`__.
One can find more instructions there, including how to use isolated
sandboxes using Docker. TQ Tezos' assets portal also shows how to start
a sandbox and interact with it using a separate ``tezos-client``:
https://assets.tqtezos.com/docs/setup/2-sandbox/.


Concepts
--------

Let’s clear a couple of things up:

-  *full* Vs *manual* sandbox: we call “full” a sandbox that uses
   baker/endorser/accuser daemons and hence advances by itself. A
   “manual” sandbox only has nodes, they require successive calls to
   ``bake for`` tezos-client commands (or ``bake`` in the interactive
   prompt if any).
-  Each sandbox scenario has a *root path* where all logs and generated
   files go (usually exposed with the ``--root-path`` option).
-  Some sandboxes can start an interactive command-line interface. The
   command-line parsing uses the Sexplib library; see
   https://github.com/janestreet/sexplib#about for the lexical
   conventions. Try the ``help`` command for instance.
-  By default, sandboxed nodes get assigned successive port numbers for
   their RPC and P2P services; with the option ``--base-port PORT``,
   ``PORT`` will be used for the RPC of node 0, ``PORT + 1`` for its
   P2P, ``PORT + 2`` for the RPC of node 1, etc.
-  Sandboxes like the ``mini-network`` also provide a shell-environment
   file at ``$ROOT_PATH/shell.env`` which provides aliases to
   ``tezos-client`` commands compatible with the sandbox (see also
   the ``help-env`` interactive command).


Examples
--------

Interactive Mini-Network
~~~~~~~~~~~~~~~~~~~~~~~~

One can run a mini-network advancing fast with accusers, bakers, and
endorsers:

::

    rlwrap ./tezos-sandbox mini-network \
           --root-path /tmp/zz-mininet-test \
           --size 2 \
           --number-of-bootstrap-accounts 2 \
           --tezos-node-binary ./tezos-node \
           --tezos-baker-alpha-binary ./tezos-baker-alpha \
           --tezos-accuser-alpha-binary ./tezos-accuser-alpha \
           --tezos-client-binary ./tezos-client

Once the network is started this test scenario becomes interactive:

::

    Flextesa.mininet: Please enter command:

Just try ``h`` (or ``help``) to see the available commands, or ``q`` to kill the
sandbox and quit.

The ``mini-network`` has many options, ``./tezos-sandbox mini --help``.

Mini-Network with User Activated Upgrade
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example runs another full sandbox (3 nodes, 2 “bakers”), for a
limited amount of time (60 blocks, no interactivity), and performs a
user-activated-upgrade (a.k.a. a protocol “hard-fork”) between Babylon
and Carthage (as built on the ``master`` branch).

We also set the base-port to 3000 and add some random traffic;
i.e. create contract originations and contract calls (for now the
setting also requires also the ``--until-level`` option):

::

   ./tezos-sandbox mini-network \
          --root-path /tmp/hard-fork-mininet \
          --size 3 \
          --base-port 3_000 \
          --number-of-bootstrap-accounts 2 \
          --protocol-hash PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS \
          --protocol-kind Babylon \
          --until-level 60 \
          --random-traffic any \
          --tezos-baker-alpha-binary ./tezos-baker-005-PsBabyM1 \
          --tezos-endorser-alpha-binary ./tezos-endorser-005-PsBabyM1 \
          --tezos-accuser-alpha-binary ./tezos-accuser-005-PsBabyM1 \
          --hard-fork 20:PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb \
          --hard-fork-baker-alpha-binary ./tezos-baker-006-PsCARTHA \
          --hard-fork-endorser-alpha-binary ./tezos-endorser-006-PsCARTHA \
          --hard-fork-accuser-alpha-binary ./tezos-accuser-006-PsCARTHA \
          --tezos-node-binary ./tezos-node \
          --tezos-client-binary ./tezos-client


Manual Mini-Network With An Archive Node
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An interactive Carthage sandbox with 3 nodes, one of which running in
``archive`` mode, and no baking daemons:

::

   rlwrap ./tezos-sandbox mini-network \
          --root-path /tmp/manual-mininet \
          --size 3 \
          --set-history-mode N000:archive \
          --no-baking \
          --protocol-hash PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb \
          --protocol-kind Carthage \
          --tezos-baker-alpha-binary ./tezos-baker-006-PsCARTHA \
          --tezos-endorser-alpha-binary ./tezos-endorser-006-PsCARTHA \
          --tezos-accuser-alpha-binary ./tezos-accuser-006-PsCARTHA \
          --tezos-node-binary ./tezos-node \
          --tezos-client-binary ./tezos-client

Once the network is started, we enter the interactive mode, and we can
use the ``bake`` command to create blocks (``bake`` can take an
argument: which client to bake with, e.g. \ ``bake 2``).

We can check that ``N000`` is indeed an archive node:
``c2 rpc get /chains/main/checkpoint`` (``help`` tells us that ``c2`` is
the client for the node ``N000``).

Double Endorsement Accusation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 3 “accusation scenarios” so far, see
``./tezos-sandbox accusation --help``. For instance, the following command
starts a small 3-node network, forces one baker to endorse two
concurrent branches, and then makes another baker inject (and bake) the
double-endorsement-evidence operation. The option ``--pause-at-end=true`` tells
``tezos-sandbox`` to enter the interactive mode (command prompt) at the end of
the test to give a chance to explore the sandbox before killing all the nodes.

::

    PATH=.:$PATH rlwrap ./tezos-sandbox accusations simple-double-endorsing \
         --root $PWD/double-endorsing-test \
         --pause-at-end=true


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

Get an URI for your ledger (the test requires both the Wallet and
Baking apps):

::

    tezos-client list connected ledgers

And use the URI (no need to import it) for the ``--with-ledger`` option:

::

    rlwrap ./tezos-sandbox voting \
         ./src/proto_006_PsCARTHA/lib_protocol/TEZOS_PROTOCOL \
         ./src/proto_demo_noops/lib_protocol/TEZOS_PROTOCOL \
         --with-ledger "ledger://crouching-tiger-hidden-dragon/ed25519/0'/0'" \
         --serialize-proposals \
         --base-port=20_000 \
         --current-node-binary ./tezos-node \
         --current-client-binary ./tezos-client \
         --winner-client-binary ./tezos-client \
         --current-admin-client-binary ./tezos-admin-client \
         --pause-on-error=true

-  The first path argument has to be the path to a valid protocol which
   can be switched to from the current (``proto_alpha``) one.
-  The second protocol, the looser, only needs to be valid for the
   protocol compilation.
-  The option ``--serialize-proposals`` tells the test to call
   ``tezos-client submit proposals for ...`` one proposal at a time
   which is the only method the ledger Wallet app can really understand.
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

See ``./vendor/lib_flextesa/internal_pervasives.ml``:

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
