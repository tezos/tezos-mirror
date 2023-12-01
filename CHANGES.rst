Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of octez-node,
octez-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

General
-------

Node
----

- **Breaking change** Removed the deprecated ``endorsing_rights`` RPC,
  use ``attestation_rights`` instead. (MR :gl:`!9849`)

- Added metrics about distributed data base messages sent, broadcasted or received

- **Breaking change** Removed the deprecated
  ``disable-mempool-precheck`` configuration flag and
  ``disable_precheck`` field of ``prevalidator`` in the shell limits
  of the configuration file. They already had no effect on the node
  anymore. (MR :gl:`!10030`)

- **Breaking change** Bumped the Octez snapshot version from ``5`` to
  ``6`` to explicit the incompatibility with previous version
  nodes. Also, improved the consistency of ``snapshot`` import errors
  messages (MR :gl:`!10138`)

- Introduced a new process, forked by the node, that is responsible of
  managing the RPC server: the RPC-process. It is used by default by
  the node.

- Introduced a new ``--local-rpc-addr`` that starts the RPC server
  locally, not using the dedicated RPC-process.

- Add logs at ``Info`` level about the disconnection reasons in the p2p section.

- Removed a spurious "missing validation plugin" warning message that
  was emitted every time a block was applied using an old protocol
  where its plugin was removed.

- **Breaking change** Removed the deprecated ``/monitor/valid_blocks``
  RPC. Instead, use the ``/monitor/applied_blocks`` RPC that has the
  same behaviour.

Client
------

- Fixed indentation of the stacks outputted by the ``normalize stack``
  command. (MR :gl:`!9944`)

- Added options to temporarily extend the context with other contracts
  and extra big maps in Michelson commands. (MR :gl:`!9946`)

- Added a ``run_instruction`` RPC in the plugin and a ``run michelson code``
  client command allowing to run a single Michelson instruction or a
  sequence of Michelson instructions on a given stack. (MR :gl:`!9935`)

- The legacy unary macros for the ``DIP`` and ``DUP`` Michelson
  instructions have been deprecated. Using them now displays a warning
  message on stderr.

- Added a ``run unit tests`` client command allowing to run one or
  several Michelson unit tests in `TZT format
  <http://tezos.gitlab.io/active/michelson.html#tzt-a-syntax-extension-for-writing-unit-tests>`__. (MR
  :gl:`!10898`)

Baker
-----

- Made the baker attest as soon as the pre-attestation quorum is
  reached instead of waiting for the chain's head to be fully
  applied (MR :gl:`!10554`)

Accuser
-------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

- The rollup node is protocol agnostic and released as part of the Docker
  image. (MR :gl:`!10086`)


Smart Rollup node
-----------------

- Now smart rollup node allows multiple batcher keys. Setting multiple
  keys for the batching purpose allows to inject multiple operations
  of the same kind per block by the rollup node. ( MR :gl:`!10512`, MR
  :gl:`!10529`, MR :gl:`!10533`, MR :gl:`!10567`, MR :gl:`!10582`, MR
  :gl:`!10584`, MR :gl:`!10588`, MR :gl:`!10597`, MR :gl:`!10601`, MR
  :gl:`!10622`, MR :gl:`!10642`, MR :gl:`!10643`, MR :gl:`!10839`, MR
  :gl:`!10842`, MR :gl:`!10861`, MR :gl:`!11008` )

- A new bailout mode that solely cements and defends existing
  commitments without publishing new ones. Recovers bonds when
  possible, after which the node exits gracefully. (MR :gl:`!9721`, MR
  :gl:`!9817`, MR :gl:`!9835`)

- RPC ``/global/block/<block-id>/simulate`` accepts inputs with a new optional
  field ``"log_kernel_debug_file"`` which allows to specify a file in which
  kernel logs should be written (this file is in
  ``<data-dir>/simulation_kernel_logs``). (MR :gl:`!9606`)

- The protocol specific rollup nodes binaries are now deprecated and replaced
  by symbolic links to the protocol agnostic rollup node. In the future, the
  symbolic links will be removed. (MR :gl:`!10086`)

- Released the protocol agnostic rollup node ``octez-smart-rollup-node`` as part
  of the Octez distribution. (MR :gl:`!10086`)

- Added the rollup node command inside the docker entrypoint (MR :gl:`!10253`)

- Added the argument ``cors-headers`` and ``cors-origins`` to specify respectively the
  allowed headers and origins. (MR :gl:`!10571`)

- Fix header in messages store to use predecessor hash to avoid missing pointer
  in case of reorganization and GC. (MR :gl:`!10847`)

- Added a garbage collection mechanism that cleans historical data before the LCC.
  (MRs :gl:`!10050`, :gl:`!10135`, :gl:`!10236`, :gl:`!10237`, :gl:`!10452`)

- Added a ``history-mode`` option, which can be either ``archive`` or
  ``full``. In ``archive``, the default, the rollup node has the whole L2 chain
  history, no GC happens. In ``full`` the rollup node retains data for possible
  refutations. (MRs :gl:`!10475`, :gl:`!10695`)

- Snapshot export with integrity checks. (MR :gl:`!10704`)

Smart Rollup client
-------------------

- **Breaking change** smart rollup client have been deprecated and
  no longer exist, most commands have equivalents RPCs and ``octez-codec`` (MR :gl:`!11046`).

- The following table outlines the deprecated of smart rollup client commands and
  their corresponding replacements with new RPCs:

  .. code-block:: rst

    ==========================================  ====================================================
    Command                                     RPC
    ==========================================  ====================================================
    get smart rollup address                    [GET global/smart_rollup_address]
    ------------------------------------------  ----------------------------------------------------
    get state value for <key> [-B --block       [GET global/block/<block>/state]
    <block>]
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level> transferring               <outbox_level>/messages] with message index in query
    <transactions>
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level>                            <outbox_level>/messages] with message index in query
    ==========================================  ====================================================

- The result of ``encode outbox message <transactions>`` can be achieved:
  ``octez-codec encode alpha.smart_rollup.outbox.message from <transactions>``.

- The keys in the smart rollup client use the same format as the ``octez-client``.
  They can be imported with ``octez-client import secret key <sk_uri>``, or by merging the key files
  between the ``octez-client`` base directory and the ``smart-rollup-client-<proto>`` base directory.

Smart Rollup WASM Debugger
--------------------------

- Added flag ``--no-kernel-debug`` to deactivate the kernel debug messages. (MR
  :gl:`!9813`)

- Support special directives using ``write_debug`` host function in the
  profiler, prefixed with ``__wasm_debugger__::``. Support
  ``start_section(<data>)`` and ``end_section(<data>)`` to count ticks in

- Partially support the installer configuration of the Smart Rollup SDK, i.e.
  support only the instruction ``Set``. The configuration can be passed to
  the debugger via the option ``--installer-config`` and will initialize the
  storage with this configuration. (MR :gl:`!9641`)

Data Availability Committee (DAC)
---------------------------------

Miscellaneous
-------------

- Beta scripts to build Debian and RedHat packages have been added to the tree.

- New Recommended Rust version 1.71.1 instead of 1.64.0.

- Extended the Micheline lexer to allow primitives starting with the
  underscore symbol (``_``). (MR :gl:`!10782`)

- Beta Debian and Redhat packages are now linked in gitlab releases.

- Renamed package registries for releases from ``tezos-x.y`` to ``octez-x.y``.
