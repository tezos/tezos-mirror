Protocol Babylon
================

**Important: revision 005_PsBABY5H of protocol Babylon contains a bug that is corrected in the latest version 005_PsBabyM1**
more details can be found in section `Bug affecting Bigmaps in 005_PsBABY5H`_.

This page contains all the relevant information for protocol 005 Babylon (005_PsBabyM1).
Each of the main changes is briefly described with links to relevant
external documentation and merge requests.
There are two sections dedicated to all the changes to RPCs and
operations.
The changelog section contains the most significant commit messages
and instructions to regenerate the protocol sources from the
Gitlab branch.

**This protocol contains several breaking changes with respect to Athens.**
Developers are particularly encouraged to carefully read this page and
to monitor it for updates.

More documentation concerning Emmy+ and Michelson is available :doc:`here
<../index>`.

.. contents:: Summary of changes

Emmy+
-----

Protocol 004 implements a consensus algorithm nick-named
``Emmy``.
Protocol 005 introduces several improvements to this algorithm,
regrouped under the name ``Emmy+``.
In particular:

- the fewer endorsements a block carries the longer it takes before it
  can be considered valid,
- the fitness of a block is simply its height in the chain,
- changes to the rewards for block and endorsement of priority greater
  than 0.

Detailed information can be found in the blog
`announcement <https://research-development.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html>`_
and
`analysis. <https://research-development.nomadic-labs.com/analysis-of-emmy.html>`_

Merge requests
`(MR58)
<https://gitlab.com/nomadic-labs/tezos/-/merge_requests/58>`_
and
`(MR72)
<https://gitlab.com/nomadic-labs/tezos/-/merge_requests/72>`_


Michelson
---------

Protocol 005 contains several improvements to the Michelson smart
contract language.
More details are provided later in the changelog and in the
`Michelson update blog post.
<https://research-development.nomadic-labs.com/michelson-updates-in-005.html>`_

A summary of the main changes:

- smart contracts now support entrypoints
  `(MR59) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/59>`_,
- contracts can now create, store and transmit as many big_maps as
  they want
  `(MR76) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/76>`_,
- comparable types are now closed under products (i.e. the pair
  constructor),
- a new instruction, ``CHAIN_ID``, allows contracts to differentiate
  between the test chain and the main network
  `(MR65) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/65>`_,
- a gas cost overhaul has been integrated, and ``STEPS_TO_QUOTA`` has been
  disabled until a more robust semantics is found
  `(MR73) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/73>`_.

New instructions to facilitate compilation to Michelson
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some new instructions have been added in order to make Michelson a
better compilation target for high level languages.

The new instructions ``DIG n``, ``DUG n``, ``DIP n { code }``, ``DROP n``
allow to simplify commonly used patterns `(MR81).
<https://gitlab.com/nomadic-labs/tezos/-/merge_requests/81>`_

The new instruction ``APPLY`` allows to perform the partial application of
an argument to a lambda `(MR46).
<https://gitlab.com/nomadic-labs/tezos/-/merge_requests/46>`_

Changes for compatibility with the accounts rehaul
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A few changes are needed as a consequence of the accounts simplification.

- the instruction ``CREATE_ACCOUNT`` disappears,
- the type of ``CREATE_CONTRACT`` changes.

.. _proposal_quorum:

Proposal quorum
---------------

During the first period of a voting procedure, proposals can be
submitted and the one with the largest number of upvotes is promoted
to the testing vote period.
In 004 the promoted proposal can have any number of upvotes, even 1,
as long as the others proposals, if any, have less.
Protocol 005 introduces a quorum of 5% (constant
``min_proposal_quorum``) for proposals to be promoted to the testing
vote period, if there is less than 5% of the stake supporting them,
the protocol goes back to a proposal period.

`Cryptium's blog post (proposal quorum).
<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-v-3e0ddfd98177>`_

`(MR71)
<https://gitlab.com/nomadic-labs/tezos/-/merge_requests/71>`_

.. _quorum_caps:

Quorum caps
-----------

During the test phases the participation needs to reach a quorum for a
vote to be successful.
The quorum adapt over time based on the participation of past votes.
In 004 the quorum can reach very high values which would make passing
new proposals very difficult even if there is large acceptance.
On the other hand the quorum could reach very low levels if there is
little participation.
Protocol 005 introduces caps to limit the maximum and minimum value
that the quorum can reach.
The values proposed for minimum quorum cap is set to 20% and the
maximum to 70%, these values can be changed in future updates.
Additionally the formula to update the quorum uses an exponential
moving average of the participation.

`Cryptium's blog post (quorum caps).
<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-ii-607227fc6d65>`_

`MR52. <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/52>`_

.. _babylon_introduced_delegation:

Make implicit accounts delegatable
----------------------------------

In protocols 004 only KT1 addresses, representing an account for
delegation or a smart contract, can be delegated and only tz
can register as delegate.
In protocol 005, tz accounts which are not registered as
delegate can be delegated towards a tz account registered as delegate.
This change does not affect existing delegations of KT accounts.

One restriction remains that may be removed in the future: once a tz
account is registered as delegate it cannot be un-registered.
This in turn means that a registered delegate that wants to stop being
one, cannot delegate to somebody else.
The only solution for now is to move the funds to a newly created tz
account and delegate from there.

Cryptium's blog posts
1. `<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-iii-1c824b760da3>`_
2. `<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-vi-540170f46c51>`_

Merge Request : `MR61. <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/61>`_


Replace KT1 accounts with ``manager.tz`` script
-----------------------------------------------

In 004 an address KT1 can refer to a scriptless account used for
delegation or to a smart contract with code.
Given that in 005 it is possible to delegate from tz accounts,
scriptless KT1 accounts are deprecated.
Existing KT1 accounts are replaced with a smart contract
``manager.tz`` which implements the same semantics.
The smart contract has been formally verified in Mi-Cho-Coq.

While the migrated accounts preserves all their features, this will
change the way wallets and other applications interact with them.
Detailed instructions for migrating such applications will be provided
in the coming days.

Cryptium's blog posts
1. `<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-iii-1c824b760da3>`_
2. `<https://medium.com/metastatedev/meanwhile-at-cryptium-labs-1-part-vi-540170f46c51>`_

`manager.tz script
<https://gitlab.com/nomadic-labs/mi-cho-coq/blob/master/src/contracts/manager.tz>`_
and
`proof
<https://gitlab.com/nomadic-labs/mi-cho-coq/blob/master/src/contracts_coq/manager.v>`_.

Merge requests
- `(MR66) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/66>`_
- `(MR74) <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/74>`_

Changes to RPCs
---------------

This section lists the changes in RPCs to put the spotlight on them.
To stay readable, it cannot provide detailed recipes to adapt to every
one of them. Affected users can get the new formats by using the
command ``tezos-client rpc list <url>`` and ``tezos-client rpc format
<url>``.


Consequence of ``Emmy +``
~~~~~~~~~~~~~~~~~~~~~~~~~

- ``GET /chains/<chain_id>/blocks/<block_id>/context/constants`` has
  two new required fields "delay_per_missing_endorsement" and
  "initial_endorsers".

- There are three new RPCs ``GET
  /chains/<chain_id>/blocks/<block_id>/minimal_valid_time``, ``GET
  /chains/<chain_id>/blocks/<block_id>/required_endorsements`` and
  ``POST /chains/<chain_id>/blocks/<block_id>/endorsing_power``.

Consequence of ``quorums``
~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``GET /chains/<chain_id>/blocks/<block_id>/context/constants`` has three
  new required fields "min_proposal_quorum", "quorum_max" and "quorum_min".

Consequences of ``New instructions to facilitate compilation to Michelson``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Inputs and outputs of

- ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>``

- ``POST /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/big_map_get``

- ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/storage``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/typecheck_data``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/typecheck_code``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/pack_data``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/forge/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/parse/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/block``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/run_code``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/run_operation``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/trace_code``

are affected

Consequences of ``Entry-point introduction``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
New RPCs

- ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/entrypoints``

- ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/entrypoints/<string>``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/entrypoint``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/entrypoints``

Consequences of other ``Michelson`` changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Fields "manager" and "spendable" disappear in ``GET
  /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>``
  as well as the RPCs ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/manager``,
  ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/spendable``
  and ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/delegatable``

- Output format of field "delegate" in ``GET
  /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>``
  and output of ``GET
  /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/manager_key``
  are now public key hashes.

- Field "counter" becomes optional in
  ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>``

- In ``GET
  /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>`` and
  ``GET
  /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/delegated_contracts``,
  field "Contract_hash" is replaced by "contract_id".

Manager operations are incompatible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As a consequence, the following RPCs formats are intentionally changed to make explicit the modifications

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/forge/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/parse/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/block``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/run_code``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/run_operation``

- ``POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/trace_code``

Changes to the binary format of operations
------------------------------------------

This section describes the changes in binary format for operations.
It is possible for readers to compile this list by themselves by
calling ``describe unsigned operation`` on the tezos client with both
protocols Athens and Babylon, and then use a diffing tool.

The ``source`` field of manager operations is now a public key hash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Babylon, only tz1, tz2 and tz3 accounts can be the source of
manager operations (transaction, origination, delegation,
reveal). These operations currently contain a source contract, that is
a byte ``0`` followed by a public key hash for a tz1, tz2 or tz3, or a
byte ``1`` followed by a contract hash for a KT1. This first byte
disappears since the KT1 case is now impossible.

Transactions now have an entrypoint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Athens, the transaction operation ends in either a byte ``0``,
equivalent to sending ``Unit``, and sufficient for transaction to tz1,
tz2 or tz3 accounts, or a byte ``1``, followed by the smart contract
parameter (four bytes of size followed by the serialized Michelson
data).

In Babylon, the transaction operation ends in either a byte ``0``,
equivalent to sending ``Unit`` to entrypoint ``%default``, and
sufficient for transaction to tz1, tz2 or tz3 accounts, or a byte
``1``, followed by the entrypoint, and then the smart contract
parameter (four bytes of size followed by the serialized Michelson
data).

The entrypoint format is as follows:

- one byte ``0`` for entrypoint ``%default``
- one byte ``1`` for entrypoint ``%root``
- one byte ``2`` for entrypoint ``%do``
- one byte ``3`` for entrypoint ``%set_delegate``
- one byte ``4`` for entrypoint ``%remove_delegate``
- one byte ``255`` for a named entrypoint, then one byte of entrypoint
  name size (limited to 31), and the name itself

Bytes ``5`` to ``254`` are unused and may be used in future update to
optimize in size frequent calls to common entrypoints.

Some fields are dropped in originations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Babylon, smart contracts do not have a manager anymore, and must have a script.

The following field thus disappear:

- the manager public key (21 bytes),
- the spendable flag (1 byte),
- the delegatable flag (1 byte),
- the presence flag before the script field (1 byte).

The tags of all manager operations are shifted by ``100``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because of the incompatibilities above, all manager operations see
their tags changed. The transaction format incompatibility between
Athens and Babylon is made explicit by this change.

- the reveal operation tag goes from ``7`` to ``107``,
- the transaction operation tag goes from ``8`` to ``108``,
- the origination operation tag goes from ``9`` to ``109``,
- the delegation operation tag goes from ``10`` to ``110``.

Developers who inject transactions in the chain must adapt to this new
tagging policy. The recommended procedure is to make a dynamic test,
and to produce a transaction in a format compatible with the
``next_protocol`` announced by the head of the chain.

Transactions that are emitted in the last moments of Athens and that
do not get included in a block because of network latency will not
survive the migration to Babylon. They will have to be emitted again
in the new format.

.. _migrating_scriptless_contracts:

Migration from scriptless KT1s to ``manager.tz``
------------------------------------------------------------------

This section explains how to interact with the manager.tz contract that all existing KT1 accounts
will have after the migration. Wallets can either urge their users to migrate to use implicit
accounts or can support implicit accounts as well as scriptful KT1s.

The ``tezos-client`` has been updated to be mostly backwards compatible, and the below explanations
are mostly directed at RPC users and the invocation of the ``tezos-client`` are given as
examples.

To set delegate using the manager.tz script, one can use:

.. code-block:: bash

   tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'do' \
               --arg '{ DROP ; NIL operation ; PUSH key_hash "<dlgt>" ; SOME ; SET_DELEGATE ; CONS }'

- ``src``: has to be equal to the ``key_hash`` found in the contract's storage,
  i.e. its manager.
- ``dst`` is the originated contract
- ``dlgt`` is the ``key_hash`` of the delegate

To remove delegate, use:

.. code-block:: bash

   tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'do' \
               --arg '{ DROP ; NIL operation ; NONE key_hash ; SET_DELEGATE ; CONS }'

- ``src``: has to be equal to the ``key_hash`` found in the contract's storage,
  i.e. its manager.
- ``dst`` is the originated contract

To transfer (spend) tezos from originated contract to an implicit account, use:

.. code-block:: bash

   tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'do' \
               --arg '{ DROP ; NIL operation ; PUSH key_hash "<adr>" ; IMPLICIT_ACCOUNT ; PUSH mutez <val> ; UNIT ; TRANSFER_TOKENS ; CONS }'

- ``src``: has to be equal to the ``key_hash`` found in the contract's storage,
  i.e. its manager.
- ``dst``: is the originated contract
- ``adr``: key_hash of the implicit account receiving the tokens
- ``val``: amount of mutez to transfer

To transfer tezos from originated contract to another originated contract, use:

.. code-block:: bash

   tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'do' \
               --arg '{ DROP ; NIL operation ; PUSH address <adr> ; CONTRACT %<ent> <par> ; ASSERT_SOME ; PUSH mutez <val> ; <ppar> ; TRANSFER_TOKENS ; CONS }'

- ``src``: has to be equal to the ``key_hash`` found in the left part of the
  contract's storage ``pair``, i.e. its manager.
- ``dst``: is the originated contract
- ``adr``: addressee to receive the tokens
- ``ent``: addressee script's entrypoint (omit if not used)
- ``par``: addressee script's call parameter type
- ``ppar``: instruction to push parameter value of call to addressee script
- ``val``: amount of mutez to transfer

Origination script transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``spendable`` and ``delegatable`` flags determine the template, if any:

========= =========== ================
spendable delegatable template
========= =========== ================
true      true        add_do
true      false       add_do
false     true        add_set_delegate
false     false       none
========= =========== ================

For a complete Michelson pseudo-code showing these transformations, together
with examples of these transformations applied to the `id.tz script <https://gitlab.com/tezos/tezos/blob/794bc16664cbed4057ffbc51631151023af835c0/src/bin_client/test/contracts/attic/id.tz>`_,
please refer to this `Mi-cho-coq merge request <https://gitlab.com/nomadic-labs/mi-cho-coq/-/merge_requests/29>`_.

For both ``add_do`` and ``add_set_delegate`` templates, the original script's
storage gets wrapped in a ``pair``, with the manager of the contract being written
into the left part of the pair. The right part of the storage is the original
storage value of the original storage type.

Template ``add_do``
^^^^^^^^^^^^^^^^^^^

The original script's parameter is wrapped in ``or`` type, with its left part
being the newly added parameter of type ``lambda unit (list operation)`` and
entrypoint annotation ``%do``. The right part of the parameter is the original
parameter of the original parameter type with added ``%default`` entrypoint
annotation.

To spend and set/remove delegate one can use the same calls as for the
[manager.tz script](#manager-tz-script).

There is no change to use original script functionality, as the original
parameter type is given ``%default`` entrypoint. Any argument you pass in
to the call will get automatically wrapped to match the ``Right`` part of the
transformed script's parameter.

Template ``add_set_delegate``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The original script's parameter is wrapped in ``or`` type, with its left part
being the newly added parameter of type:

.. code-block:: michelson

   or
     (key_hash %set_delegate)
     (unit %remove_delegate)

with two entrypoints - ``%set_delegate`` and ``%remove_delegate``. The right part of
the parameter is the original parameter of the original parameter type with
added ``%default`` entrypoint annotation.

To set delegate using the added entrypoint, one can use:

.. code-block:: bash

  tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'set_delegate' \
               --arg '"<dlgt>"'

- ``src``: has to be equal to the ``key_hash`` found in the left part of the
  contract's storage ``pair``, i.e. its manager.
- ``dst`` is the originated contract
- ``dlgt`` is the ``key_hash`` of the delegate

To remove delegate, use:

.. code-block:: bash

  tezos-client transfer 0 from <src> to <dst> \
               --entrypoint 'remove_delegate' \
               --arg 'Unit' # arg is optional, it defaults to Unit when omitted

- ``src``: has to be equal to the ``key_hash`` found in the left part of the
  contract's storage ``pair``, i.e. its manager.
- ``dst`` is the originated contract

Please note, that you are not allowed to transfer tokens on ``%do``,
``%set_delegate``, or ``%remove_delegate`` entrypoints calls. Invoke these
entrypoints with ``tezos-client transfer 0``.

Gas cost changes
^^^^^^^^^^^^^^^^
The cost for managing the delegate of the ``manager.tz`` script is 25817
gas to set the delegate and 25722 to withdraw the current delegation.

For other contracts with ``%set_delegate`` and
``remove_delegate``, it varies with the contract as the gas cost for
typechecking depends on the contract's code.

The gas cost for each kind of transfer operation is as follow:

- implicit account (tz1|tz2|tz3...) → implicit account :  10207 gas
- implicit account → originated manager.tz : 15285 gas
- originated manager.tz → implicit account : 26183 gas
- originated manager.tz → originated manager.tz : 44625 gas

.. _005-bigmap-bug:

Bug affecting Bigmaps in 005_PsBABY5H
-------------------------------------

Protocol 005_PsBABY5H contains a bug affecting Bigmaps.

The ``has_big_map`` function used to compute whether or not a Michelson
type contains a ``big_map`` was wrongly implemented and always returned
``false``. This had the following consequences:

- For newly originated contracts, storing several ``big_map``\ s and
  operating on them is possible but regular maps are used under the
  hood so the efficiency is much worse than expected. Sending a
  ``big_map`` to another contract is not always possible.

- For migrated contracts storing a ``big_map``, updating the
  stored ``big_map`` is not possible anymore; getting the stored values
  is possible but less efficient than expected.

Additionally there is also a small regression affecting the
``trace_code`` RPC.
As a result, the tracing functionality of the interpreter was
disabled.

Both issues above are fixed in protocol 005_PsBabyM1.


Changelog
---------

You can see the full git history on the branch `proto-005
<https://gitlab.com/nomadic-labs/tezos/-/commits/proto-005>`_.
In order to regenerate a protocol with the same hash as Babylon you
can run from this branch::

  $ ./scripts/snapshot_alpha.sh babylon_005 from athens_004
  $ ls src/proto_005_PsBabyM1


Emmy+
~~~~~

- Baker: adapt baker code for Emmy+

::

    This is not a patch for the protocol.
    It does not affect the hash, but is needed for the baker to work.

     - BREAKING CHANGE: remove the await_endorsement arg as it becomes mandatory.
     - Implement new heuristic to wait for endorsements
     - Adapt local validation to match the new validation semantics.
     - Fix "bake for --minimal-timestamp".
     - Prevent the creation of block with a timestamp in the future unless --force is given.

- Proto: rewards depend on block priority

::

    Contains a BREAKING CHANGE (see end of message).

    The baking reward is now calculated w.r.t a given priority [p] and a
    number [e] of included endorsements as follows:

    (block_reward / (p+1)) * (0.8 + 0.2 * e / endorsers_per_block)

    Explorers or bakers that recompute the reward by themselves should
    implement this new formula. Those who use the block receipts should be
    fine.

- Proto: freeze endorsement deposits at operation application

::

    In Athens and before, endorsement deposits where taken at the end of
    the block validation, after the transactions, including transaction
    from the endorsers' accounts. This made things more difficult for the
    baker, and led to a few mishaps in the past.

    This patch changes that behaviour, so that endorsement deposits are
    taken before transactions are evaluated.

- Proto: add RPCs to query the required endorsement constraints

::

    This patch adds the necessary RPCs to implement the baker for Emmy+.

    Developers of analytics tools or explorers may also want to use these
    new RPCs.

- Proto: add a minimum number of endorsements requirement, a.k.a Emmy+

::

    Contains a BREAKING CHANGE (see end of message).

    A block is now valid only if its timestamp has a minimal delay with
    respect to the previous block's timestamp, and this minimal delay
    depends not only on the block's priority but also on the number of
    endorsement operations included in the block.

    In Emmy+, blocks' fitness increases by one unit with each level.

    In this way, Emmy+ simplifies the optimal baking strategy: The bakers
    used to have to choose whether to wait for more endorsements to
    include in their block, or to publish the block immediately, without
    waiting. The incentive for including more endorsements was to increase
    the fitness and win against unknown blocks. However, when a block was
    produced too late in the priority period, there was the risk that the
    block did not reach endorsers before the block of next priority. In
    Emmy+, the baker does not need to take such a decision, because the
    baker cannot publish a block too early.

    Third party developers should make sure they can parse the new fields
    in the `/constants` RPC, or at least ignore them.


Michelson
~~~~~~~~~

- Proto/Michelson: Deprecate instruction STEPS_TO_QUOTA

::

    The semantics of the STEPS_TO_QUOTA instruction changes each time the
    gas constants are modified to better reflect the real costs.

    Moreover, because of STEPS_TO_QUOTA, the interpreter is not monotonic:
    it is easy to write a contract that runs successfully at some gas
    amount but fails when more gas is given.

- Proto/Michelson: expose internal function of the Michelson interpreter

::

    This patch is a refactor that does not change the semantics. It will
    allow external tools such as steppers or debuggers to control more
    finely the Michelson interpreter from outside the protocol.

- Proto/Michelson: add ``APPLY`` instruction to partially apply a lambda

::

    This instruction applies a tuplified function from the stack.  Such a
    lambda is storable, and thus values that cannot be stored (values of
    type `operation`, `contract _` and `big_map _ _`) cannot be
    captured by `APPLY` (cannot appear in `'a`).

- Proto/Michelson: relax big_map restrictions

::

    A contract can now have more than one big_map, they can be placed
    anywhere in the storage. Big maps can be transferred from a contract
    to another, either as parameter (transactions) or storage
    (originations). In this case, they are morally duplicated (as opposed
    to shared) from the contract point of view. In the implementation,
    sharing happens. Big maps can be created with `EMPTY_BIG_MAP t` and
    cleared on the fly.

    The big_map type still cannot appear as argument of big_map, PUSH or
    UNPACK. When you duplicate a big map, you are charged with the full
    storage cost.

    This patch moves the big maps outside of the contracts in the context,
    in their own prefix path and indexed by integers. Big_map literals in
    Michelson expressions are now either the same as maps or their integer
    index.

    A temporary zone is introduced, necessary to make sure that big_maps
    are not spuriously cleared or left dangling during big_map transfers
    in internal operations. These are represented by negative indexes, and
    don't persist.

- Proto/Michelson: new gas costs

::

    The cost functions in Michelson_v1_gas were to a large extent
    automatically generated. Please refer to meta_model.ml

    The (abstract) cost model makes large use of floating-point
    coefficient. These were converted to either integer
    multiplication/divisions or to statically generated fixed-point
    computations.

- Proto/Michelson: finer-grained cost accounting for the interpreter

::

    I. Rescaling step cost
    - Rescale step_cost by 2^7 to allow finer cost accounting in the
      interpreter.
    - Expose new function atomic_step_cost exposing finer resolution step
      increments.

    II. Provide facilities for interpreter-specific cost accounting

    Introduce new functions `Gas.incr_interpreter_cost` and
    `Gas.bill_interpreter_cost`.

    - The context stores a new counter 'interpreter_cost' of type
      Gas_limit_repr.cost
    - functions are provided to:
      - increment this counter (incr_interpreter_cost) and
      - bill for the gas corresponding to this counter and reset this
      counter. Until bill_interpreter_cost is called, the interpreter_cost
      is _not_ taken into account into the effectively consumed gas.
    - Each call to incr_interpreter_cost still checks that we are under
      the operation and block gas limits.
    - The interpreter uses these functions instead of the usual
      Gas.consume.

    The invariant that has to be respected for this to be transparent to
    the rest of the protocol is that all continuations of the `step`
    function to other functions should bill and reset the interpreter_cost
    beforehand. This concerns calls to interp, calls to the typechecker,
    calls to read from a big map, calls to the
    serialization/deserialization  mechanism, etc; in short, all calls to
    other parts of the protocol should have a context in a state where
    this fine-grained gas bookkeeping has been settled and reset.

- Proto/Michelson: add comparable_ty type witness in boxed sets

::

    Some cost functions require computing the size of keys/elts of
    maps/sets. Not being able to dispatch on the element type was making
    this impossible outside of the interpreter (where the element type of
    the set could be accessed elsewhere). This patch fixes that.

- Proto/Michelson: unshare cost functions of the interpreter & the rest of the protocol

::

    This patch is a refactor to prepare for the gas costs rehaul. It
    dissociates the gas consumed by the interpreter, which is the part
    that is updated according to thorough benchmarking, from other source
    of gas consumption in the protocol (typechecking, serialization etc.)
    which are left untouched in this update.

- Proto/Michelson: extend comparison to linear pair structures

::

    Michelson's `COMPARE` instruction can currently only compare simple
    values (`string`\ s, `int`\ s, etc.). This limitation also applies to
    `set`, `map` and `big_map` indexes.

    This is an issue in particular for `big_map`\ s that cannot be nested,
    because it prevents indexing data by a pair of indexes, such as a
    `key_hash` and a `string`.

    This patch lifts that restriction, allowing to compare `pair`\ s of
    values, as long as their left component remains a simple value,
    implicitly making comparable values right combs whose leaves are simple
    values. The ordering is naturally lexicographic.

    This patch also refactors a bit the comparison code to simplify it and
    reduce code duplication.

- Proto/Michelson: comparisons return -1, 0, or 1, as per the documentation

::

    The Michelson documentation states that `COMPARE` pushes -1 (resp. 1)
    if the top element of the stack is smaller (resp. greater) than the
    second. However, the implementation can actually push a negative
    number instead of -1 and a positive number instead of 1 depending on
    the type and values.

    This semantics should not break any code as the result of `COMPARE` is
    almost always consumed by comparison projectors such as `GT` or `LT`
    who only care about the sign. However, for the sake of consistency,
    this patches makes `COMPARE` return only -1, 0 or 1.

    This fixes issue #546

- Proto/Michelson: add special encoding for ``do`` and ``set/remove_delegate`` entrypoints

::

    This patch optimises the binary representation of transactions to
    usual entrypoints. The `do` entrypoint is used by manager.tz script
    and the `set_delegate` and `remove_delegate` by spendable script
    transformation.

- Proto/Michelson: handle default entrypoint originated before migration

::

    This patch preserves the semantics of `CREATE_CONTRACT` instructions
    for contracts deployed before the migration that deploy a contract
    with a default entrypoint. This is done by adding a `%root` entrypoint
    as detailed in a previous patch.

- Proto/Michelson: Add CHAIN_ID and chain_id

::

    Add an abstract type and an instruction to obtain the chain id from
    Michelson code.

    This is to implement replay protection between the main chain and the
    test chain spawned in phase 3 of the voting procedure.

- Proto/Michelson: new instructions ``DIG n``, ``DUG n``, ``DIP n { code }``, ``DROP n``

::

     - `DIG n` : get the element at top of the n-th tail of the stack and move it to the top. `DIG 0` is a no-op.
     - `DUG n` : get the element at the top of the stack, and move it downwards n slots. `DUG 0` is a no-op.
     - `DIP n { code }` : execute code after removing the top n elements of the stack, and put these n elements back on top of the resulting stack. `DIP 0 { code }` is equivalent to `{ code }`.
     - `DROP n` : drop the top `n` elements of the stack. `DROP 0` is a no-op.

    Smart contract authors should switch to these new instructions in
    their developments.

- Proto/Michelson: corrected error message for the contract type

::

    This is a minor fix for the Michelson typechecker to produce a better
    error message on some ill-typed contracts.

- Proto/Michelson: modify semantics of NOW instruction

::

    The `NOW` instruction now pushes the minimal injection time on the
    stack for the current block/priority, instead of the actual timestamp
    put in the block by the baker.

    This is a change required by the switch to Emmy+, in which a baker
    could decide after having forged a block to include a late endorsement
    and update the timestamp to an earlier point. With the current
    semantics of `NOW`, this would mean reevaluating all operations to
    make sure they are still valid every time such a change is
    decided. This patch prevents that issue by fixing the timestamp seen
    by Michelson independently of the number of endorsements.

- Proto/Michelson: annotation semantics fixes

::

    Contains a BREAKING CHANGE (see end of message).

    Some instructions were missing consistency checks on the annotations
    of their arguments. For instance, it was possible to `CONS` a value of
    type `unit :A` on a `list (unit :B)`.

    Smart contracts already deployed before the migration will continue to
    work even if they present such issues.

    However, smart contract authors should already make sure that their
    annotations are consistent by using the new typechecker in a sandbox.
    This is even more recommended for contracts deployed before the
    migration that use the `CREATE_CONTRACT` instruction. If the code they
    deploy is ill-annotated according to the new stricter rule, these
    contracts will produce failing operations after the migration.

- Proto/Michelson: do not allow annotations inside data anymore

::

    Some Michelson values could bear type annotations. These were
    inconsistent and unspecified. Annotations inside data can now only
    appear inside lambdas.

- Proto/Michelson: option cannot bear field annotations anymore

::

    Contains a BREAKING CHANGE (see end of message).

    Field annotations on `option` types were inconsistent with other field
    annotations on other types, interfering with field annotations on
    their parent type, and the implementation was buggy.

    Smart contract authors should stop putting field annotations on their
    option types, or their contract will not be deployable after the
    migration. It is enough to erase the annotations.

- Proto/Michelson: add services to list entrypoints

::

    This patch adds four new URIs.

     - `/helpers/entrypoint_type`
     - `/helpers/list_entrypoints`
     - `/contracts/index/<KT1>/entrypoints/`
     - `/contracts/index/<KT1>/entrypoints/<name>`

- Proto/Michelson: add lightweight multiple entrypoints

::

    Contains a BREAKING CHANGE (see end of message).

    This patch implements a way for a transaction to target a specific
    code path of a smart contract using a name. The implementation is
    piggy baking on Michelson's or type and field annotations.

    To take advantage of the multiple entrypoint feature, the parameter
    type of a contract must have at its toplevel a tree of `or` types. At
    each branching point in this tree, a field annotation (the ones with a
    %) can appear, providing the name of the entrypoint.

    Transactions now have to specify an entrypoint name. When a
    transaction is executed, the appropriate `Left` and `Right`
    constructors are automatically added to the value that is pushed onto
    the input stack, depending on the position of the entrypoint in the
    parameter type tree.

    This way, two contracts who share an entrypoint of the same type under
    the same name can be called exactly the same, even if the entrypoint
    is placed at a different point in their parameter type tree. From
    inside the smart contract, nothing changes.

    From within Michelson, this feature is also available. The `contract
    t` type now points to a specific entrypoint (of type `t`) of the
    contract. For this, the `CONTRACT` and `SELF` instructions now take an
    optional annotation (set to `%default` if not passed). The
    `TRANSFER_TOKEN` instruction will then use the entrypoint from the
    `contract t` value that it consumes from the stack.

    An exception to the semantics is made for the `%default` entrypoint :
    if present in the contract, it behaves as any other, however if not
    present, default is automatically attributed to the root of the
    parameter type.

    A special check is made at origination that there is no two
    entrypoints with the same name, and that if a default is present
    somewhere, then all entrypoints must be named, as otherwise some parts
    of the code would be unreachable.

    Smart contract developers can already use the feature, and their
    contracts will automatically take advantage of entrypoints after the
    migration.

    Smart contract developers should take great care when deploying
    contracts that use the `CREATE_CONTRACT` instruction, as this
    instruction will produce a failing operation after the migration if it
    tries to deploy a contract with ill formed entrypoints. To prevent
    this, contract authors should test their contract in a sandbox with
    the new protocol, or simply avoid hardcoding the `CREATE_CONTRACT`
    instruction when possible.

- Proto/Michelson: disable storing values of type ``contract t`` in newly originated contracts

::

    Contains a BREAKING CHANGE (see end of message).

    In Athens and before, Michelson contracts could store typed handles to
    contracts in their storage or in constants in the code. This meant
    that typechecking a contract required accessing other contracts from
    the chain context. This extra type safety was not worth the
    engineering cost for tooling and high level languages. Contracts will
    now have to store values of type `address` and use the `CONTRACT`
    instruction to typecheck contract references on demand.

    All existing contracts that used the feature will continue to work
    as-is. This is done by introducing a `legacy` flag throughout the
    typechecking code, with the following trivial semantics:
     - everything already in the chain is considered `legacy` and can
       use deprecated features,
     - everything added to the chain (parameters of transactions and code
       and storage of originations cannot.

    Smart contract developers should adapt their code to store `address`\ es
    and use instruction `CONTRACT`.

- Proto/Michelson: eliminate useless storage read for parse_contract

::

    This patch removes a spurious access to the storage when typechecking a
    contract reference. It makes this operation cheaper in gas.

- Proto/Michelson: peephole optimization of UNPAIR

::

    This makes the often used `UNPAIR` macro cheaper in terms of gas.


Governance
~~~~~~~~~~

- Proto: Require 5% minimum quorum of protocol proposal

::

    Contains a BREAKING CHANGE (see end of message).

    The protocol will now remain in the initial proposal voting phase
    until a protocol gets upvoted by at least 5% of the stake.

    Third party developers should make sure they can parse the new fields
    in the `/constants` RPC, or at least ignore them.

- Proto: participation EMA and min/max quorum caps

::

    Contains a BREAKING CHANGE (see end of message).

    Change the formula from quorum update on vote period to participation
    EMA (exponential moving average). Current quorum storage is removed
    and new storage participation EMA is introduced.

    Minimum and maximum quorum caps are added to the constants of the
    economic protocol. Whenever a voting period would cause the quorum to
    go below or above the caps it will be bound to the limit defined in
    the constants.

    In the future token holders can easily modify the caps by changing the
    constants.

    Third party developers should make sure they can parse the new fields
    in the `/constants` RPC, or at least ignore them.


Accounts rehaul
~~~~~~~~~~~~~~~

- Proto: all KT1s must now be scripted

::

    Contains BREAKING CHANGES (see end of message).

    It removes the manager, spendable and delegatable flags and counter from all KT1s.

    It deprecates CREATE_ACCOUNT from use in new contracts, as well as the
    manager, spendable and delegatable arguments from CREATE_CONTRACT.

    Already deployed contracts with deprecated instructions will continue
    to work by using legacy support scripts (deploying `manager.tz` for
    `CREATE_ACCOUNT` and adding entrypoints for `CREATE_CONTRACT`).

    This change will impact all users of the RPC API as well as anyone who
    forges operations. The source of manager operations is now a tz1, tz2
    or tz3, and no longer a KT1. The manager field and the spendable and
    delegatable flags disappear from the origination operation format
    (JSON and binary) as well as everywhere in the RPC API.

- Proto: add code stubs to handle ``%default`` entrypoints originated before migration

::

    This code stub adds a `%root` entrypoint to contracts that have a
    default entrypoint, and rewrite their calls to `SELF` into `SELF
    %root`. This is used to preserve the typing of `SELF` within contracts
    with deployed before the migration that have a `%default` entrypoint.

- Proto: add Michelson code stubs to replicate manager operations on KT1s

::

    Spendable, scriptless contracts are simulated by the 'manager.tz' script,
    which replaces their functionality. It allows for the contract's manager to set
    and withdraw delegate, spend the contract's funds and to set a new manager,
    which is written into script's storage.

    The 'manager.tz' script's parameters have field annotations, which in
    combination with script entry-points allows for friendlier commands for
    running the script.

    Spendable and delegatable flags are simulated by adding entrypoints to
    a scripted contract.

- Proto: make implicit accounts delegatable

::

    Contains BREAKING CHANGES (see end of message).

    Implicit accounts (tz1, tz2, tz3) can directly set their
    delegate. Furthermore implicit accounts have the ability to delete
    their delegate by sending a "delegate" transaction with an empty
    delegate field.  This specific patch does not impact the ability for
    originated (KT1) accounts to set or delete their delegate.

    The storage type of the "Delegated" accounts changes it's index from
    "Contract_hash" to "Contract_repr.Index". This change in the type
    signature allows that both implicit and originated accounts can be
    stored in the set.

    Explorers and wallets should handle the delegation from tz1, tz2 and
    tz3 accounts. RPC `/context/delegates/<pkh>/delegated_contracts` (and
    composite RPC `/context/delegates/<pkh>/`) can now contain tz1, tz2
    and tz3 addresses.


Migration
~~~~~~~~~

- Proto/Migration: switch scripted KT1s to new ``big_map`` storage

::

    This patch looks for big_maps in existing smart contracts, and moves
    them to their new storage path.

- Proto/Migration: handle default entrypoint originated before migration

::

    This patch updates contracts deployed before the migration with a
    `%default` entrypoint. This is done by adding a `%root` entrypoint as
    detailed in a previous patch.

- Proto/Migration: update deployed multisigs to the newest supported version

::

    Contains a BREAKING CHANGE (see end of message).

    This does not change the behaviour of the multisig. It adds a call to
    the newly introduced `CHAIN_ID` instruction in order to add extra
    replay protection between the main chain and the test chain.

    Smart contract users that do not use the `tezos-client` but a custom
    tool to interact with multi-signature contracts deployed with the
    `tezos-client` should also include the `CHAIN_ID` in the commands they
    sign.

- Proto/Migration: migrate KT1s with and without script

::

    Contains a BREAKING CHANGE (see end of message).

    All spendable, scriptless contracts are migrated to 'manager.tz' script.

    Contracts that have a spendable flag set are augmented with a `%do`
    entrypoint. Contracts that have a delegatable flag set are augmented
    with `%set_delegate` and `%remove_delegate` entrypoints.

    Interacting with converted contracts must now be done via smart
    contract calls. As an example, here is how `tezos-client` handles
    retro-compatibility for the `transfer` and `set delegate` commands.

    When crafting a transaction, if the source is a KT1, if checks that
    its storage is either of type `key_hash` or `pair key_hash _`, and
    retrieve this `key_hash`. Let's name this `key_hash` <manager>.

    To implement `tezos-client set delegate for <contract> to <delegate>`,
    it starts by looking for entrypoints.

    If `%set_delegate` is present, it does the equivalent of
      'tezos-client transfer 0 from <manager> to <contract> \
                      --entrypoint 'set_delegate' --arg '<delegate>'
    where <manager> is the key_hash found in the contract's storage

    If `%do` is present, it does the equivalent of
       'tezos-client transfer 0 from <manager> to <contract> \
                     --entrypoint 'do' \
                     --arg '{ NIL operation ; \
                              PUSH key_hash <delegate> ; \
                              SOME ; \
                              SET_DELEGATE ; \
                              CONS }'
       where <manager> is the key_hash found in the contract's storage

    To implement `tezos-client transfer <amount> from <contract> to <destination>`,
    when the destination is a simple address or a contract of type `unit`,
    it does the equivalent of
    ```
    tezos-client transfer 0 from <manager> to <contract> \
                 --entrypoint "do" \
                 --arg '{ NIL operation ; \
                          PUSH address <destination> ; \
                          CONTRACT unit;
                          AMOUNT ; \
                          UNIT ; \
                          TRANSFER_TOKENS ; \
                          CONS ; \
                          PAIR }'
    ```

    To implement `tezos-client transfer <amount> from <contract> to <destination> \
                    [--arg <value>] [--entrypoint <entrypoint>]`,
    it starts by checking that the contract has a `%do` entrypoint.

    Then it look for type `<entrypoint>` of contract `<destination>` in the chain

    And it does the equivalent of
    ```
    tezos-client transfer 0 from <manager> to <contract> \
                 --entrypoint "do" \
                 --arg '{ NIL operation ; \
                          PUSH address <destination> ; \
                          CONTRACT %<entrypoint> <type>; # Omit <entrypoint> if not given
                          AMOUNT ; \
                          PUSH <type> <value> ; \ # UNIT if <arg> not given
                          TRANSFER_TOKENS ; \
                          CONS ; \
                          PAIR }'
    ```

- Proto/Migration: new constant min_proposal_quorum

::

    This patch initializes the newly introduced min_proposal_quorum
    protocol parameter to 5%.

- Proto/Migration: migrate the values of 'Contract.Delegated' storage

::

    This patch migrates the context according to the previous patch.

- Proto/Migration: migrate last_block_priority to block_priority

::

    This patch migrates the context to include the current block priority
    instead of the one of the predecessor. This is needed for the new
    block reward schema introduced by the previous patch.

- Proto/Migration: constants for Emmy+

::

    This patches stores the initial values for the new protocol parameters
    introduced by Emmy+.

- Proto/Migration: participation EMA and min/max quorum caps

::

    This commit amends the context. It uses the
    last value of current quorum for participation EMA and adds
    min/max quorum caps to it. Initially the minimum quorum cap
    is set to 20% and the maximum to 70%.

- Proto/Migration: add all constants in the context

::

    This patch does not change the semantics.

    It migrates the stored constants in a way compatible with the new
    format defined by the previous patch.

    In the previous format, only parameters different from the (now
    removed) default values were stored. Now all parameters are stored
    explicitly.

- Proto/Migration: add invoicing to multi-sig smart-contract
