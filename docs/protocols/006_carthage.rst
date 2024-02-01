Protocol Carthage
=================

This page contains all the relevant information for protocol 006 Carthage (006_PsCARTHA).
Each of the main changes is briefly described with links to relevant
external documentation and merge requests.
There are dedicated sections for all the changes to RPCs and
operations.
The changelog section contains the most significant commit messages
and instructions to regenerate the protocol sources from the
Gitlab branch.

Test network Carthagenet is available to test Carthage.
See details in :ref:`Test Networks<test_networks>`
and instructions to join in :ref:`How to get Tezos<howtoget>`.

The code can be found in the Gitlab branch
`proto-006`_ and its
full hash is ``PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb``.

**This protocol contains several breaking changes with respect to Babylon.**
Developers are particularly encouraged to carefully read this page and
to monitor it for updates.

.. contents:: Summary of changes

Baking Daemon
-------------

The baking daemon requires direct access to the context of the Tezos node.
The daemon for 006 requires the new context introduced by Irmin2. As such
bakers that use the default baker need to upgrade to the new storage
backend in order to be able to run the 006 baking daemons.

Smart Contracts
---------------

The gas limit per block and per operation was increased by 30%. For
operations it changed from 800,000 to 1,040,000 and for blocks it
changed from 8,000,000 to 10,400,000.

Baking and Endorsing
--------------------

The formula to calculate baking and endorsing rewards was improved
in order to provide more accurate results.

The formula was further modified in order to make it more resistant
to certain types of attacks. A full explanation can be found
`here <https://research-development.nomadic-labs.com/a-new-reward-formula-for-carthage.html>`_.

Accounts
--------

The assert that was triggered when a delegated account tried to empty
itself was replaced by a proper error message.

Michelson
---------

Protocol 006 contains several improvements to the Michelson smart
contract language.

Optimisation of the ``CONTRACT`` instruction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``CONTRACT`` instruction has been optimized to avoid performing a
useless disk access in the case its argument is the address of an
implicit (aka. tz) account. In this case, this small optimisation
saves 132 gas units.

Comparable pairs in sets and maps
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Comparability of pairs was added in the Babylon protocol; it is
possible in Babylon to use the ``COMPARE`` instruction to
lexicographically compare two pairs. However, due to a missing case in
the type-checker for comparable types, the Babylon implementation of
comparable pairs did not allow the use of pairs as elements in sets nor
keys in maps and big maps. This is fixed in Carthage.

Fixing MAPping on maps with side effects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``MAP`` instruction can be used in Michelson to apply a function to
each element of a list or each value of a map, producing respectively
a ``list`` of the same length as the original or a ``map`` with the same
keys.

During the development of a new unit test suite for the Michelson
language, the `Runtime Verification <https://runtimeverification.com/>`_
team discovered that the Michelson interpreter incorrectly handled
side effects in the ``map`` case of the ``MAP`` instruction. Until
Babylon, if the body of the ``MAP`` instruction modifies the remaining
of the stack, then these changes are reverted when the ``MAP``
exits. This behaviour is consistent with neither the Michelson
documentation nor the ``list`` case; it has been fixed in Carthage.

This change is not backward compatible so we have inspected the
current state of the mainnet chain and we have checked that, at time
of writing, no contract is affected by this bug. Until the activation
of Carthage (or any protocol including this fix), smart contract
authors should avoid relying on the bogus behaviour of the ``MAP``
instruction on maps by using the ``ITER`` instruction instead when
they need to perform side effects on the remaining of the stack during
an iteration.

Dead optimisation of the ``UNPAIR`` macro
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``UNPAIR`` macro is very commonly used in Michelson to destruct
pairs. In order to encourage its use, it received a special treatment
in Babylon by which its gas cost was artificially
decreased. Unfortunately, a small mistake in the unfolding of the
``UNPAIR`` macro made this special treatment dead code; the interpreter
is looking for the sequence ``{DUP; CAR; DIP CDR}`` but the unfolding of
``UNPAIR`` is actually ``{DUP; CAR; DIP {CDR}}`` (note the extra pair of
curly braces around ``CDR``). Moreover, the `Babylon gas
update <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/73>`_ has
made this peephole optimisation of the ``UNPAIR`` macro much less
interesting because the gas costs of all stack and pair instructions
are much lower than in previous protocols. We plan to promote ``UNPAIR``
as a new Michelson instruction in a future protocol proposal.

Error message for ``EMPTY_BIG_MAP`` arity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``EMPTY_BIG_MAP`` instruction, which was added in Babylon and can be
used to push an empty ``big_map`` on the stack, expects two parameters
(the types for keys and values). When the instruction is used with
another arity, the error message produced in Babylon was unclear
because of a missing case in the type checker. This missing case has
been added and the error message is clearer in Carthage.

Typechecking ``big_map`` literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The typechecking RPCs ``typecheck_script`` and ``typecheck_data`` are
useful tools for Michelson editors featuring typechecking. The
``typecheck_data`` RPC was restricted to non-``big_map`` types for no good
reason. This limitation has been removed; it is possible in Carthage
to typecheck ``big_map`` literals.

Checking validity of annotations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Annotations are enforced to only contain valid JSON.


Changes to RPCs
---------------

*BREAKING CHANGES*: the semantics of the ``baking_rights`` RPC and the
return values of the ``block_reward`` and ``endorsement_reward`` RPCs
have changed.

Below you can find all the RPC changes.

Baking_rights
~~~~~~~~~~~~~

In Babylon the argument ``max_priority`` causes the RPC to return
the rights up to ``max_priority`` excluded, for example setting
``max_priority=0`` returns the empty list.
In Carthage the value of ``max_priority`` is included, for example
``max_priority=0`` returns the rights of priority zero.

Block_reward
~~~~~~~~~~~~

This constant is accessed by calling ``/chains/main/blocks/head/constants``,
which returns a JSON object where the field ``block_reward`` was renamed to
``baking_reward_per_endorsement`` and its value was changed from a single
value to a list of values.

Endorsement_reward
~~~~~~~~~~~~~~~~~~

This constant is accessed by calling ``/chains/main/blocks/head/constants``,
which returns a JSON object where the value of the field ``endorsement_reward``
was changed from a single value to a list of values.


Changes to the binary format of operations
------------------------------------------

There are **no changes** to the binary format of operations.


Changelog
---------

You can see the full git history on the branch `proto-006`_.
In order to regenerate a protocol with the same hash as Carthage you
can run from this branch::

  $ ./scripts/snapshot_alpha.sh carthage_006 from babylon_005
  $ ls src/proto_006_PtXXX


List of Merge Requests
~~~~~~~~~~~~~~~~~~~~~~

* `Baking RPC <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/108>`_
* `Baking/Endorsement Formula <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/110>`_
* `Empty implicit delegated contract <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/112>`_
* `Gas Limit Increases <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/117>`_
* `Dead Code Removal <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/118>`_
* `Comparable Pairs <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/106>`_
* `Michelson Fix for MAP instruction <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/120>`_
* `Modified the Emmy+ reward function <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/134>`_
* `Improve CONTRACT instruction <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/95>`_
* `Improve BIG_MAP error message <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/121>`_
* `Check validity of annotations <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/135>`_
* `Move BIG_MAP initialisation <https://gitlab.com/nomadic-labs/tezos/-/merge_requests/119>`_


Detailed Changelog
~~~~~~~~~~~~~~~~~~

- Proto: remove .ocamlformat-ignore and make fmt

::

   Apply the ocamlformat tool to the protocol codebase.

- Protocol/Migration: remove babylon's vanity nonce

- Protocol/Storage: initialize big_map ids only for genesis

- Protocol/RPC: fix 'baking_rights' so that 'max_priority' is included

::

   Fix a bug where the `../helpers/baking_rights` RPC would exclude the
   `max_priority` baking right from its result.

   BREAKING CHANGE: the semantics of the `baking_rights` RPC has changed

- Protocol/Emmy+: fix baking and endorsement reward formulae

::

   Fix the imprecision in the baking reward formula to make it linear in
   the number of endorsements included instead of a step function.

   Improve the precision on the endorsement reward computation by
   applying the priority malus on the total endorsement reward.

- Protocol/Michelson: fix comparable comb pairs

::

   Allow comb pairs as map keys and set elements, not only as operands of
   COMPARE.

- Protocol/Michelson: allow all parameter types when typechecking a literal

::

   Extend the range of the typecheck_data RPC by also allowing big_map
   values.

- Protocol/Gas: increase the gas limits per block and operation by 30%

::

   Bump the gas limit for blocks and operations by 30% going from 800000
   per operation and 8000000 per block to 104000 per operation and
   1040000 per block.

- Protocol/Migration: bump gas limit constants in the context

::

   Update the gas limit constants in the context on protocol transition.

- Protocol/Michelson: remove the peephole optimisation of UNPAIR

::

   Remove an unreachable optimisation. A proper UNPAIR instruction shall
   be added in the next protocol instead.

- Protocol/Michelson: handling of the bad arity error for the EMPTY_BIG_MAP instruction

::

   Improve error reporting when checking for the arity of the
   EMPTY_BIG_MAP instruction

- Protocol/Michelson: fix the interpretation of the MAP instruction on maps

::

   In the previous implementation, accumulating a value during a MAP on a
   map was impossible because the initial stack tail was restored. This
   was not the documented behavior of the MAP instruction and it was
   inconsistent with the case of mapping over a list.

   BREAKING CHANGE: originated contracts that rely on the previous (and
   incorrect) semantics might behave incorrectly.

- Protocol/Michelson: improve the performance of the CONTRACT instruction

::

   Add an optimisation that make the instruction cheaper in gas for
   implicit contracts (tz1, tz2, tz3) by saving an I/O.


.. _proto-006: https://gitlab.com/nomadic-labs/tezos/-/tree/proto-006
