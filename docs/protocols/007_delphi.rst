Protocol Delphi
===============

This page contains all the relevant information for protocol 007 Delphi (007_PsDELPH1).
Each of the main changes is briefly described with links to relevant
external documentation and commits.
The changelog section contains the most significant commit messages
and instructions to regenerate the protocol sources from the
Gitlab branch.

Test network Delphinet is available to test Delphi.
See details in :ref:`Test Networks<test_networks>`
and instructions to join in :ref:`How to get Tezos<howtoget>`.

The source code of this proposal is available in `this tar archive
<https://research-development.nomadic-labs.com/files/delphi_007_PsDELPH1.tar>`_ and its
full hash is ``PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo``.

**This protocol contains several breaking changes with respect to Carthage.**
Developers are particularly encouraged to carefully read this page and
to monitor it for updates.

.. contents:: Summary of changes

Performance Improvements
------------------------

Some effort was directed to streamline the Michelson interpreter and
improve its performance. This mostly consisted in factoring gas
consumption, simplifying logging and removing some calls to ``Lwt``
(cf. the dedicated section on that last point).

Patches:

-  217b4fd025 Proto/Michelson: abstract logging facility
-  7504b35b94 Proto/Michelson: make lists carry their size and type of
   elements
-  c744b2e36e Proto/Michelson: hoist gas consumption in
   Slice_{string,bytes}
-  9854ddc12a Proto/Michelson: hoist Gas.consume out of interp. pattern
   matching

Gas Changes
-----------

Gas accounting has seen a significant update. Even though gas limits are
the same as in Carthage, the amount of computation per unit of gas
should see a significant increase in Delphi.

More precisely, the following parts of the gas subsystem have been
updated. - Following the optimization work on the interpreter, gas costs
for all instructions have been recomputed. - The cost model for IOs has
been updated and should account better for performances of modern
storage hardware. - The typechecking system has an entirely new cost
model. - Finally, the base gas cost of manager operations has been
reduced from 10000 to 1000 units of gas.

Overall, users should see significant gas cost reductions accross the
board.

Patches:

-  8747f0a987 Proto/Michelson: Use the right cost function for
   SET_DELEGATE instr
-  6825b14cc0 Protocol/Michelson: fix gas consumption in Loop exit case
-  267ddd55d9 Protocol/gas: fix scaling of gas accounting for number of
   writes
-  f1397bffb6 Proto/gas: make internal gas visible in interpreter traces
-  1a874fb990 Proto/gas: rescale gas by 2^7, making internal_gas always
   0
-  866fa99e71 Proto/gas: remove internal_gas
-  3eab3718a4 Proto/Michelson: properly carbonate
   extract_big_map_updates and collect_big_maps
-  a5baf0ed59 Proto/Michelson: Remove gas cost of ‘map_to_list’ and
   ‘set_to_list’ conversions
-  1ae30c0932 Protocol/Migration: scale gas limit constants in the
   context by 128
-  76918bf6fa Proto/Gas: remove intermediary cost record
-  3e11d74d91 Proto/Gas: factor calls to scaling function
-  07f89a7a9e Proto/Gas: inline calls to scale
-  c67f0dd61c Proto/Gas: perform rescaling from x128 to x1000
-  786bdb360f Proto/Gas: arithmetic optimization (remove multiplication
   by 1)
-  6c611b948d Protocol/Migration: scale gas limit constants in the
   context by 1000
-  5dfb79a814 Revert “Protocol/Migration: scale gas limit constants in
   the context by 1000”
-  fbccb2390a Revert “Vendors/flextesa: scale gas limit constants in the
   context by 1000”
-  f3201231f2 Revert “Protocol/Migration: scale gas limit constants in
   the context by 128”
-  b50d6f870a Revert “Vendors/flextesa: scale gas limit constants in the
   context by 128”
-  27bb7aeb2d Proto/gas: set 1 milligas = 1 atomic_step_cost
-  7e50a74755 Proto/Gas: add new gas cost functions in dedicated module
-  097e79f626 Proto/Gas: inject new cost functions in ``Gas.cost``,
   expose in .mli
-  78a2e8069c Proto/Gas: plug new costs on the interpreter
-  02b5fc516c Proto/Gas: fix gas for Concat_string & Concat_bytes
-  e00ec5eff8 Proto/Gas: unplug 006 interpreter gas
-  1838b7da21 Proto/Gas: introduce Storage_costs module
-  a21c1ed287 Proto/Gas: use Storage_costs in carbonated storage
   functors
-  3281de90c2 Proto/Gas: introduce Contract.get_balance_carbonated
-  2e1a9d88e3 Proto/Gas: adding encoding/decoding cost constants
-  68b4c2c37d Proto/Gas: injecting Gas_limit_repr.cost into
   Alpha_context.Gas.cost
-  33a0d8f015 Proto/Gas: expose 007 typechecking costs
-  be9c3159d8 Proto/Gas: Adapt translator to 007 typechecking costs
-  bd205ad65e Proto/Gas: expose 007 unparsing costs
-  eb784b35be Proto/Gas: adapt translator to 007 unparsing costs
-  9b478326fd Proto/Gas: add cost functions and helpers for
   strip_locations
-  b574bff205 Proto/Gas: carbonate calls to Micheline.strip_locations
-  6e6d59f578 Proto/Gas: Unplug 006 translator gas
-  9ae9b3c771 Proto/Gas: prettify constants
-  987c4f93a7 Proto/Gas: remove useless ``let`` bindings in cost
   functions
-  4ffc37e769 Proto/tests: add basic unit test for cost functions
-  5ef9a9aae1 Proto/Gas: reduce gas cost of manager op

Change in format of receipts related to gas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The protocol now internally handles *milligas* but exposes gas to the
user. - Protocol constants such as per-block and per-operation gas
limits are **still specified in gas**. - On the input side of the
protocol, the ``gas_limit`` field in manager operations is **still
specified in gas**, as before. - On the output side, receipts contain
both the ``consumed_gas`` (as before) and a new field
``consumed_milligas``. The relationship between this two fields is the
following:

.. math::


   consumed\_gas = \lceil consumed\_milligas / 1000 \rceil

In other terms, ``consumed_gas``, provided for backwards compatibility,
is the rounded-up version of ``consumed_milligas``. Users wanting to
have the most accurate description of the gas consumption of their
transfers should consider using ``consumed_milligas``.

To make understanding the change easier, here is a small example
illustrating a transfer in sandboxed mode to a toy ``minimal.tz``
contract:

::

   { parameter int ;
     storage unit ;
     code { CDR ; NIL operation; PAIR } }

Let us originate this contract.

::

   tezos-client -l originate contract mini transferring 0 from bootstrap1 running minimal.tz --burn-cap 0.07375 --init 'Unit'

During simulation, the ``preapply`` RPC is given the following
operation:

::

   { "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
     "branch": "BMSAKmQD2Q2Ghk2jtHMGpx5Zww98oLsFdh4XW1863TizUjyfY8S",
     "contents":
     [ { "kind": "origination",
         "source": ..., "fee": "434", "counter": "1",
         "gas_limit": "1690",
         "storage_limit": "315",
         "balance": "0",
         "script": ... }

The ``gas_limit``, equal to ``1690`` in this example, is labelled in
**gas**. The outcome of the simulation is the following receipt:

::

   { "kind": "origination",
               "source": ..., "fee": "434",
               "counter": "1",
               "gas_limit": "1690",
               "storage_limit": "315", "balance": "0",
               "script": ...,
               "metadata":
                 { "balance_updates": ...,
                   "operation_result":
                     { "status": "applied", "big_map_diff": [],
                       "balance_updates": ...,
                       "originated_contracts": ...,
                       "consumed_gas": "1590",
                       "consumed_milligas": "1589562",
                       "storage_size": "38",
                       "paid_storage_size_diff": "38" } } }

The metadata contains both: - the field ``consumed_milligas``,
corresponding to the **exact** amount of gas consumed, labelled in
**milligas**; - the field ``consumed_gas``, obtained by ceiling the
milligas value as described above, labelled in **gas**.

Note that the ``tezos-client`` binary prints gas, not milligas. However
it displays three decimals:

::

   This sequence of operations was run:
     Manager signed operations:
       From: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
       Fee to the baker: ꜩ0.000434
       Expected counter: ...
       Gas limit: 1690
       Storage limit: 315 bytes
       Balance updates:
         tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ........... -ꜩ0.000434
         fees(tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx,0) ... +ꜩ0.000434
       Origination:
         From: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
         Credit: ꜩ0
         Script:
           { parameter int ; storage unit ; code { CDR ; NIL operation ; PAIR } }
           Initial storage: Unit
           No delegate for this contract
           This origination was successfully applied
           Originated contracts:
             KT1HUgv4V8RnEhzNXUxWSqU2PGZ6MgRh34n3
           Storage size: 38 bytes
           Paid storage size diff: 38 bytes
           Consumed gas: 1589.562
           Balance updates:
             tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.0095
             tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.06425

Patches:

-  7868fa947b Proto/gas: introduce Fixed_point_repr module
-  c436869c49 Proto/tests: unit tests for Fixed_point_repr
-  5a4f408582 Proto/gas: edit TEZOS_PROTOCOL for Fixed_point, move
   Gas_limit_repr
-  e939017844 Proto/gas: use fixed-point computations for gas (FIXME)
-  6031a0dc8c Proto/client: use fixed-point computations for gas
-  7d9f76418d Proto/delegate: use fixed-point computations for gas
-  804b42cd86 Proto: restore format of receipts to a retrocompatible
   schema

Lowered storage costs
---------------------

In Tezos, storing data in the state leads to a burn of tez proportional
to the size of the storage increase. This happens when creating a new
account, originationg a new smart contract or making the storage of a
smart contract grow above its historical higher size. This is different
from fees that are paid to the baker proportionally to the size of
operations. Indeed, operations are transient (and can be forgotten by
nodes in the lighweight rolling mode), while data in the state is
replicated by all nodes and can be so forever.

In Delphi, the amount of tez burned to store data in the ledger’s state
is decreased by a factor of 4, going from 1 tez to 0.25 tez for a
kilobyte. The price to create a new account is thus lowered from 0.257
tez to 0.06425.

Patches:

-  8f808e8317 proto/parameters: reduce cost_per_byte to 0.000250tz
-  ffaba729db proto/migration: set cost_per_byte to 0.000250tz

Bug fixes
---------

Compatibility with 32-bit platforms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Carthage has some parts that assume a 64-bit runtime. These
modifications restore the compatibility with 32-bit systems, and in
general make the code clearer and less dependent on the underlying word
size.

Patches:

-  da91297c0d Protocol/Michelson: avoid overflowing [Int32.to_int
   Int32.max_int]
-  377af3acb0 Proto/Michelson: Simplify parse_uint30
-  06c2f6f97b Protocol: Safer Int64.to_int
-  40f9a2c9a3 Storage: rename Int -> UInt16
-  ceb4ef33ba Storage: rename Int_index -> Int31_index
-  70c0aa4641 Proto/Gas: Zarith-ify some cost functions

BREAKING CHANGE: Michelson annotations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Field annotations in Michelson types were not properly checked and could
contain invalid characters. In particular, it was possible to use a
digit as the first meaningful character of a field annotation but only
if it appeared in a type; a few contracts on Mainnet contain such
numerals as first meaningful character. We have added the missing check
and extended the syntax of Michelson annotations to allow digits. At the
time of writing, all Mainnet and Carthagenet contracts successfully
typecheck.

BREAKING CHANGE: If a smart contract containing a non-numeral invalid
character in a field annotation inside a type were to be originated
before the activation of Delphi, such a contract would be locked by the
activation.

Patches:

-  ec1d992e5c Use plain algebraic types for the Michelson annotations
-  642bab2f97 Proto/Michelson: ensures all annotations are checked
-  0f12f628e9 Proto/Michelson: extend the set of allowed Michelson
   annotations
-  1b179aeb0b Proto/Michleson: make annotations on ``Right`` and ``Elt``
   consistent

Miscellanous bug fixes
~~~~~~~~~~~~~~~~~~~~~~

Patches:

-  8c1dd8e53b Proto/Michelson: fix the arity check for chain_id
-  dde9e19d55 Proto/RPC: return all delegates if no flags were used
-  28b8181a8c Proto/Michelson: fix registration of error
   Invalid_syntactic_constant
-  7f329a1700 Proto/Michelson: add missing cases in typechecking error
   reporting
-  083e9c6f9b Proto: Add missing case for keyword namespace encoding
-  b1af688dfd Proto: for transfers with 0 tz, check if target exists
-  982dd6ec77 Proto: Update ``max_revelations_per_block`` to include
   anon ops
-  508b3ae5f4 Proto/Michelson: readable unparsing of chain ids
-  b03b8b0883 Proto: Fix double encoding of big_map_diff
-  ae0626d708 Proto: make signature check of operation packs more
   efficient

General code cleanup and refactoring
------------------------------------

Removal of ``Lwt`` when possible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A large batch of changes consists in mostly trivial modifications that
drop parts of the code that don’t perform IOs out of the IO monad. This
makes the code a bit less uniform, but helps with safety and future
refactoring, and in some cases increases performance (in particular in
the interpreter).

Patches:

-  9e38b8fece Proto/Michelson: remove some Lwt wrappers
-  eda54e8ba3 Proto/Michelson: avoid Lwt.bind when logging
-  187d9d7cab Proto: bind -> to
-  b2f8aac20c List.map -> fold_left_s
-  85592c26e9 Proto: filter_map_s -> filter_s
-  14eda1e7a9 List.fold_left -> Error_monad.map
-  43306a4b89 Script: force_decode/bytes, get rid of Lwt.t
-  7b1199d76d Baking: minimal_(valid\_)time, get rid of Lwt
-  c5a0f66bb0 Baking.earlier_predecessor_timestamp: get rid of Lwt
-  e112d592b2 Baking: baking/endorsing_reward, get rid of Lwt
-  622ac83eaf Baking/tests: baking/endorsing_reward, get rid of Lwt
-  287985b4c7 Baking.check_fitness_gap: get rid of Lwt
-  a9cdf7e3bc Contract_storage.fresh_contract_from_current_nonce: get
   rid of Lwt
-  aef7541f69
   Delegate_services.required_endorsements/minimal_valid_time: get rid
   of Lwt
-  3854c15614 Fees_storage.origination_burn: get rid of Lwt
-  d71fe7c127 Operation.check_signature: get rid of Lwt
-  419d29b41b Proto: Lwt.return >>=? -> >>?=
-  36172136a2 Raw_context.add_fees/rewards/deposit: get rid of Lwt
-  f19e52bd86 Script_ir_annot: get rid of fail_unexpected_annot
-  b55bfc4673 Script_ir_translator: remove lots of Lwt in parse_instr
-  4381c2b266 Script_ir_translator.parse_data: remove lots of Lwt
-  509decd748 Script_ir_translator.parse_data: traced
-  8da99eff1a Script_ir_translator: less Lwt
-  407e460bce Script_ir_translator.collect_big_maps: get rid of Lwt
-  1c94741b2b Storage: less Lwt
-  30fad4cb40 Proto: fail_unless/when -> error_unless/when
-  01aeae86a3 Proto: use predefined ok constants
-  0b8964ad8f Proto: lift some Lwt.return

Miscellanous code improvements & refactors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A long series of patches consists in minor improvements to the code
style and contents of comments or error messages. Part of this work is
thanks to the automated transcription of the code of the protocol into
Coq (as part of our formal verification effort).

Patches:

-  88bc601fe9 Proto: Uniform variable names for context + comments
   clean-up
-  6d59b3f32a Michelson: exposes parsing without specifying storage
-  bef609828f Proto: expose parse_storage
-  abb3a3a48c Include ty_eq in merge_types
-  b1519b85e2 Express (comparable\_)ty_eq in terms of
   merge_(comparable\_)ty
-  d96d1cc2d1 Migration: remove leftovers Alpha_previous and Babylon_005
-  cd4bcec080 Proto/Michelson: remove dead code in parse_instr
-  74db352afd Remove mutual dependency between numeric types
-  60b8998900 Proto: remove unused exported vals
-  83aa3f49a5 Rewrite some when clauses for Coq
-  4b5307d995 Remove useless recursion
-  f34ec2a28d Proto: Fix formatting
-  99d7b81533 Proto: Move and export Michelson prim namespace function
-  29153f27ff Proto/Michelson: simplify the interpreter
-  36cbee348b Michelson: rename Left/Right as Cons_left/right
-  c6cba0db30 Michelson: simplify GADT matchings in script_ir_translator
   1/4
-  e828029fb3 Michelson: simplify GADT matchings in script_ir_translator
   2/4
-  0bff45c7f2 Michelson: simplify GADT matchings in script_ir_translator
   3/4
-  edfae2bcf8 Michelson: simplify GADT matchings in script_ir_translator
   4/4
-  cff2ab3b69 Proto/Michelson: extract_big_map_updates: aux
-  425f3eaaa1 Proto/Michelson: compute has_big_map only when needed
-  3b59480b92 Proto/Michelson: get rid of has_big_map flag and old
   function
-  dd34270a63 Proto/Michelson: factorize parse_storage_ty
-  8d17e58cc4 Proto/RPCs: use parse_packable/parameter_ty instead of
   parse_ty in services
-  09013da929 Proto/Contracts: big_map_diff/Copy: use inline record
-  084037dc9c Proto/Michelson: remove dead code
-  5e83fe9126 Proto/Michelson: simplify Big_map.fresh
-  0172dec928 Proto/Michelson: expose parse_ty for convenience to
   external tools
-  6c8e8a7f46 Proto/Michelson: expose unparse_code in
   Script_ir_translator
-  d793d30003 Proto_alpha: spell check
-  5807a28c44 Proto/coq-of-ocaml: Remove a polymorphic variant in
   raw_context
-  5da72db33f Proto: update the old operator to regular naming scheme
-  03ab590364 Protocol/coq-of-ocaml: name the signatures of the protocol
-  a06caece9f Protocol/coq-of-ocaml: avoid a name collision on encoding
   in the generated Coq
-  a25feb04f5 Use Option.value
-  12c7cf2f81 Protocol/coq-of-ocaml: rename of_seconds to prevent a
   collision of name
-  b8dd4fc1f9 Protocol/coq-of-ocaml: renaming of force_decode /
   force_bytes to prevent collision
-  c42b58c8c3 Protocol/coq-of-ocaml: rename consume and check_enough to
   prevent collisions
-  c4ac279d65 Protocol/coq-of-ocaml: renaming to compile alpha_context
-  d6ec7dd8c7 Protocol/coq-of-ocaml: lint the interpreter
-  da7d945551 Protocol/coq-of-ocaml: changes to compile storage.ml
-  75dad446a4 Protocol/coq-of-ocaml: add signature annotations
-  036c287157 Script_ir_translator.parse_data: rename error
-  b25d542877 Proto: fix typo in docstring
-  fe988f439c Proto: remove dead code

Safety of Michelson
-------------------

Explicit limitations in the Michelson typechecker and interpreter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Up until now, some features of Michelson were implicitly limited by gas.
This series of changes add explicit limits that will help with safety,
future refactoring, and debugging (more precise errors).

-  Deep stack instructions ``DIG``, ``DUG``, ``DROP``, and ``DIP`` are
   now bounded to stacks of less than 1024 elements
-  The interpreter (resp. typechecker) now have explicit recursion
   limits, setting a maximum depths for terms that can be interpreted
   (resp. typechecked).

Both changes set limits to high enough values, and should thus be
invisible to almost all contract authors. Reaching the limits is only
likely to happen when debugging erroneous (such as non-terminating)
code.

Patches:

-  4c019e1004 Proto/Michelson: fix error message for DIP with wrong
   constant
-  7d0211b648 Michelson: fix number_of_generated_growing_types
-  6a7bbf17a4 Proto: add max stack depth
-  040abed403 Proto: normalize stackoverflows in typechecking and
   unparsing
-  95c31f963c Michelson: restrict deep stack instructions to 1023
