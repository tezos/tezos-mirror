Protocol Ithaca
===============

**Important**: revision ``PsiThaCaT...jVP`` of protocol Ithaca contains
`bugs <https://research-development.nomadic-labs.com/announcing-ithaca-2.html>`_
that have been corrected in the latest version ``Psithaca2...z6A``.

This page contains all the relevant information for protocol Ithaca
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_012_Psithaca` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Ithaca with respect
to Hangzhou.

.. contents::

New Environment Version (V4)
----------------------------

This protocol requires a different protocol environment than Hangzhou.
It requires protocol environment V4, compared to V3 for Hangzhou.
(MRs :gl:`!3379`, :gl:`!3468`)

- Move BLS12-381 to blst backend. (MR :gl:`!3296`)

- Expose BLS signature module. (MR :gl:`!3470`)

- Remove Error-monad compat layer. (MR :gl:`!3575`)

- Allow different type of error categories in the different error monads.
  (MR :gl:`!3664`)

- Fix interface of Hex. (MR :gl:`!3267`)

- Fix use of Micheline canonical encoding. (MR :gl:`!3764`)

- Add concat_map. (MR :gl:`!3801`)

- Make Micheline.canonical_location abstract. (MR :gl:`!3744`)

- Error_monad: better tracing primitives. (MR :gl:`!3589`)

- Error-monad: simpler trace_eval. (MR :gl:`!3869`)

- Add missing functions from Lwtreslib. (MR :gl:`!3905`)

- Expose an order for folding over the context. (MR :gl:`!3910`)

- List: fix to_seq signature. (MR :gl:`!3961`)

Tenderbake
----------

- Tenderbake is a new consensus algorithm replacing Emmy* in order to provide
  deterministic finality. (MRs :gl:`!3738`, :gl:`!3822`, :gl:`!3832`,
  :gl:`!3811`, :gl:`!3850`, :gl:`!3906`, :gl:`!3977`, :gl:`!3987`, :gl:`!4033`,
  :gl:`!4036`)

- The list of breaking changes related to Tenderbake are described in a separate :doc:`change log<tenderbake>`.

Precheck of operations
----------------------

- Expose ``precheck_manager`` and ``check_manager_signature`` (MR :gl:`!3872`)

- Remove the gas block limit for prevalidator mode. (MR :gl:`!3802`)


Michelson
---------

- A new ``SUB_MUTEZ`` instruction has been added, it is similar to the
  ``mutez`` case of the ``SUB`` instruction but its return type is
  ``option mutez`` instead of ``mutez``. This allows subtracting
  ``mutez`` values without failing in case of underflow. (MR :gl:`!3079`)

- The ``SUB`` instruction on type ``mutez`` is deprecated. It can be
  replaced by ``SUB_MUTEZ; ASSERT_SOME`` (and ``SUB; DROP`` can be
  replaced by ``ASSERT_CMPGE``). (MR :gl:`!3079`)

- The ``MAP`` instruction can now also be applied to values of type ``option
  a``. In this case the block of code given is executed if and only if the value
  at the top of the stack is ``Some a``. It should map the value at the top of
  the stack into a value of any type ``b``. The block has access to the
  remainder of the stack also, but its type should remain unchanged. The result
  of the instruction is the stack returned by the applied block of code, where
  the value at the top is wrapped in ``Some`` again. If the value at the top of
  input stack is ``None``, the instruction does nothing. (MR :gl:`!3574`)

Tickets Hardening (ongoing)
---------------------------

- Add ticket-balance storage module. (MR :gl:`!3495`)

- Add API for scanning values for tickets. (MR :gl:`!3591`)

- Add API for generating ticket-balance key hashes. (MR :gl:`!3788`)

Bug Fixes
---------

- Use Cache_costs.cache_find in cache find. (MR :gl:`!3752`)

- Fix gas accounting for the deserialization of Michelson arguments in
  operations. (MR :gl:`!3930`)

- Do not count type annotation size. (MR :gl:`!4075`)

Minor Changes
-------------

- Update and simplify fixed constants. (MR :gl:`!3454`)

- Simplify pack cost. (MR :gl:`!3620`)

- Do not play with locations inside protocol. (MR :gl:`!3667`)

- Remove the optional entrypoint in ticketer address. (MR :gl:`!3570`)

- Make delegate optional for bootstrap contracts. (MR :gl:`!3584`)

- Fix interface of Hex. (MR :gl:`!3267`)

- Update migration for protocol "I". (MR :gl:`!3668`)

- Make ``max_operations_ttl`` a parametric constant of the protocol, now called
  ``max_operations_time_to_live``. (MR :gl:`!3709`)

- ``NOW`` and ``LEVEL`` are now passed to the Michelson interpreter as
  step constants instead of being read from the context each time
  these instructions are executed. (MR :gl:`!3524`)

- The RPC ``../helpers/current_level`` does not support anymore a
  negative ``offset`` argument. The level which used to be returned by
  ``../<block>/helpers/current_level?offset=-<n>`` can still be obtained by
  ``../<block~n>/helpers/current_level``. (MR :gl:`!3808`)

- Ensure annotations are non-empty. (MR :gl:`!3746`)

- Only allow positive depth in context query RPC and other RPC.
  (MR :gl:`!3564`)

- Liquidity Baking: postpone the sunset level by 819,200 blocks and
  decrease the escape hatch threshold from one half to one third.
  (MR :gl:`!3911`)

- Bump up bls12-381.1.1.0. (MRs :gl:`!3914`, :gl:`!3942`)

- Refactor empty transactions. (MR :gl:`!3867`)

- Fix storage error during transfer. (MR :gl:`!3963`)

- Update gas costs for the new version of bls12-381, and other minor gas changes. (MR :gl:`!3955`)

- In the ``michelson_v1.runtime_error`` error, which appears in the
  error trace of operations failing because of runtime errors (such as
  interpreting the ``FAILWITH`` instruction) during the execution of a
  smart contract, the ``contract_code`` field is deprecated. The
  failed script can still be fetched from the address returned in the
  ``contract_handle`` field. (MR :gl:`!4223`)

- Other internal refactorings or documentation. (MRs :gl:`!3506`, :gl:`!3550`,
  :gl:`!3593`, :gl:`!3552`, :gl:`!3588`, :gl:`!3612`, :gl:`!3575`,
  :gl:`!3622`, :gl:`!3631`, :gl:`!3630`, :gl:`!3707`, :gl:`!3644`,
  :gl:`!3529`, :gl:`!3739`, :gl:`!3741`, :gl:`!3695`, :gl:`!3763`,
  :gl:`!3779`, :gl:`!3745`, :gl:`!3256`, :gl:`!3326`, :gl:`!3812`,
  :gl:`!3920`, :gl:`!3929`)

- Add ``/chains/main/blocks/<block>/context/selected_snapshot?cycle=<cycle>``
  RPC to retrieve the snapshot index used to compute baking right for
  the given block's cycle or at the explicit given 'cycle' optional argument.
  Context entries located in
  ``/chains/main/blocks/<block>/context/raw/bytes/cycle/<cycle>/roll_snapshot``
  are no longer accessible after Tenderbake.
  As observed in issue :gl:`#2764`, the RPC is buggy for cycle ``474``: the correct result for that cycle is index 16 (not 4).
