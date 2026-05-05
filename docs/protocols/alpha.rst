Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to U025 (see :ref:`naming_convention`).

For changes brought by Quebec with respect to Paris, see :doc:`../protocols/021_quebec`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

Environment Version
-------------------



Smart Rollups
-------------

- The protocol can now designate a canonical rollup per chain, which
  can send signals to activate rollup-specific features without
  requiring a protocol amendment. The first supported signal,
  ``WASM_PVM_ENABLE_NEW_DURABLE_STORAGE``, enables the WASM PVM to use
  a new backend for its durable storage. (MR :gl:`!21634`)

Data Availability Layer
-----------------------

Adaptive Issuance
-----------------


Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------


Errors
------


Protocol parameters
-------------------

- Added ``smart_rollup_canonical_rollup_address``, identifying the
  canonical rollup for the current chain. (MR :gl:`!21634`)

- Removed ``allow_tz4_delegate_enable`` and ``aggregate_attestation`` from the
  parametric constants. Both feature flags had been ``true`` since their
  introduction (in protocols 022 and 023 respectively) and the conditional
  logic they gated was dead code. (MR :gl:`!21007`)

Bug Fixes
---------

Minor Changes
-------------

Internal
--------


