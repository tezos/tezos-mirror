Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

Smart Rollups
-------------

- The ``smart_rollup_originate`` operation now also takes an optional
  whitelist of public key hashes. This whitelist cannot be used yet
  (the ``sc_rollup.private_enable`` flag has to be set to true). (MR :gl:`!9401`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Issuance (ongoing)
----------------------------

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- Register an error's encoding: ``WASM_proof_verification_failed``. It was
  previously not registered, making the error message a bit obscure. (MR :gl:`!9603`)

