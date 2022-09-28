Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Lima (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Tezos.

.. contents::

New Environment Version (V8)
----------------------------

This protocol requires a different protocol environment version than Lima.
It requires protocol environment V8, compared to V7 for Lima.

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

- Introduce local context access APIs to the indexed subcontext for optimized accesses with locality. (MR :gl:`!5922`)

- Optimized cleaning of implicit contract with 0 balance using local context accesses (MR :gl:`!5922`)
