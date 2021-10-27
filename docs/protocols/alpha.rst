Protocol Alpha
==============

This page contains all the relevant information for protocol Alpha
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_alpha` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Alpha with respect
to Hangzhou.

.. contents::

New Environment Version (V4)
----------------------------

This protocol requires a different protocol environment than Hangzhou.
It requires protocol environment V4, compared to V3 for Hangzhou.
(MR :gl:`!3468`)

Bug Fixes
---------

- Fix `a bug <https://marigold.dev/blog/communicating-about-view-issue-hangzhou/>`_ in the semantics of views, where step constants
  ``BALANCE``, ``AMOUNT``, ``SENDER``, ``SELF``, and ``SELF_ADDRESS`` would not
  be restored after returning from a view (MR :gl:`!3654`)

Minor Changes
-------------
