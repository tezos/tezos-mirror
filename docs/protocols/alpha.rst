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

Tickets Strengthening
---------------------

- Add ticket-balance storage module. (MR :gl:`!3495`)

Bug Fixes
---------

- Fix `a bug <https://marigold.dev/blog/communicating-about-view-issue-hangzhou/>`_ in the semantics of views, where step constants
  ``BALANCE``, ``AMOUNT``, ``SENDER``, ``SELF``, and ``SELF_ADDRESS`` would not
  be restored after returning from a view (MR :gl:`!3654`)

Minor Changes
-------------

- Update and simplify fixed constants. (MR :gl:`!3454`)

- Simplify pack cost. (MR :gl:`!3620`)

- Do not play with locations inside protocol. (MR :gl:`!3667`)

- Remove the optional entrypoint in ticketer address. (MR :gl:`!3570`)

- Other internal refactorings or documentation. (MRs :gl:`!3506`, :gl:`!3550`,
  :gl:`!3593`, :gl:`!3552`, :gl:`!3588`, :gl:`!3612`, :gl:`!3575`,
  :gl:`!3622`, :gl:`!3631`, :gl:`!3630`)
