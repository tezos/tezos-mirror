On-chain Views
==============

Views are a mechanism that allows smart contracts to call smart contracts, and that:

- are read-only: they may depend on the storage of the contract
  declaring the view but cannot modify it nor emit operations (but they
  can call other views),
- take arguments as input in addition to the contract storage,
- return results as output,
- are synchronous: the result is immediately available on the stack of
  the caller contract.

Homonyms
--------

Beware that the term *view* has also been used for other kinds of mechanisms to access some data
from the blockchain.

-  `Off-chain views <https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#semantics-of-off-chain-views>`__ cannot be called from smart contracts but only by off-chain tools such as wallets.
-  `Legacy on-chain views <https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-5/tzip-5.md#view-entrypoints>`__ are a conventional way of making some
   contract data available. Their purpose is subsumed by that of the on-chain
   views defined here, but they are still referenced in FA1.2 and FA2 standards which predate the introduction of native on-chain views in the Hangzhou amendment.

In the following, the term "view" denotes the modern notion of on-chain views, described in this page.

Execution flow
--------------

The execution of a view is included in the operation of the
caller’s contract, but accesses the storage of the declarer’s contract, in
read-only mode. Thus, in terms of execution, views are more like lambda
functions rather than contract entrypoints, Here is an example:

::

  code {
    ...;
    TRANSFER_TOKENS;
    ...;
    VIEW "view_ex" unit;
    ...;
  }

This contract calls a contract ``TRANSFER_TOKENS``, and, later on, a view
called ``view_ex``. No matter if the callee ``view_ex`` is defined in the
same contract with this caller contract or not, this view will be executed
immediately in the current operation, while the operations emitted by
``TRANSFER_TOKENS`` will be executed later on. As a result, although it
may seem that ``view_ex`` receives the storage modified by
``TRANSFER_TOKENS``, this is not the case. In other words, the storage of
the view is the same as when the execution of the contract calling the view started. In
particular, in case of re-entrance, i.e., if a contract ``A`` calls a
contract ``B`` that calls a view on ``A``, the storage of the view will be
the same as when ``B`` started, not when ``A`` started.

Declaration
-----------

Views are **declared** at the toplevel of the script of the contract on
which they operate, alongside the contract parameter type, storage type,
and code. To declare a view, the ``view`` keyword is used; its syntax is
``view name 'arg 'return { instr; ... }`` where:

- ``name`` is a string of at most 31 characters matching the regular
  expression ``[a-zA-Z0-9_.%@]*``; it is used to identify the view,
  hence it must be different from the names of the other views declared
  in the same script;
- ``'arg`` is the type of the argument of the view;
- ``'return`` is the type of the result returned by the view;
- ``{ instr; ... }`` is a sequence of instructions of type ``lambda
  (pair 'arg 'storage_ty) 'return`` where ``'storage_ty`` is the type of
  the storage of the current contract. Certain specific instructions
  have different semantics in views: ``BALANCE`` represents the current
  amount of mutez held by the contract where the view is declared; ``SENDER``
  represents the contract which is calling the view; ``SELF_ADDRESS``
  represents the contract declaring the view; ``AMOUNT`` is always 0 mutez.

Forbidden types
---------------

Note that in both view input (type ``'arg``) and view output (type
``'return``), the following types are forbidden: ``ticket``,
``operation``, ``big_map`` and ``sapling_state``.
