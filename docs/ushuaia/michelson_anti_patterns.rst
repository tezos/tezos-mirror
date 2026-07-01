Michelson Anti-Patterns
=======================

Even though Michelson is designed to make it easy to write secure
contracts and difficult to write vulnerable ones, it is still possible
to write buggy contracts that leak data and funds. This is a list of
mistakes that you can make when writing or interacting with contracts on
the Tezos blockchain and alternative ways to write code that avoid these
problems.

This list is not exhaustive and will never be. The following resources
partially complement it:

- https://opentezos.com/smart-contracts/avoiding-flaws/
- https://ligolang.org/docs/tutorials/security/
- https://github.com/InferenceAG/TezosSmartContractDetails
- https://medium.com/protofire-blog/recommendations-to-enhance-security-of-tezos-smart-contracts-d14c0e53a6d3

Storing unbounded data
----------------------

The gas costs for serializing and deserializing a contract storage are
proportional to its size. Contracts allowing arbitrary users to add
data, or allowing authenticated users to add data of unbounded size, are vulnerable to malicious users increasing the storage size to
make legitimate interactions with the contract consume a lot of gas or
even deadlocking the contract.

Possible issues:
~~~~~~~~~~~~~~~~

- Malicious users may increase the storage size by adding large chunks
  of data.

- Even if each account can only store a bounded amount of data, a
  malicious user may create many accounts to bypass the limit.

Alternatives/Solutions:
~~~~~~~~~~~~~~~~~~~~~~~

- Store unbounded data offchain. When some data is not required for
  the execution of a smart contract, the contract does not need to
  store it. Typically, in most cases there is no need to store the full
  contents of an NFT in a smart contract, but rather some metadata or pointer.
  Even when some data is genuinely required, it can often be
  replaced in storage by its hash and revealed by the user when
  it is required for the contract execution.

- Store user data in big maps. Since big map are lazy data structures,
  instead of deserializing the full content of a stored big map before
  the execution, deserialization happens during the execution and only
  for the accessed keys. The size of the part of a big map which is
  not accessed during the execution has no impact on the gas costs of
  the execution (with two exceptions: transferring or deleting a complete big map
  containing tickets leads to an update of the ticket table which is
  linear in the number of keys). By storing user data under a big map
  whose keys are linked to the user addresses having added each key, it is possible to
  guarantee that the gas costs for legitimate users is not impacted by
  the interactions of malicious users.

Refunding to a list of contracts
--------------------------------

One common pattern in contracts is to refund a group of people’s funds
at once. This is problematic if you accepted arbitrary contracts as a
malicious user can do cause various issues for you.

Possible issues:
~~~~~~~~~~~~~~~~

-  One contract swallows all the gas through a series of callbacks
-  One contract writes transactions until the block is full
-  Reentrancy bugs. Michelson intentionally makes these difficult to
   write, but it is still possible if you try.
-  A contract calls the \`FAIL\` instruction, stopping all computation.

Alternatives/Solutions:
~~~~~~~~~~~~~~~~~~~~~~~

-  Create a default account from people’s keys. Default accounts cannot
   execute code, avoiding the bugs above. Have people submit keys rather
   than contracts.
-  Have people pull their funds individually. Each user can break their
   own withdrawal only. **This does not protect against reentrancy
   bugs.**

Avoid batch operations when users can increase the size of the batch
--------------------------------------------------------------------

Contracts that rely on linear or super-linear operations are vulnerable
to malicious users supplying values until the contract cannot finish
without running into fuel limits. This can deadlock your contract.

Possible issues:
~~~~~~~~~~~~~~~~

-  Malicious users can force your contract into a pathological worst
   case, stopping it from finishing with available gas. Note that in the
   absence of hard gas limits, this can still be disabling as node
   operators may not want to run contracts that take more than a certain
   amount of gas.
-  You may hit the slow case of an amortized algorithm or data structure
   at an inopportune time, using up all of your contract’s available
   gas.

Alternatives/Solutions:
~~~~~~~~~~~~~~~~~~~~~~~

-  Avoid data structures and algorithms that rely on amortized
   operations, especially when users may add data.
-  Restrict the amount of data your contract can store to a level that
   will not overwhelm the available gas.
-  Write your contract so that it may pause and resume batch operations.
   This would complicate these sequences and require constant checking
   of available gas, but it prevents various attacks.

\*Do not assume an attack will be prohibitively expensive\*
Cryptocurrencies have extreme price fluctuations frequently and an
extremely motivated attacker may decide that an enormous expense is
justified. Remember, an attack that disables a contract is not just
targeted at the authors, but also the users of that contract.

Signatures alone do not prevent replay attacks
----------------------------------------------

If your contract uses signatures to authenticate messages, beware of
replay attacks. If a user ever signs a piece of data, you *must* make
sure that that piece of data is never again a valid message to the
contract. If you do not do this, anyone else can call your contract with
the same input and piggyback on the earlier approval.

Possible issues:
~~~~~~~~~~~~~~~~

-  A previously approved action can be replayed.

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Use an internal counter to make the data you ask users to sign
   unique. This counter should be per key so that users can find out
   what they need to approve. This should be paired with a signed hash
   of your contract to prevent cross-contract replays.
-  Use the ``SENDER`` instruction to verify that the expected sender is
   the source of the message.

Do not assume users will use a unique key for every smart contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users should always use a different key for every contract with which
they interact. If this is not the case, a message the user signed for
another contract can be sent to your contract. An internal counter alone
does not protect against this attack. It *must* be paired with a hash of
your contract. You must verify the source of the message.

Storing/transferring private data
---------------------------------

Once data is published to anyone, including broadcasting a transaction,
that data is public. Never transmit secret information via any part of
the blockchain ecosystem. As soon as you have broadcast a transaction
including that piece of information, anyone can see it. Furthermore,
malicious nodes in the system can manipulate unsigned transactions by
delaying, modifying, or reordering them.

Possible Issues
~~~~~~~~~~~~~~~

-  If data is not signed, it can be modified
-  Transactions can be delayed
-  Secret information will become public

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Do not store private information on the blockchain or broadcast it in
   transactions.
-  Sign all transactions that contain information that, if manipulated,
   could be abused.
-  Use counters to enforce transaction orders.

This will at least create a logical clock on messages sent to your
contract.

Not setting all state before a transfer
---------------------------------------

Reentrancy is a potential issue on the blockchain. When a contract makes
a transfer to another contract, that contract can execute its own code,
and can make arbitrary further transfers, including back to the original
contract. If state has not been updated before the transfer is made, a
contract can call back in and execute actions based on old state.

Possible Issues
~~~~~~~~~~~~~~~

-  Multiple withdrawals/actions
-  Generating illegal state if state is updated twice later

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Forbid reentrancy by means of a flag in your storage, unless you have
   a good reason to allow users to reenter your contract, this is likely
   the best option.
-  Only make transfers to trusted contracts or default accounts. Default
   accounts cannot execute code, so it is always safe to transfer to
   them. Before trusting a contract, make sure that its behavior cannot
   be modified and that you have an extremely high degree of confidence
   in it.
