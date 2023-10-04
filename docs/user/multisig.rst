Built-in multi-sig contracts
============================

A multi-signed account, or *multisig* for short, is a way to share the
ownership of an address (and of the associated balance) between
several participants.

To act on a multisig, a fraction of the participants must agree on the
action by signing it with their private keys. The minimal number of
participants that need to agree for the action to be approved is
called the multisig *threshold*.

On Tezos, a way to run a multisig is by using a smart contract. Such a
multisig contract has built-in support in the ``octez-client`` and has
been formally verified using the `Mi-Cho-Coq <https://gitlab.com/nomadic-labs/mi-cho-coq/>`_ framework.

Interacting with a multisig contract using ``octez-client``
-----------------------------------------------------------

The recommended way to create and use a multisig contract is via
the ``octez-client`` built-in commands for the multisig contract. The command
``octez-client man multisig`` gives a complete list of
multisig-related commands with details about the syntax of each
command.

Originating a new multisig contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To originate a new generic multisig contract, use the ``octez-client
deploy multisig`` command. It is similar to ``octez-client originate
contract`` with the following differences:

- no script is given because the script of the generic multisig
  contract is fixed
- instead of giving the initial storage with the ``--init`` option,
  the threshold and the public keys of the participants are given on
  the command line.

For example, the following commands can be used to generate three pairs
of keys named ``alice``, ``bob``, and ``charlie`` and originate a multisig
contract named ``msig`` that can be actioned by any two of them; the
initial balance of this contract and of each of the signers is ꜩ100 generously offered by the
first bootstrap account:

::

   $ octez-client gen keys alice
   $ octez-client gen keys bob
   $ octez-client gen keys charlie
   $ octez-client transfer 100 from bootstrap1 to alice --burn-cap 1
   $ octez-client transfer 100 from bootstrap1 to bob --burn-cap 1
   $ octez-client transfer 100 from bootstrap1 to charlie --burn-cap 1
   $ octez-client deploy multisig msig transferring 100 from bootstrap1 with threshold 2 on public keys alice bob charlie --burn-cap 1


Preparing a transaction
~~~~~~~~~~~~~~~~~~~~~~~

The ``octez-client prepare multisig transaction`` commands are used to
obtain the byte sequence corresponding to a possible action and that
needs to be signed.

To avoid writing Michelson lambdas, special cases for a single
transfer or delegate change have their own commands.

By default the ``octez-client prepare multisig transaction`` commands
display not only the byte sequence to sign but also a cryptographic
hash (this can be useful when signing with a hardware signer), the
threshold and the participant public keys. To obtain the byte sequence
only, these commands accept a ``--bytes-only`` option.
For example, if Alice and Charlie want to send ꜩ10 from the
multisig to Bob they will need to sign a transaction. They can call

::

   $ octez-client prepare multisig transaction on msig transferring 10 to bob

This command will give them the byte sequence they need to sign, its
cryptographic hash, the threshold (which is 2 in this case), and the
public keys of Alice, Bob, and Charlie.

Signing an action
~~~~~~~~~~~~~~~~~

There are two equivalent ways to sign an action with ``octez-client``:

- preparing the action with one of the ``octez-client prepare multisig
  transaction`` commands and then signing it using the ``octez-client
  sign bytes`` command,
- or directly using one of the ``octez-client sign multisig
  transaction`` commands that combine these two steps.

For example, Alice can sign the transfer to Bob using

::

   $ TO_SIGN=$(octez-client prepare multisig transaction on msig transferring 10 to bob --bytes-only)
   $ ALICE_S_SIGNATURE=$(octez-client sign bytes "$TO_SIGN" for alice | cut -d ' ' -f 2)

and Charlie can sign the same transfer using

::

   $ CHARLIE_S_SIGNATURE=$(octez-client sign multisig transaction on msig transferring 10 to bob using secret key charlie)

Acting on the multisig contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once a user has gathered enough signatures to act on the multisig
contract, there are again two equivalent ways of sending the
signatures:

- preparing the action with one of the ``octez-client prepare multisig
  transaction`` commands and then using the produced byte sequence
  in the ``octez-client run transaction`` command,
- or directly using one of the following commands depending on the action:


  - ``octez-client from multisig contract <multisig> transfer``
  - ``octez-client from multisig contract <multisig> run lambda``
  - ``octez-client set delegate of multisig contract``
  - ``octez-client withdraw delegate of multisig contract``
  - ``octez-client set threshold of multisig contract``


For example, if Alice sends her signature to Charlie, he can perform
the multi-signed transfer of ꜩ10 to Bob using either:

::

   $ octez-client run transaction "$TO_SIGN" on multisig contract msig on behalf of charlie with signatures "$ALICE_S_SIGNATURE" "$CHARLIE_S_SIGNATURE"

or

::

   $ octez-client from multisig contract msig transfer 10 to bob on behalf of charlie with signatures "$ALICE_S_SIGNATURE" "$CHARLIE_S_SIGNATURE"


Supported versions of the multisig contract
-------------------------------------------

Two main versions of the multisig contract are supported by
``octez-client``. They are called the generic multisig contract and
the legacy multisig contract.

The generic multisig contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :src:`generic multisig contract<michelson_test_scripts/mini_scenarios/generic_multisig.tz>` is
the multisig contract that is currently recommended. It has the
following features:

- it can receive tokens from unauthenticated sources on its default
  entrypoint of type ``unit``
- the possible authenticated actions on the contract are:


  - atomically execute an arbitrary list of operations (of type
    ``lambda unit (list operation)`` in Michelson)
  - update the contract storage to change both the threshold and the
    participant public keys

The legacy multisig contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``octez-client`` also supports
:src:`a legacy version<michelson_test_scripts/mini_scenarios/legacy_multisig.tz>` of the multisig contract which has the following
limitations:

- it cannot receive tokens from unauthenticated sources, sending
  tokens to the contract is only possible as a side effect of an
  authenticated action
- the possible authenticated actions on the contract are:


  - transfer without parameter to an implicit account or to a smart
    contract with an entrypoint of type ``unit``
  - set the delegate of the contract
  - remove the delegate of the contract
  - update the contract storage to change both the threshold and the
    participant public keys

In particular, the legacy multisig contract does not support executing
several operations atomically, calling smart contracts with
parameters, and originating new contracts. In contrast, all the
features of the legacy multisig contract are supported by the generic
multisig contract.

Listing supported hashes
~~~~~~~~~~~~~~~~~~~~~~~~

For security reasons, ``octez-client`` will not interact with unknown
scripts even if their interface matches one of the supported
multisig contracts. To check if a script is one of the supported ones,
it stores a list of script hashes that can be printed by
``octez-client show supported multisig hashes``. The script originated
by the ``octez-client deploy multisig`` command is always one of the
supported multisig contracts.

Interacting with a multisig contract directly
---------------------------------------------

The following subsections describe in detail the low-level API of a
built-in multisig contract, allowing one to originate and use in
situations where ``octez-client`` cannot be used e.g., when
interacting with the chain from a web browser or in a mobile
application. In particular, this interface is typically useful when
developing multisig support in another Tezos wallet.

Anti-replay protection
~~~~~~~~~~~~~~~~~~~~~~

A replay attack consists in authenticating as someone else by reusing
a signature emitted in a different context. Examples of replay attacks
include reusing a signature sent in a previous transaction, to another
multisig contract, or to the same contract on another chain.

To protect against replay attack, signed data of a multisig contract
needs to contain not only the action to perform but also:

- the address of the multisig contract to avoid replaying signatures
  meant for another multisig contract,
- the chain identifier of the current chain to avoid replaying
  signatures between the test chain forked during the testing period
  of :doc:`the voting procedure <../active/voting>` and the main chain,
- an always-increasing anti-replay counter to avoid replaying past
  transactions on the same multisig contract.

The anti-replay counter is stored in the multisig contract storage and
incremented at each successful call of the multisig contract.

Multisig contract storage
~~~~~~~~~~~~~~~~~~~~~~~~~

Both the generic and the legacy multisig contracts have a storage of
type ``(pair (nat %stored_counter) (pair (nat %threshold) (list %keys
key)))`` so the storage of the multisig contract is of the form ``Pair
<stored_counter> (Pair <threshold> { <first_public_key>;
<second_public_key>; ...; <last_public_key> })`` where
``<stored_counter>`` and ``<threshold>`` are Micheline integers
representing respectively the anti-replay counter and the threshold
and each public key is either a Micheline byte sequence or a Micheline
string depending on the mode used to unparse the storage.

Multisig contract actions
~~~~~~~~~~~~~~~~~~~~~~~~~

The type of actions for the generic multisig is ``(or :action (lambda
%operation unit (list operation)) (pair %change_keys (nat %threshold)
(list %keys key)))`` so a valid action is either of the form ``Left
{<code>}`` where ``code`` is of type ``lambda unit (list operation)``
for executing the given lambda and sending the produced operations or
``Right (Pair <new_threshold> {<new_first_public_key>; ...;
<new_last_public_key>})`` for changing the threshold and participant
public keys.

The type of actions for the legacy multisig is ``(or :action (pair
:transfer (mutez %amount) (contract %dest unit)) (or (option %delegate
key_hash) (pair %change_keys (nat %threshold) (list %keys key))))`` so
a valid action is either of the form ``Left (Pair <amount>
<destination>)`` for a transfer, ``Right (Left None)`` for withdrawing
the delegate, ``Right (Left (Some <new_delegate>))`` for changing the
delegate, or ``Right (Right (Pair <new_threshold>
{<new_first_public_key>; ...; <new_last_public_key>}))`` for changing
the threshold and participant public keys.

Multisig contract sign data
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The data to sign for a given action is the binary serialisation (using
the ``PACK`` Michelson instruction) of an expression of type ``pair
(pair chain_id address) (pair :payload (nat %counter) <action>)``
where the ``<chain_id>`` is the chain id of the current chain as
returned by the ``CHAIN_ID`` instruction, the address is the one of
the multisig contract as returned by ``SELF; ADDRESS``, the ``nat``
counter must match exactly the stored counter.

Multisig contract parameter
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The generic contract has two entrypoints:

- ``default`` of type ``unit`` used to receive tokens from
  unauthenticated sources
- ``main`` of type ``pair (pair :payload (nat %counter) <action>)
  (list %sigs (option signature))`` used to perform a multi-signed
  action.

The legacy contract has only one entrypoint that is unnamed and whose type corresponds to the second above.

The ``nat`` counter must exactly match the stored counter and the list
of optional signatures must be of the same length and given in the
same order as the stored public keys; ``None`` can be used to skip a
signature, the number of provided signatures must be greater or equal
to the stored threshold.

Formal verification
-------------------

See
`here <https://gitlab.com/nomadic-labs/mi-cho-coq/-/blob/master/src/contracts_coq/generic_multisig.v>`_
for a formal specification and a correctness proof of the generic
multisig script written in Coq using the Mi-Cho-Coq framework.
