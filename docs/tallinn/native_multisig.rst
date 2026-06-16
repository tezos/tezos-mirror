========================
Native Multisig accounts
========================

.. note::

  This page provides reference documentation for native multisig accounts, available starting with protocol S.
  For step-by-step instructions on how to stake and bake with multisig accounts, see `User stories for staking and baking multisigs <https://docs.google.com/document/d/15zZviTAFecXTHZXYg0MyQ6J6sKEvwPpywoTrzBCxnfE/edit?usp=drive_link>`__ .

:doc:`Tezos accounts <accounts>` are defined by a pair of secret and
public keys. Users use their secret keys to sign :doc:`operations and
blocks <blocks_ops>` (or any other message), and the resulting signature is validated
against the account's public key. While this model is simple and
effective, it presents two main challenges:

- The entire security of the account relies on a single secret key. If
  this key is compromised, an attacker can gain full control of the
  account.

- The model is not well-suited for organizations or shared accounts,
  as it does not allow for multiple users to manage the account
  collectively.

Both problems are usually solved by using a multisig **contract**, that allows to share its ownership and its associated balance or assets, between several participants. Tezos provides specific support for multisig contracts in the form of a :doc:`builtin multisig contract <../user/multisig>` and even a set of related client commands to interact with it. However, this solution is not applicable in all use cases, and most importantly in the case of collectively staking or baking. This is because in Tezos, a smart contract cannot be a :ref:`delegate <def_delegate_tallinn>`, and not even a :ref:`staker <def_staker_tallinn>`.

Native multisig **accounts** address the above limitations with a
cryptographic solution based on BLS multi-signature schemes. BLS
signatures are particularly well-suited for this purpose due to their
*aggregation* properties. Tezos supports BLS signatures
with :ref:`tz4 address accounts <tz4_accounts_tallinn>`,
and starting with protocol S, these adopt a
proof-of-possession (PoP) scheme. As a result, protocols starting with S
can benefit from faster verification of multiple signatures of the
same message without breaking compatibility with the existing support
for BLS (and ``tz4`` accounts).

The multisig accounts implementation offers :ref:`RPCs and client
commands <native_multisig_rpc_cli_tallinn>` to facilitate the signing of
operations in a native multisig setup. As a result, the protocol does
not need to differentiate between a ``tz4`` address belonging to a single
user account and one associated with a multisig account.


Enshrined multisig signing scenarios
------------------------------------

A (native) **multisig account** is a ``tz4`` address managed collectively by N participants (also called members).

Multisig accounts support two different multi-signature
schemes, which provide different scenarios based on the requirements
for signing an operation:

- **N-of-N scenario (signature aggregation)**: All N members must sign
  every operation for it to be valid. In this scenario, users derive a multisig
  account by aggregating the public keys of their existing ``tz4``
  addresses without the need for sharing secrets or trusted parties.

- **M-of-N scenario (threshold signature)**: A *threshold* number of
  participants is required to sign the operation. Here, M denotes the
  minimum number of signatures required to create a valid signature,
  out of the total of N members. In this scenario, users generate a new
  unencrypted secret for the multisig account, which is then
  split into secret shares that need to be distributed among the
  participants along with a public identifier (a natural number between 1 and N).
  This entails that all
  participants must trust the honesty of the party in charge of
  splitting the secret key and distributing the secrets.

The workflow for signing operations is as follows:

- The participants decide *a priori* which scheme to use: either
  N-of-N or M-of-N.

- The participants reveal the resulting public key of the
  multisig account on-chain.

- The participants inject operations signed with the multisig
  accounts’s signature scheme.

For example, a multisig account can stake 10,000 tez following
almost the same process as if it were carried out by a single
staker. The only difference is the signing process: signatures (for
each operation) must first be collected from the required members, and
then aggregated into a single signature based on the chosen
scenario. The resulting operation is identical to a staking operation
signed by a single implicit ``tz4`` account. For instance, the JSON output
of a multisig ``stake`` operation would look like this:

.. figure:: native-multisig-accounts.png

(the source and destination are both the address of the multisig account, because staking is implemented by a pseudo-operation consisting in sending a transaction to oneself).


.. _native_multisig_rpc_cli:
.. _native_multisig_rpc_cli_tallinn:

Octez CLI commands and RPC endpoints
------------------------------------

Multisig accounts introduce the following Octez client
commands and/or associated RPC endpoints to facilitate the creation
of multisig accounts and the aggregation of signatures.

Setting up a multisig account
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :widths: 10 45 45
   :header-rows: 1

   * -
     - N-of-N (signature aggregation)
     - M-of-N (threshold signature)
   * - Client command
     - ``octez-client aggregate bls public keys '[{"public_key":"<pk_1>","proof":"<proof_1>"}, ..., {"public_key":"<pk_N>","proof":"<proof_N>"}]'``
     - ``octez-client share bls secret key <sk> between <N> shares with threshold <M>``
   * - RPC
     - ``rpc post /bls/aggregate_public_keys with '[{"public_key":"<pk_1>","proof":"<proof_1>"}, ..., {"public_key":"<pk_N>","proof":"<proof_N>"}]'``
     - No RPC as it would require sending a secret key.
   * - Parameters
     - + Public keys ``pk_i`` of each participant with their proof-of-possession ``proof_i``
     - + Unencrypted secret key ``sk`` of the multisig account
       + total number of participants ``N``
       + threshold number ``M`` (minimum number of signatures required)
   * - Output
     - + Public key of the multisig account ``public_key`` and its hash ``public_key_hash``
     - + Public key of the multisig account ``public_key`` and its hash ``public_key_hash``
       + Proof of possession ``proof`` required for revealing a multisig account's public key ``public_key``
       + Secret shares ``secret_shares``: a unique identifier ``id`` in the range [1; ``N``] and a secret key ``secret_key`` for each participant of the multisig
   * - Example output
     - .. code-block:: json

         { "public_key": "BLpk...",
           "public_key_hash": "tz4..." }

     - .. code-block:: json

          { "public_key": "BLpk...",
            "public_key_hash": "tz4...",
            "proof": "BLsig...",
            "secret_shares":
              [ { "id": 1, "secret_key": "BLsk..." },
                { "id": 2, "secret_key": "BLsk..." },
                { "id": 3, "secret_key": "BLsk..." },
                // ...
              ] }

When aggregating public keys, the N-of-N scenario must also check a
proof-of-possession (PoP) to mitigate rogue key attacks. For creating a PoP,
the dedicated client command is:

.. list-table::
   :widths: 10 90
   :header-rows: 1

   * -
     - N-of-N (signature aggregation)
   * - ``proof_i``
     - ``octez-client create bls proof for <pk_i>``

**On sharing secrets.** The current implementation of M-of-N Scenario
relies on `Shamir’s Secret Sharing
<https://en.wikipedia.org/wiki/Shamir%27s_secret_sharing>`_ algorithm
to share an unencrypted master secret key between participants --
i.e. it returns a public key and unencrypted secret keys for each of
the multisig account participants. We envision implementing support
for further key-generation mechanisms in future, notably `Distributed
Key Generation
<https://en.wikipedia.org/wiki/Distributed_key_generation>`_, if there
is sufficient ecosystem interest.


Proof of possession
^^^^^^^^^^^^^^^^^^^

For revealing ``tz4`` accounts a proof of possession is required.

.. list-table::
   :widths: 10 45 45
   :header-rows: 1

   * -
     - N-of-N (signature aggregation)
     - M-of-N (threshold signature)
   * - Client command
     - ``octez-client aggregate bls proofs '{"public_key": "<pk_multisig>", "proofs": ["<proof_1_pk_multisig>", ..., "<proof_N_pk_multisig>"]}'``
     - ``proof`` is returned by the ``share bls secret key`` client command.
   * - RPC
     - ``rpc post /bls/aggregate_proofs with '{"public_key": "<pk_multisig>", "proofs": ["<proof_1_pk_multisig>", ..., "<proof_N_pk_multisig>"]}'``
     - No RPC as it would require sending a secret key.
   * - Parameters
     - + Public key of the multisig account ``public_key``
       + Proof of possession ``proof_i_pk_multisig`` of each multisig participant created for a multisig account's public key ``public_key``
     - *N/A*
   * - Output
     - Proof of possession ``proof`` required for revealing a multisig account's public key ``public_key``
     - *N/A*
   * - Example output
     - ``BLsig...``
     - *N/A*

For N-of-N Scenario, it requires to create ``proof_i_pk_multisig`` for
a multisig account's public key ``pk_multisig``:

.. list-table::
   :widths: 10 90
   :header-rows: 1

   * -
     - N-of-N (signature aggregation)
   * - ``proof_i_pk_multisig``
     - ``octez-client create bls proof for <pk_i> --override-public-key <pk_multisig>``


Signing operations with multisig accounts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :widths: 10 45 45
   :header-rows: 1

   * -
     - N-of-N (signature aggregation)
     - M-of-N (threshold signature)
   * - Client command
     - ``octez-client aggregate bls signatures '{"public_key": "<pk_multisig>", "message": "<msg>", "signature_shares": ["<signature_1>", ..., "<signature_N>"]}'``
     - ``octez-client threshold bls signatures '{"public_key": "<pk_multisig>", "message": "<msg>", "signature_shares": [{"id":1,"signature":"<signature_1>"}, {"id":5,"signature":"<signature_5>"},...]}'``
   * - RPC
     - ``rpc post /bls/aggregate_signatures with '{"public_key": "<pk_multisig>", "message": "<msg>", "signature_shares": ["<signature_1>", ..., "<signature_N>"]}'``
     - ``rpc post /bls/threshold_signatures with '{"public_key": "<pk_multisig>", "message": "<msg>", "signature_shares": [{"id":1,"signature":"<signature_1>"}, {"id":5,"signature":"<signature_5>"},...]}'``
   * - Parameters
     - + Public key of the multisig account ``public_key``
       + Message ``message``
       + Signatures ``signature_i`` for ``message`` from each multisig participant
     - + Public key of the multisig account ``public_key``
       + Message ``message``
       + Signatures ``signature_i`` from threshold number of multisig participants with their identifier.
       + Signature ``signature_i`` for ``message`` is produced by a multisig participant ``id_i``
   * - Output
     - Valid aggregated signature of a message ``message`` under a multisig account's public key ``public_key``
     - *idem*
   * - Example output
     - ``BLsig...``
     - *idem*

In both scenarios, multisig participants may use the existing ``sign bytes`` client
command to produce their own signature ``signature_i``::

  octez-client sign bytes '<unsigned_operation>' for <pk_i>

Signature shares can also be verified using the existing ``check
bytes`` client command::

  octez-client check that bytes '<unsigned_operation>' were signed by <pk_i> to produce <signature_i>
