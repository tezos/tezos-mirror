Accounts and addresses
======================

The Tezos ledger currently supports two types of accounts that can hold
tokens (and be the destinations of transactions), identified by distinct
addresses:

- A user account (or implicit account) is a non-programmable account, whose tokens
  are spendable and delegatable by the owner of a private key. Its address is
  the hash of the public key, prefixed by ``tz1``, ``tz2``,
  ``tz3`` or ``tz4``.
- A smart contract is a programmable account, associated with some Michelson code and a piece of data storage.
  The address of a smart contract is a unique hash that depends on
  the operation that led to its creation, called origination, prefixed by ``KT1``.
  This is why smart contracts are also called originated accounts.

Finally, addresses prefixed with ``sr1`` identify :doc:`Smart Rollups
<./smart_rollups>`.

.. _user_accounts_qena:

User accounts
~~~~~~~~~~~~~

From the economic protocol's point of view, user accounts are considered as a particular case
of smart contracts that always succeed in receiving tokens or tickets,
and do nothing else.
Transferring toward a user account succeeds even if the address of the account has never appeared on the chain before.
This is why user accounts are also called implicit accounts.

Transactions that are signed by the private key corresponding to the public key
hash, *i.e.* address of the account can spend its tokens. Each
prefix for addresses denotes a different cryptographic signing scheme. They are
briefly described below from a user point of view.

The sizes of public keys, secret keys and signatures may differ between the
different schemes but addresses are always 20 bytes long.

``tz1``: Ed25519
''''''''''''''''

Addresses that start with the ``tz1`` prefix are hashes of Ed25519 public keys
and signatures must be produced by using the `EdDSA signature scheme
<https://datatracker.ietf.org/doc/html/rfc8032>`_ with the `Curve25519 curve
<https://ed25519.cr.yp.to>`_. This is the default scheme of Octez when, *e.g.*,
generating key pairs. It is also the recommended cryptographic scheme to use
because it offers better security guarantees than EcDSA and has good performance
on most hardware. It may not be available in all wallets or on all dedicated
chips which is why Tezos supports multiple schemes.

``tz2``: Secp256k1
''''''''''''''''''

Addresses that start with the ``tz2`` prefix are hashes of Secp256k1 public keys
and signatures must be produced by using the `EcDSA signature scheme
<https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm>`_
with the `Secp256k1 curve <https://www.secg.org/sec2-v2.pdf>`_. Secp256k1 is
notably the cryptographic scheme used by Bitcoin and Ethereum. This means that
private keys and addresses used on Bitcoin can also be used on Tezos.

``tz3``: P-256
''''''''''''''

Addresses that start with the ``tz3`` prefix are hashes of P-256 public keys and
signatures must be produced by using the `EcDSA signature scheme
<https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm>`_
with the `P-256 curve
<https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-186.pdf>`_, also known as
Secp256r1. This is one of the curves for EcDSA recommended by NIST. It is also
often the only cryptographic scheme supported by HSMs (Hardware Security
Modules) of cloud providers.

``tz4``: BLS
''''''''''''

Addresses that start with the ``tz4`` prefix are hashes of BLS public keys and
signatures must be produced by using the `BLS signature scheme
<https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-05>`_ with
the `BLS12-381 curve <https://hackmd.io/@benjaminion/bls12-381>`_. One
particularity of BLS signatures is that they are aggregatable. This means that
multiple signatures can be aggregated into one, and later verified as having
been produced for the correct expected public keys. This allows for numerous
applications like mutli-signatures schemes, multi-party key exchanges,
signatures compaction, etc. BLS is notably used by Zcash and Ethereum 2.0.

Smart contracts
~~~~~~~~~~~~~~~

A transaction to a smart contract
address can provide data and optionally some tokens, and triggers the execution of the code, which may read and update the storage. The transaction can succeed or fail, according to the :ref:`transaction semantics <transaction_semantics_qena>`.
