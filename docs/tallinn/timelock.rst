Time-lock
=========

Time-lock is a cryptographic primitive that can be used as part of a commit & reveal scheme, to provide a guarantee that the information associated to the commit phase is eventually revealed.

For more background information on time-locks and their uses, see `Timelocks <https://docs.tezos.com/smart-contracts/data-types/crypto-data-types#timelocks>`__ in the Tezos documentation.

Cryptographic design
--------------------

The time-lock features are supported in Octez by the :package-api:`Tezos_crypto.Timelock library <octez-libs/Tezos_crypto/Timelock/index.html>`.

Users first generate a RSA modulus and a symmetric encryption key.
They use authenticated encryption to encrypt a packed Michelson value (an array of bytes computed with ``PACK``)
and encrypt that encryption key using a time-lock puzzle.
They then combine the RSA modulus, the time-locked symmetric key, the constant ``T``
and the encrypted value as a single value as well (called ``chest`` in our library).

A proof of decryption can be the symmetric key itself.
However, a malicious user could propose an authenticated ciphertext that does not yield a valid value
even when decrypted with the symmetric key that was indeed time locked.
To avoid this threat, an opening (called ``chest_key`` in our library) includes the symmetric key and
a proof that the symmetric key proposed is indeed the one hidden in the time-lock puzzle.
In this way one can differentiate whether the chest or the chest_key was proposed by a
malicious user.


Opcode and types
----------------

To expose the features of this library, the Michelson language provides the following types:

- ``chest``, which represents time-locked arbitrary bytes with the
  necessary public parameters to open it.
- ``chest_key``, which represents the decryption key,
  alongside with a proof that the key is correct.

and the following opcode:

``open_chest ::  chest_key → chest → time → bytes option``

``open_chest`` takes a ``chest`` and a ``chest_key``, and produces either the underlying plaintext
or indicates that the ``chest_key`` is malicious.
If the ``chest`` is not well-formed (which is possible since we use authenticated encryption),
the empty Byte is returned.


Implementation of the time-lock puzzle
--------------------------------------

The implementation of the time-lock puzzle
and proof scheme is located in :src:`src/lib_crypto/timelock.ml`.

To facilitate the use of time-locks,  commands have also been implemented in Octez client to generate a ``chest`` and ``chest_key`` as well as to open and verify them. An additional command ``precompute`` was implemented to fasten the time-lock ``chest`` generation.

For more information on the client commands, see :doc:`cli-commands<cli-commands>`.

Example
-------

The built-in :src:`coin flip contract <src/proto_024_PtTALLiN/lib_protocol/contracts/timelock_flip.tz>` gives an example of using time-lock. Beware this contract is for educational purpose only and is not secure.
