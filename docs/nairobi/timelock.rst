Time-lock
=========

Time-lock is a cryptographic primitive that can be used as part of a commit & reveal scheme, to provide a guarantee that the information associated to the commit phase is eventually revealed.

For more background information on time-locks and their uses, see :doc:`../global/timelock`.

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

``open_chest ::  chest_key → chest → time → or (bytes, bool)``

``open_chest`` takes a ``chest`` and ``chest_key``, and produces either the underlying plaintext
or indicates that the ``chest`` or the ``chest_key`` is malicious.

If we open the chest with a correctly generated chest key, the instruction pushes
``Left bytes`` on top of the stack where the bytes are
cryptographically guaranteed to be the bytes the chest provider time-locked.
If the ciphertext does not decrypt under the symmetric key that was time-locked, it pushes on the stack
``Right False``.
If the provided symmetric key was not the one time-locked
(detectable due to the time-lock proof),
it pushes on the stack ``Right True``.
Note that the implementation uses an authenticated encryption scheme,
so one can detect if someone provides a wrong key while fooling the time-lock proof.
This is doable only by someone knowing the factorization of the RSA modulus.
However, this cannot prevent someone from encrypting a wrong key, or putting
a wrong message authentication code, so this is why a proof of correctness is still needed.


Implementation of the time-lock puzzle
--------------------------------------

The implementation of the time-lock puzzle
and proof scheme is located in :src:`src/lib_crypto/timelock.ml`. It is inspired by
the proof-of-concept shown
`here <https://gist.github.com/murbard/23a29454a107d03d8a98393b0b98466d>`__.

The utility developed by `Completium <https://completium.com>`_ available `here <https://github.com/completium/timelock-utils>`__,
allows a user to create chests and chest keys to interact with a smart contract.


Example
-------

The built-in :src:`coin flip contract <src/proto_alpha/lib_protocol/contracts/timelock_flip.tz>` gives an example of using time-lock. Beware this contract is for educational purpose only and is not secure.
