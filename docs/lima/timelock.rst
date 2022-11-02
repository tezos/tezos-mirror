Time-lock
=========


Background
----------

The issue of "generalized front-running", is a common attack on certain blockchain transactions.
Since a transaction can be observed before it is actually included in the chain, it
can give an advantage to one user (generally a trader) against another. More specifically,
it means block producers can extract "rent" from the system
as they have the ability to choose and order transactions within a block.

This issue is sometimes referred to, in proof-of-work networks like Ethereum, as
`Miner Extractable Value <https://arxiv.org/pdf/1904.05234.pdf>`_ or MEV for short.
It is described in more detail
`here <https://medium.com/degate/an-analysis-of-ethereum-front-running-and-its-defense-solutions-34ef81ba8456>`__.
We refer to it as BPEV, for "Block Producer Extractable Value".
Note that the term "front-running" is misleading as it implies there is a
fiduciary relationship between block producers and transaction emitters where,
in fact, none exists unless explicitly contracted into.

For example, upon receiving a transaction, a baker could craft a block including
this transaction and one of their own such that the sequential execution of these
two transactions guarantees a gain to the baker.

Preventing BPEV with time-lock
------------------------------

BPEV can be prevented with the use of time-lock encryption
(see `Time-lock puzzles and timed release Crypto <http://www.hashcash.org/papers/time-lock.pdf>`_
for more details).

Time-lock encryption allows for encrypting a message so it can be
decrypted in two ways.
Either the author of the ciphertext produces a plaintext
(and a proof of correct decryption)
by providing a secret trapdoor (the factorization of an RSA modulus in our case).
Or, a sequential computation can decrypt the ciphertext after a computation
requiring ``T`` sequential operations (modular squaring in our case),
for some pre-determined constant ``T``.

In addition, a proof of the correctness of the decryption can also be produced and checked in sub linear time (``log T`` in our case).

By experimentally measuring the time the sequential operation takes
on available hardware using optimized implementation, one can estimate
a rough conversion (or a bound in our case) between the constant ``T`` and
wall clock time.
Note that the `VDF alliance <https://www.vdfalliance.org/>`_ has been working on producing an ASIC for squaring in an RSA group to
ensure a level playing field in terms of computational speed.


General principles and usage
-----------------------------

The typical usage pattern would be as follows:

1. In a first period, a contract collects user-submitted and time-lock encrypted Michelson values along with some valuable deposit, such as tez.
2. In a second period, after the values are collected, the contract collects from users a decryption of the value they submitted alongside with a proof that the decryption is correct.
3. In a third period, if any value isn't decrypted, anyone can claim part of the deposit by submitting a decryption of the value, with the other part of the deposit being burnt. Different penalties can be assessed depending on whether the user merely failed to submit a decryption for their value, or if they also intentionally encrypted invalid data. Different rewards can be distributed for submitting a correct decryption. The third period needs to be long enough so that people have enough time to perform the time-lock decryption.
4. Finally, the contract can compute some function of all the decrypted data.

There is generally no incentive for users not to provide
the decryption of their data and thus the third period generally does not need
to take place. However, the second period needs to be long enough so that bakers
cannot easily censor submission of the decryption in a bid to later claim the reward.
Burning a part of the deposit also limits grieving attacks where a user gets back
their whole deposit by providing the decryption, but in a way that delays everyone else.

Cryptographic design
--------------------

The time-lock features are supported by the Tezos_crypto.Timelock `library <https://tezos.gitlab.io/api/odoc/_html/tezos-crypto/Tezos_crypto/Timelock/index.html>`__.

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

The raffle contract on Open Tezos `here <https://opentezos.com/archetype/raffle-example/raffle-contract/>`__
gives a concrete example of using time-lock.
