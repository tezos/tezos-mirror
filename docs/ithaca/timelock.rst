Time locked Michelson against Block Producer Extractable Value
==============================================================


Block Producer Extractable Value
--------------------------------

We aim at tackling the issue known through
the unfortunate misnomer of "generalized front running", described
for instance in `here <https://medium.com/@danrobinson/ethereum-is-a-dark-forest-ecc5f0505dff>`__.

Observing a transaction before it is actually included in the chain
can give an advantage to a trader against another one. Ultimately,
this means block producers can extract a rent from the system
as they have the ability to choose and order transactions in a block.
This is sometimes referred to, in proof-of-work networks like Ethereum as
`Miner Extractable Value <https://arxiv.org/pdf/1904.05234.pdf>`_ or MEV for shorts.
We refer to it as BPEV, for "Block producer extractable value"
or "Baker pecuniary extractable value", at the reader's preference.
We also note that the term "front-running" is regrettable as it can mislead people
into thinking there is a fiduciary relationship between block producers
and transaction emitters where, in fact, none exists unless explicitly contracted into.

For example, upon receiving a transaction, a baker could craft a block including
this transaction and one of his such that the sequential execution of these
two transactions guarantees a gain to the baker.

Timelock
--------

We propose a solution to alleviate this issue which is relatively easy to implement
and is based on time lock encryption
(see
`Timelock puzzles and timed release Crypto <http://www.hashcash.org/papers/time-lock.pdf>`_
for more details).

Time lock encryption allows to encrypt a message such that it can be
decrypted in two ways.
Either the author of the ciphertext produces a plaintext
(and a proof of correct decryption)
by providing a secret trapdoor (the factorization of an RSA modulus in our case).
Otherwise a sequential computation can decrypt the ciphertext after a computation
requiring ``T`` sequential operations (modular squaring in our case),
for some pre-determined constant ``T``.

In addition, a proof of the correctness of the decryption can also be produced and checked in sub linear time (``log T`` in our case).

By experimentally measuring the time the sequential operation takes
on available hardware using optimized implementation, we can estimate
a rough conversion (or a bound in our case) between the constant ``T`` and
wall clock time.
We also note that the `VDF alliance <https://www.vdfalliance.org/>`_ has been working on producing an ASIC for squaring in an RSA group to
ensure a level playing field in terms of computational speed.

An implementation of the timelock puzzle
and proof scheme is available in :src:`src/lib_crypto/timelock.mli` inspired from
a proof of concept available
`here <https://gist.github.com/murbard/23a29454a107d03d8a98393b0b98466d>`__.

General principle
-----------------

To limit BPEV, we introduce in Michelson an opcode (``OPEN_CHEST``) and two types (``chest`` and ``chest_key``) allowing
timelock-encrypted values to be used inside a Michelson contract.

The typical usage pattern would be as follows:

1. In a first period, a contract collects user-submitted and timelock encrypted Michelson values along with some valuable deposit, such as tez.
2. In a second period, after the values are collected, the contract collects from users a decryption of the value they submitted alongside with a proof that the decryption is correct.
3. In a third period, if any value remains undecrypted, anyone can claim part of the deposit by submitting a decryption of the value, with the other part of the deposit being burnt. Different penalties can be assessed depending on whether the user merely failed to submit a decryption for their value, or if they also intentionally encrypted invalid data. Different rewards can be distributed for submitting a correct decryption. The third period needs to be long enough so that people have enough time to perform the timelock decryption.
4. Finally, the contract can compute some function of all the decrypted data.

There is generally no incentive for users not to provide
the decryption of their data and thus the third period generally does not need
to take place. However, the second period needs to be long enough so that bakers
cannot easily censor submission of the decryption in a bid to later claim the reward.
Burning a part of the deposit also limits grieving attacks where a user gets back
their whole deposit by providing the decryption, but in a way that delays everyone else.

Cryptographic principles
------------------------

Users first generate a RSA modulus and a symmetric encryption key.
They use authenticated encryption to encrypt a packed Michelson value (an array of bytes computed with ``PACK``)
and encrypt that encryption key using a timelock puzzle.
They then combine the RSA modulus, the timelocked symmetric key, the constant ``T``
and the encrypted value as a single value as well (called chest in our library in :src:`src/lib_crypto`).

A proof of decryption can be the symmetric key itself.
However, a malicious user could propose an authenticated ciphertext that does yield a valid value
even when decrypted with the symmetric key that was indeed time locked.
To avoid this threat, an opening (called
the chest_key in our library) includes the symmetric key and
a proof that the symmetric key proposed is indeed the one hidden in the timelock puzzle.
In this way we can differentiate whether the chest or the chest key was proposed by a
malicious user.

Finally, our library exposes an ``open_chest`` function taking a chest, a chest key and
produces either the underlying plaintext or indicates that the chest or the chest key is
malicious.

Proposed opcode and types
---------------------------

To expose the features added by our library, we introduce the following Michelson types:

- ``chest``, which represents timelocked arbitrary bytes with the
  necessary public parameters to open it.
- ``chest_key``, which represents the decryption key,
  alongside with a proof that the key is correct.

and the following opcode:

``unlock ::  chest_key → chest → time →or (bytes, bool)``

If everything is correct it pushes
``Left bytes`` on top of the stack where bytes are
cryptographically guaranteed to be the bytes the chest provider timelocked.
If the ciphertext does not decrypt under the symmetric key that was timelocked, it pushes on the stack
``Right False``
If the provided symmetric key was not the one timelocked
(which we detect thanks to the timelock proof),
it pushes on the stack ``Right True``.
Note that we are using an authenticated encryption scheme,
so we can detect if someone provides a wrong key while fooling the time lock proof.
This is doable only by someone knowing the factorisation of the RSA modulus.
However, this cannot prevent someone from encrypting a wrong key, or putting
a wrong message authentication code,
so this is why we still need the proof of correctness.
