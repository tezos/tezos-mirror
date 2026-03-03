Key Management
==============

Securely managing keys is of utmost importance in any blockchain, including Tezos, because keys are used to sign sensitive operations such as transfers of valuable assets (tez, FA tokens, tickets, ...) or baking operations.

The Octez tool suite offers several solutions to store your private keys safely and use them securely for signing operations.
However, these solutions are **not** enabled by default, so you have to turn them on, as explained in this tutorial.

Indeed, by default:

- Private keys are stored in ``$OCTEZ_CLIENT_DIR/secret_keys``. Keys generated with ``octez-client`` are stored encrypted by default for Tezos ``mainnet``, whereas they are unencrypted by default for test networks.
- The client uses these keys to sign user operations (e.g. transfers) by itself.
- The baker daemon uses these keys to automatically sign its blocks and operations (e.g. (pre-)attestations).
- The baker's own key is used to sign consensus operations and :doc:`DAL <../shell/dal>` attestations.

The solutions provided to strengthen the security of the default key management and signing are the following:

- A hardware wallet (highly recommended) allows to:

  + store your private keys securely
  + sign user operations (e.g. transfers) interactively on the wallet
  + automatically sign baking operations, such as (pre-)attestations, more securely.

- Using option ``--encrypted`` to encrypt the keys when using a testnet (this is the default on mainnet, where you must use option ``--unencrypted`` to force unencrypted keys.)

- A separate signer daemon allows to decouple the client and baker from the signing process.

- Separate keys (known as "consensus key" and "companion key") can be used to sign consensus operations and/or DAL attestations, respectively.

  In particular, this allows executing the signer remotely (that is, on a different machine than the client and/or the baker), perhaps less exposed to attacks.

  As the keys only need to be accessible to the signer, they can also benefit from the lesser exposure. Even better (and recommended), a remote signer can be combined with a hardware wallet connected to the same machine as the signer.

These solutions are detailed in the rest of this page.

.. _ledger:

Ledger support
--------------

It is possible and advised to use a hardware wallet to securely store and manage your
keys. The Octez client supports Ledger Nano devices provided that they have
two Tezos apps installed, `Tezos Wallet app <https://github.com/trilitech/ledger-app-tezos-wallet>`_ and `Tezos Baking app <https://github.com/trilitech/ledger-app-tezos-baking>`_ which are provided by `Trilitech Ltd <https://www.trili.tech/>`_. They can be installed from the Ledger Live store.

For performance reasons (see `details <https://github.com/trilitech/ledger-app-tezos-baking?tab=readme-ov-file#benchmarking>`__), a Ledger Nano S Plus model is a preferable choice with respect to a Ledger Nano.

Ledger Manager
~~~~~~~~~~~~~~

The preferred way to set up your Ledger is to install `Ledger
Wallet
<https://shop.ledger.com/pages/ledger-wallet/>`_ (formerly Ledger Live).
On Linux make sure you correctly set up your ``udev`` rules as explained
`here <https://github.com/trilitech/ledger-app-tezos-baking/tree/main?tab=readme-ov-file#udev-rules-linux-only>`_.
Connect your Ledger, unlock it and go to the dashboard.
In Ledger Wallet install ``Tezos Wallet`` from the applications list.
To install the ``Tezos Baking`` app, you need to enable Developer Mode in the Ledger Wallet settings.
Then you can install the app from the Manager tab in the Ledger Wallet.

.. warning::

    Ledger does not support tz4 addresses. If you wish to use a tz4 address, consider using an alternative method.

Tezos Wallet app
~~~~~~~~~~~~~~~~

Connect the Ledger device and open the ``Tezos Wallet`` app on the Ledger device.
Now on the Octez client we can import the keys (make sure the device is
in the Tezos Wallet app)::

   ./octez-client list connected ledgers

This will display some instructions to import the Ledger encrypted private key, and
you can choose between the root or a derived address.
We can follow the instructions and then confirm the addition by listing known addresses::

   ./octez-client import secret key my_ledger ledger://XXXXXXXXXX
   ./octez-client list known addresses

Optional: we can check that our Ledger signs correctly using the
following command and confirming on the device::

   octez-client show ledger ledger://XXXXXXXXXX --test-sign

The address can now be used as any other with the exception that
during an operation the device will prompt you to confirm when it's
time to sign an operation.


Tezos Baking app
~~~~~~~~~~~~~~~~

The ``Tezos Baking`` app allows a delegate to sign automatically (i.e., there is no need
to manually sign every block or (pre-)attestation).
Of course, the application is restricted to only sign baking operations; it never signs a transfer, for example.
To provide additional protection from double signing the blocks/attestations, it is recommended to use ``octez-signer`` along with the Ledger baking app.

Setup the Ledger baking app on any Tezos network using the following command::

   octez-client setup ledger to bake for my_ledger

More details on setting up baking with the Ledger can be found on the `Tezos Baking app readme
<https://github.com/trilitech/ledger-app-tezos-baking>`_.

For details about baking using a Ledger device, see a `dedicated tutorial <https://docs.tezos.com/tutorials/bake-with-ledger>`__.

.. _signer:

Signer
------

A solution to decouple the client and the baker from the signing process is to
use a *remote signer*.

A complete manual page of the signer is available :ref:`here <signer_manual>`.

In this configuration, the client sends signing requests over a
communication channel towards ``octez-signer``, which can run on a
different machine that stores the secret key.

There are several *signing schemes* supported by the client, corresponding to different communication channels, such as ``unix``,
``tcp``, ``http`` and ``https``. We can list the available schemes with::

   octez-client list signing schemes

We now explain how this remote signer configuration works based on signing requests, how can it be set up, and how the connection to the signer can be secured (as by default it is not secure).

Signer requests
~~~~~~~~~~~~~~~

The ``octez-signer`` handles signing requests with the following format::

    <magic_byte><data>

In the case of blocks or consensus operations for example, this format is instantiated as follows::

    <magic_byte><chain_id><block|consensus_operation>

Starting with Octez v12 (supporting the Ithaca protocol), consensus operations also include :ref:`preattestations <quorum>`. The magic byte distinguishes pre-Ithaca messages from (post-)Ithaca messages, as follows:

.. list-table::
   :widths: 55 25
   :header-rows: 1

   * - Message type
     - Magic byte
   * - Legacy block
     - 0x01
   * - Legacy endorsement
     - 0x02
   * - Transfer
     - 0x03
   * - Authenticated signing request
     - 0x04
   * - Michelson data
     - 0x05
   * - Block
     - 0x11
   * - Pre-attestation
     - 0x12
   * - Attestation
     - 0x13

The magic byte values to be used by the signer can be restricted using its option ``--magic-bytes``, as explained in the :ref:`signer's manual <signer_manual>`.

Signer configuration
~~~~~~~~~~~~~~~~~~~~

In our home server we can generate a new key pair (or import one from a
:ref:`Ledger<ledger>`) and launch a signer that signs operations using these
keys.
To select the ``tcp`` signing scheme, one has to launch ``octez-signer`` with the ``socket`` argument, as shown below.
The new keys are stored by the signer in ``$HOME/.octez-signer`` in the same format
as ``octez-client``.
On our internet-facing virtual private server, called "vps" here, we can then import a key with the address
of the signer.

::

   home~$ octez-signer gen keys alice
   home~$ cat ~/.octez-signer/public_key_hashs
   [ { "name": "alice", "value": "tz1abc..." } ]
   home~$ octez-signer launch socket signer -a home

   vps~$ octez-client import secret key alice tcp://home:7732/tz1abc...
   vps~$ octez-client sign bytes 0x03 for alice

Every time the client on *vps* needs to sign an operation for
*alice*, it sends a signature request to the remote signer on
*home*.

However, with the above method, the address of the signer is hard-coded into the remote key value.
Consequently, if we ever have to move the signer to another machine or access it using another protocol, we will have to change all the remote keys.
A more flexible method is to only register a key as being remote, and separately supply the address of the signer using the ``-R`` option::

   vps~$ octez-client -R 'tcp://home:7732' import secret key alice remote:tz1abc...
   vps~$ octez-client -R 'tcp://home:7732' sign bytes 0x03 for alice

Alternatively, the address of the signer can be recorded in environment variables::

   vps~$ export TEZOS_SIGNER_TCP_HOST=home
   vps~$ export TEZOS_SIGNER_TCP_PORT=7732
   vps~$ octez-client import secret key alice remote:tz1abc...
   vps~$ octez-client sign bytes 0x03 for alice

All the above methods can also be used with the other signing schemes, for instance, ``http``::

   home~$ octez-signer launch http signer -a home

   vps~$ octez-client import secret key alice http://home:7732/tz1abc...
   vps~$ octez-client sign bytes 0x03 for alice

   vps~$ octez-client -R 'http://home:7732' import secret key alice remote:tz1abc...
   vps~$ octez-client -R 'http://home:7732' sign bytes 0x03 for alice

   vps~$ export TEZOS_SIGNER_HTTP_HOST=home
   vps~$ export TEZOS_SIGNER_HTTP_PORT=7732
   vps~$ octez-client import secret key alice remote:tz1abc...
   vps~$ octez-client sign bytes 0x03 for alice

The complete list of environment variables for connecting to the remote signer is:

+ ``TEZOS_SIGNER_TCP_HOST``
+ ``TEZOS_SIGNER_TCP_PORT`` (default: 7732)
+ ``TEZOS_SIGNER_HTTP_HOST``
+ ``TEZOS_SIGNER_HTTP_PORT`` (default: 6732)
+ ``TEZOS_SIGNER_HTTPS_HOST``
+ ``TEZOS_SIGNER_HTTPS_PORT`` (default: 443)
+ ``TEZOS_SIGNER_UNIX_PATH``
+ ``TEZOS_SIGNER_HTTP_HEADERS``

Secure the connection
~~~~~~~~~~~~~~~~~~~~~

Note that the above setup alone is not secure, **the signer accepts
requests from anybody and happily signs any transaction!**

Improving the security of the communication channel can be done at the
system level by setting up a tunnel with ``ssh`` or ``wireguard``
between *home* and *vps*.

The signer itself can also be configured to provide additional protection.
With the option ``--require-authentication`` the signer requires the
client to authenticate before signing any operation.

First we create a new key on the *vps* and then import it as an
authorized key on *home* where it is stored under
``.octez-signer/authorized_keys`` (similarly to ``ssh``).
Note that this key is only used to authenticate the client to the
signer and it is not used as a Tezos account.

::

   vps~$ octez-client gen keys vps
   vps~$ cat ~/.tezos-client/public_keys
   [ { "name": "vps",
       "value":
          "unencrypted:edpk123456789" } ]

   home~$ octez-signer add authorized key edpk123456789 --name vps
   home~$ octez-signer --require-authentication launch socket signer -a home-ip

All request are now signed with the *vps* key, guaranteeing
their authenticity and integrity.
However, this setup **does not guarantee confidentiality**: an eavesdropper can
see the transactions that you sign (on a public blockchain this may be less of a concern).
In order to avoid that, you can use the ``https`` scheme or a tunnel to encrypt your traffic.

.. _consensus_key_details:

Consensus Key and Companion Key
-------------------------------

Overview of Baker Key Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A delegate (baker) can use up to three distinct keys. The following table summarizes
their roles, constraints, and when each is needed:

.. list-table::
   :widths: 18 22 15 15 30
   :header-rows: 1

   * - Key
     - Purpose
     - Allowed types
     - Fund access
     - When needed
   * - **Baking (aka Manager) key**
     - Delegate identity; signs manager operations (transfers, staking, key updates)
     - tz1, tz2, tz3, tz4
     - Full
     - Always (every delegate has one; cannot be changed)
   * - **Consensus key**
     - Signs blocks and consensus operations (preattestations, attestations)
     - tz1, tz2, tz3, tz4
     - Can drain spendable balance
     - Optional: defaults to baking (manager) key; recommended for operational security
   * - **Companion key**
     - Signs DAL-specific payload included in aggregated attestations
     - tz4 only
     - None
     - Only needed for DAL participation when using a tz4 consensus key

By default, the baking (manager) key is also the consensus key. A separate consensus key is recommended for bakers who want
to keep their manager key offline (cold storage) while still baking. A companion key is only
needed in the specific scenario where the consensus key is a tz4 (BLS) key and the baker
participates in the :doc:`DAL <../shell/dal>`.

.. note::

   The "consensus key" feature is available since the :doc:`Lima<../protocols/015_lima>` protocol.
   The "companion key" feature is available since protocol :doc:`Seoul <../protocols/023_seoul>`.

Do I Need a Consensus Key?
~~~~~~~~~~~~~~~~~~~~~~~~~~

You **should** set up a consensus key if any of the following apply:

- You want to keep your manager key in cold storage (offline) while the baker runs with a separate key.
- You use a remote signer or cloud-hosted Key Management System (KMS) and want to isolate the signing key from the baker's identity.
- You want the ability to rotate your consensus key periodically without requiring your delegators to redelegate.
- You run your baker infrastructure in a cloud environment where you may lose access, and want to be able to switch to a new key quickly.

You can continue baking **without** a consensus key if you are comfortable using
your manager key directly for signing consensus operations.

Do I Need a Companion Key?
~~~~~~~~~~~~~~~~~~~~~~~~~~

A companion key is only required if **both** of the following conditions are true:

1. Your active consensus key (or baking (manager) key, if a different consensus key is not active) is a **tz4** (BLS) key.
2. You want your baker to include **DAL attestations** in its consensus operations.

If you use a tz1, tz2, or tz3 consensus key, you do **not** need a companion key---DAL attestations are signed with the consensus key directly.

If you have a tz4 consensus key but do **not** register a companion key, your baker
will still produce regular attestations, but will be unable to include DAL attestation
data. This means you will not participate in the Data Availability Layer,
and as a result, you might lose DAL participation rewards when the DAL is active.

For more details on the technical reason behind this requirement, see
:doc:`../shell/baker` (section "The Role of the Companion Key").

Consensus Key
~~~~~~~~~~~~~

By default, the baker's key, also called manager key, is used to sign in the consensus protocol, i.e. signing blocks while baking,
and signing consensus operations (preattestations and attestations).

A delegate may elect instead to choose a dedicated key: the *consensus key*. It can then be changed without redelegation.

It also allows establishment of baking operations in an environment where access is not ultimately guaranteed:
for example, a cloud platform providing hosted Key Management Systems (KMS) where the private key is
generated within the system and can never be downloaded by the operator. The delegate can designate
such a KMS key as its consensus key. Shall they lose access to the cloud platform for any reason, they can simply switch to a new key.

Security Model
^^^^^^^^^^^^^^

Using a consensus key improves operational security compared to baking directly with the manager key:

- **Manager key stays offline**: The manager key can be kept in cold storage (hardware wallet, air-gapped machine). Only the consensus key needs to be accessible to the baker or signer.
- **Key rotation without redelegation**: If the consensus key is compromised, you can rotate it using the offline manager key. Delegators are unaffected.
- **Faster incident response**: You can detect compromise and rotate the consensus key while the manager key remains safe.

.. warning::

   The consensus key can **drain the delegate's spendable balance** via a ``Drain_delegate``
   operation. Both the delegate's manager and consensus keys are listed in
   :doc:`relevant RPCs<../api/openapi>` like ``/chains/main/blocks/head/helpers/baking_rights``.
   The consensus key should therefore be treated with care, even though the manager key
   remains the primary identity.

To mitigate the risk of fund draining by a compromised consensus key:

- **Stake most of your funds**: only leave a minimum for operation fees in the spendable balance. Frozen (staked) funds cannot be drained.
- **Rotate the consensus key regularly**, especially before unstaking tez. The activation delay for a new consensus key is one cycle shorter than the unstake finalization delay.

.. note::

   In contrast, a **companion key** has **no access** to the delegate's funds. It can only
   sign DAL attestation data. If a companion key is compromised, the worst outcome is
   that the attacker can produce invalid DAL attestations — no funds are at risk.

Registering a Consensus Key
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A consensus key can be changed at any point.

The operation is signed by the manager key and does not require the consensus private key to be accessible by the client.

However the public key must be known by the client. It can be imported with the command::

   octez-client import public key <consensus_key> unencrypted:edpk...

The command to update the consensus key is::

   octez-client set consensus key for <manager_key> to <consensus_key>

The update becomes active after ``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles
(see :ref:`cs_constants`). We therefore distinguish the active consensus key and the pending
consensus keys. The active consensus key is by default the delegate’s manager key, which cannot change.

However, it is also possible to register as a delegate and immediately set the consensus key::

   octez-client register key <mananger_key> as delegate with consensus key <consensus_key>

There can be multiple pending updates: it is possible to have multiple pending consensus keys for multiple future cycles.
A subsequent update within the same cycle takes precedence over the initial one.

Note that registering a tz4 consensus key, just like revealing a tz4 public key, requires a proof of
possession. This is the signature of the consensus public key using the consensus private key, and it
ensures ownership of the key. This process is done automatically by the client, and the proof is included in
the receipt of the update operation.

.. _baking_consensus_key:

Baking With a Consensus Key
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In your baker's command, replace the delegate's manager key alias with the consensus key alias::

   octez-baker run with local node ~/.tezos-node <consensus_key_alias> --liquidity-baking-toggle-vote pass

While :ref:`transitioning from the delegate's manager key <consensus_key>`, it is possible to pass the alias for both delegate's manager key and consensus key.
The delegate will seamlessly keep baking when the transition happens::

   octez-baker run with local node ~/.tezos-node <consensus_key_alias> <delegate_key_alias> --liquidity-baking-toggle-vote pass

Draining a Manager's Account With its Consensus Key
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This operation immediately transfers all the spendable balance of the ``baker_pkh``’s user account into the ``destination_pkh`` user account::

   octez-client drain delegate <baker_pkh> to <destination_pkh> with <consensus_pkh>

If the destination is the consensus key account, this can be simplified to::

   octez-client drain delegate <baker_pkh> to <consensus_pkh>

The active consensus key is the signer for this operation, therefore the private key associated to the consensus key must be available
in the wallet of the client typing the command. The delegate's private key does not need to be present.

The drain operation has no effect on the frozen balance.

A fixed fraction of the drained delegate’s spendable balance is transferred as fees to the baker that includes the operation,
i.e. the maximum between 1 tez or 1% of the spendable balance.

.. _companion_key:

Companion Key
-------------

Starting with protocol S, bakers will be able to register a second key called the *companion key*. It is a tz4 key,
whose purpose is to sign DAL specific content in consensus operations. This key is required for delegates with
a tz4 consensus key that wish to participate in the DAL.

More precisely, if a delegate has an active tz4 consensus key, but no companion key is active,
or if it is missing from the client set of known keys, the baker
will still be able to produce attestations, but without any DAL attestations.
In other words, even if the baker is connected to a DAL node and receives attestable slots for the delegate,
since the companion key is not available, it will not be able to include a DAL attestation in its
consensus operation, and will only send a regular attestation.

Any delegate, regardless of their kind of address, can register a companion key,
it will only be used when necessary. There is no downside in doing so, because ``drain delegate``
only applies to consensus keys, not companion keys.

The command to update the companion key is::

   octez-client set companion key for <manager_key> to <companion_key>

Since a companion key has to be a tz4, this command will also create a proof of possession and include it in the operation.

A companion key takes the same amount of time as a consensus key to become activated, which is
up to ``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles (see :ref:`cs_constants`).

Alternatively, it is possible to register a companion key when registering as a delegate::

   octez-client register key <manager_key> as delegate --companion-key <companion_key>

It is even possible to register both a consensus key and a companion key, with the following command::

   octez-client register key <manager_key> as delegate --consensus-key <consensus_key> --companion-key <companion_key>

Please do (re)start the baker and provide the new companion key alias alongside the consensus and/or the delegate's key on the command line (the latter is still needed until the new keys become active)::

   octez-baker run with local node ~/.tezos-node <consensus_key> <companion_key> <delegate_key_alias> --liquidity-baking-toggle-vote pass

.. _consensus_companion_example:

End-to-End Example: Setting Up Consensus and Companion Keys
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example walks through a complete setup with a tz4 (BLS) consensus key
and a companion key for DAL participation.

**Step 1: Generate the keys**

Generate a BLS key for consensus and another for the companion role::

   octez-client gen keys my_consensus_key --sig bls
   octez-client gen keys my_companion_key --sig bls

**Step 2: Register as a delegate with both keys**

If you are registering as a new delegate::

   octez-client register key my_manager as delegate \
     --consensus-key my_consensus_key \
     --companion-key my_companion_key

If you are already a delegate, update each key separately::

   octez-client set consensus key for my_manager to my_consensus_key
   octez-client set companion key for my_manager to my_companion_key

**Step 3: Wait for activation**

Both keys become active after ``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles
(see :ref:`cs_constants`). You can check the pending keys via::

   octez-client rpc get /chains/main/blocks/head/context/delegates/<delegate_pkh>

Look for the ``pending_consensus_keys`` and ``pending_companion_keys`` fields.

**Step 4: Start the baker with all keys**

During the transition period (before the new keys activate), pass all key aliases
so the baker uses the right key at each point::

   octez-baker run with local node ~/.tezos-node \
     my_consensus_key my_companion_key my_manager \
     --liquidity-baking-toggle-vote pass

Once the new keys are active, you may omit the manager key alias, but keeping it
is harmless and recommended for smooth transitions during future key rotations.

**Step 5: Verify**

After activation, confirm your baker is using the new keys by checking the
attestation operations it produces, or by querying::

   octez-client rpc get /chains/main/blocks/head/context/delegates/<delegate_pkh>

The ``active_consensus_key`` and ``active_companion_key`` fields should reflect
your new keys.

.. _consensus_companion_faq:

Frequently Asked Questions
~~~~~~~~~~~~~~~~~~~~~~~~~~

**I set my consensus key, but the baker is still using the old one.**
   Consensus key updates take ``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles to activate
   (see :ref:`cs_constants`). During the transition, pass both key aliases to the baker
   command so it can use the right key at each point. Check the pending keys with::

      octez-client rpc get /chains/main/blocks/head/context/delegates/<delegate_pkh>

**I have a tz4 key. Do I need a companion key?**
   Only if you want to participate in the :doc:`DAL <../shell/dal>`. Without a companion key,
   your baker will still produce regular attestations, but will not include DAL attestation
   data. If you don't use the DAL, you don't need a companion key.

**Can I use the same tz4 key as both consensus key and companion key?**
   No. The consensus key and companion key must be distinct keys. They serve different
   cryptographic roles: the consensus key signs the common attestation payload (enabling
   aggregation across bakers), while the companion key signs the baker-specific DAL payload.

**How do I check which consensus key is currently active?**
   Query the delegate's information via RPC::

      octez-client rpc get /chains/main/blocks/head/context/delegates/<delegate_pkh>

   The ``active_consensus_key`` field shows the currently active key. The
   ``pending_consensus_keys`` field shows upcoming changes.

**I use a remote signer. How do I set up a consensus key with it?**
   Generate the consensus key on the signer, then import its **public key** on the client::

      # On the signer machine
      octez-signer gen keys my_consensus_key --sig bls

      # On the client machine (import the public key only)
      octez-client import public key my_consensus_key unencrypted:<public_key>
      octez-client set consensus key for my_manager to my_consensus_key

   The private key stays on the signer. The client only needs the public key to register
   the update. At baking time, the baker will request signatures from the signer.
   See :ref:`signer` for details on setting up the remote signer connection.

**What happens if my consensus key is compromised?**
   An attacker with your consensus key can drain your delegate's **spendable** balance
   (but not staked/frozen funds). To respond:

   1. Use your manager key (which should be in cold storage) to set a new consensus key.
   2. Consider draining your own account first if you still have access to the consensus key.
   3. The new key activates after ``CONSENSUS_KEY_ACTIVATION_DELAY + 1`` cycles.

   To minimize exposure, keep most funds staked and rotate the consensus key regularly.

.. _activate_fundraiser_account:

Getting keys for fundraiser accounts
------------------------------------

If you took part in the fundraiser but didn't yet activate your account,
it is still possible to activate your Mainnet account on https://check.tezos.com/.
This feature is also included in some wallets.
If you have any questions or issues, refer to that page or to the `Tezos
Foundation <https://tezos.foundation/>`_ for support.

You may also use ``octez-client`` to activate your account, but **be
warned that you should have
a very good understanding of key management in Tezos and be familiar
with the command-line.**
The first step is to recover your private key using the following
command which will ask for:

- the email address used during the fundraiser
- the 14 words mnemonic of your paper wallet
- the password used to protect the paper wallet

::

   octez-client import fundraiser key alice

Once you insert all the required information, the client computes
your secret key and it asks you to create a new password in order to store your
secret key on disk encrypted.

If you haven't already activated your account on the website, you can
use this command with the activation code obtained from the Tezos
foundation.

::

   octez-client activate fundraiser account alice with <code>

Check the balance with::

   octez-client get balance for alice

As explained above, your keys are stored under ``~/.tezos-client``.
We strongly advise you to first **make a backup** and then
transfer your tokens to a new pair of keys imported from a Ledger (see
:ref:`ledger`).
