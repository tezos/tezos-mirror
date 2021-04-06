Key Management
==============


.. _ledger:

Ledger support
--------------

It is possible and advised to use a hardware wallet to manage your
keys, Tezos' client supports Ledger Nano devices provided that you have
a Tezos app installed.
The apps were developed by Obsidian Systems and they provide a comprehensive
`tutorial on how to install it.
<https://github.com/obsidiansystems/ledger-app-tezos>`_

Ledger Manager
~~~~~~~~~~~~~~

The preferred way to set up your Ledger is to install `Ledger
Live
<https://www.ledger.com/ledger-live/>`_.
On Linux make sure you correctly set up your `udev` rules as explained
`here <https://github.com/obsidiansystems/ledger-app-tezos#udev-rules-linux-only>`_.
Connect your ledger, unlock it and go the dashboard.
In Ledger Live `install Tezos Wallet` from the applications list and open it on the
device.


Tezos Wallet app
~~~~~~~~~~~~~~~~

Now on the client we can import the keys (make sure the device is
in the Tezos Wallet app)::

   ./tezos-client list connected ledgers

You can follow the instructions to import the ledger private key and
you can choose between the root or a derived address.
We can confirm the addition by listing known addresses::

   ./tezos-client import secret key my_ledger ledger://tz1XXXXXXXXXX
   ./tezos-client list known addresses

Optional: we can check that our ledger signs correctly using the
following command and confirming on the device::

   tezos-client show ledger path ledger://tz1XXXXXXXXXX

The address can now be used as any other with the exception that
during an operation the device will prompt you to confirm when it's
time to sign an operation.


Tezos Baking app
~~~~~~~~~~~~~~~~

In Ledger Live (with Developer Mode enabled), there is also a `Tezos Baking`
app which allows a delegate to sign non-interactively e.g. there is no need
to manually sign every block or endorsement.
The application however is restricted to sign exclusively blocks and
endorsement operations; it is not possible to sign for example a
transfer.
Furthermore the application keeps track of the last level baked and allows
only to bake for increasing levels.
This prevents signing blocks at levels below the latest
block signed.

If you have tried the app on some network and want to
use it on another network you might need to reset this level with the command::

   tezos-client setup ledger to bake for my_ledger

More details can be found on the `Tezos Ledger app
<https://github.com/obsidiansystems/ledger-app-tezos>`_.

.. _signer:

Signer
------

Another solution to decouple the node from the signing process is to
use the *remote signer*.
Among the signing scheme supported by the client, that we can list
with ``tezos-client list signing schemes``, there are ``unix``,
``tcp``, ``http`` and ``https``.
These schemes send signing requests over their respective
communication channel towards the ``tezos-signer``, which can run on a
different machine that stores the secret key.

In our home server we can generate a new key pair (or import one from a
:ref:`Ledger<ledger>`) and launch a signer that signs operations using these
keys.
The new keys are store in ``$HOME/.tezos-signer`` in the same format
as ``tezos-client``.
On our internet facing vps we can then import a key with the address
of the signer.

::

   home~$ tezos-signer gen keys alice
   home~$ cat ~/.tezos-signer/public_key_hashs
   [ { "name": "alice", "value": "tz1abc..." } ]
   home~$ tezos-signer launch socket signer -a home-ip

   vps~$ tezos-client import secret key alice tcp://home-ip:7732/tz1abc...

Every time the client on *vps* needs to sign an operation for
*alice*, it sends a signature request to the remote signer on
*home*.
Note that this setup alone is not secure, **the signer accepts
requests from anybody and happily signs any transaction!**

Secure the connection
~~~~~~~~~~~~~~~~~~~~~

Improving the security of the communication channel can be done at the
system level, setting up a tunnel with ``ssh`` or ``wireguard``
between *home* and *vps*, otherwise the signer already provides an
additional protection.

With the option ``--require-authentication`` the signer requires the
client to authenticate before signing any operation.
First we create a new key on the *vps* and then import it as an
authorized key on *home* where it is stored under
``.tezos-signer/authorized_keys`` (similarly to ``ssh``).
Note that this key is only used to authenticate the client to the
signer and it is not used as a Tezos account.

::

   vps~$ tezos-client gen keys vps
   vps~$ cat ~/.tezos-client/public_keys
   [ { "name": "vps",
       "value":
          "unencrypted:edpk123456789" } ]

   home~$ tezos-signer add authorized key edpk123456789 --name vps
   home~$ tezos-signer --require-authentication launch socket signer -a home-ip

All request are now signed with the *vps* key thus you are
guaranteed authenticity and integrity.
This set up **does not guarantee confidentiality**, an eavesdropper can
see the transactions that you sign but on a public blockchain this is
less of a concern.
You can still use the ``https`` scheme or the tunnel to encrypt your
traffic.


.. _activate_fundraiser_account:

Activate fundraiser account - Mainnet
-------------------------------------

If you took part in the fundraiser you can activate your account for
the Mainnet on https://check.tezos.com/.
This feature is also included in some wallets.
If you have any questions or issues, refer to that page or to the `Tezos
foundation <https://tezos.foundation/>`_ for support.

You may also use ``tezos-client`` to activate your account, **be
warned that you should have a very good understanding of key
management in Tezos and be familiar with the command-line.**
The first step is to recover your private key using the following
command which will ask for:

- the email address used during the fundraiser
- the 14 words mnemonic of your paper wallet
- the password used to protect the paper wallet

::

   tezos-client import fundraiser key alice

Once you insert all the required information, the client computes
your secret key and it asks to create a new password to store your
secret key on disk encrypted.

If you haven't already activated your account on the website, you can
use this command with the activation code obtained from the Tezos
foundation.

::

   tezos-client activate fundraiser account alice with <code>

Like explained above, your keys are stored under ``~/.tezos-client``.
We strongly advice you to first **make a backup** and then
transfer your tokens to a new pair of keys imported from a ledger (see
:ref:`ledger`).

Check the balance with::

    tezos-client get balance for alice
