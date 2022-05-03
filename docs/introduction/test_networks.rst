.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

.. _test-networks:

=============
Test Networks
=============

Mainnet is the main Tezos network, but is not appropriate for testing.
Other networks are available to this end. Test networks usually run
with different :ref:`constants <protocol_constants>` to speed up the chain.

There is one test network for the current protocol, and one test
network for the protocol which is being proposed for voting. The
former is obviously important as users need to test their development
with the current protocol. The latter is also needed to test the proposed
protocol and its new features, both to decide whether to vote yes and
to prepare for its activation. After the intended protocol of a test
network is activated (such as Ithaca for ithacanet), the protocol
no longer changes because this could break the workflow of some users
while they are testing their development, as they may not be ready for
the new protocol. So every time a new protocol is proposed on Mainnet,
a new test network is spawned. This also makes synchronization much
faster than with a long-lived network.

Get Free Funds
==============

Test networks have a list of built-in accounts with some funds. You
can obtain the key to these accounts from a faucet to claim the funds.
Faucets can be accessed from https://teztnets.xyz/. Each of the test
network listed there, including the active test networks described
below, have independent faucets.

Jakartanet
==========

- Built-in network alias: ``jakartanet`` (see :ref:`builtin_networks`)

  * Available from version 13.0.

Jakartanet is a test network which runs the Jakarta 2 protocol.
Jakartanet will run until Jakarta is refused through the amendment process
or replaced by another protocol on Mainnet.

On Jakartanet, some constants differ from Mainnet.
This results in a faster chain than Mainnet.
See :ref:`protocol constants <protocol_constants>` to learn how to find out their values.

Ithacanet
=========

- Built-in network alias: ``ithacanet`` (see :ref:`builtin_networks`)

  * Available from version 12.0~rc1 but 12.0~rc2 updated it to refer
    to the second version of Ithacanet which runs ``Psithaca2`` instead
    of ``PsiThaCa``.

Ithacanet is a test network which ran the Ithaca protocol and which was reset
to run on the Ithaca2 protocol.
Ithacanet will run until Ithaca2 is replaced by another protocol on Mainnet.

On Ithacanet, some constants differ from Mainnet.
This results in a faster chain than Mainnet.
See :ref:`protocol constants <protocol_constants>` to learn how to find out their values.

Future Networks
===============

At some point, there will be a proposal for a successor to the current
protocol (let's call this new protocol P). After P is injected, a new test network
(let's call it P-net) will be spawned. It will run alongside the latest
test network until either P is rejected or activated. If P is rejected, P-net will
end, unless P is immediately re-submitted for injection. If, however,
P is activated, the previous test network will end and P-net will continue on its own.

Old Networks
============


Hangzhounet
-----------

Hangzhounet was a test network running the Hangzhou protocol.
Following the activation of the Ithaca protocol replacing Hangzhou on Mainnet,
Hangzhounet stopped being maintained on April 6, 2022 (the bootstrap baker
is no longer producing blocks).

Granadanet
----------

Granadanet was a test network running the Granada protocol.
Following the activation of the Hangzhou protocol replacing Granada on Mainnet,
Granadanet stopped being maintained on December 4, 2021 (the bootstrap baker
is no longer producing blocks).

Florencenet
-----------

Florencenet was a test network running the Florence protocol.
Following the activation of the Granada protocol replacing Florence on Mainnet,
Florencenet stopped being maintained on August 6, 2021 (the bootstrap baker
is no longer producing blocks).

Edo2net
-------

Edo2net was a test network running the Edo protocol.
Following the activation of the Florence protocol replacing Edo on Mainnet,
Edo2net stopped being maintained on May 11th, 2021 (the bootstrap baker is
no longer producing blocks).

Delphinet
---------

Delphinet was a test network running the Delphi protocol.
Following the activation of the Edo protocol replacing Delphi on Mainnet,
Delphinet stopped being maintained on February 28th, 2021 (the bootstrap baker
is no longer producing blocks).

Dalphanet
---------

Dalphanet was an experimental test network spawned during summer 2020
featuring Sapling and baking accounts. Since this test network required
a modified protocol environment, it was not available in any release branch.
It was available in experimental branch ``dalpha-release``.

Carthagenet
-----------

Carthagenet was a test network running the Carthage protocol.
Following the activation of the Delphi protocol replacing Carthage on Mainnet,
Carthagenet stopped being maintained on December 12th, 2020.

Babylonnet
----------

Babylonnet was a test network which ran the Babylon protocol.
It was spawned after the injection of the proposal for Babylon.
It ended its life on March 31st, 2020 as Carthage
replaced Babylon on Mainnet on March 5th, 2020.

Alphanet
--------

Alphanet was the test network before Babylonnet. At the end of its life,
it was running the Athens protocol. Bootstrap nodes were shut down after
the Babylon protocol was activated on Mainnet.

Zeronet
-------

Zeronet is a generic name for an unstable test network that is sometimes spawned
when the need arises. It is currently not running. When it was running, it was used
to test protocol proposals that were in development. It was reset frequently.
