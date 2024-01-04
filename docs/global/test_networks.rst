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
network is activated (such as Nairobi for nairobinet), the protocol
no longer changes because this could break the workflow of some users
while they are testing their development, as they may not be ready for
the new protocol. So every time a new protocol is proposed on Mainnet,
a new test network is spawned. This also makes synchronization much
faster than with a long-lived network.

.. _faucet:

Faucets
=======

Faucets can be accessed from https://teztnets.com/. Each of the test
network listed there, including the active test networks described
below, have independent faucets. Enter the public key hash of any test
account on the website to receive test tokens.

Future Networks
===============

At some point, there will be a proposal for a successor to the current
protocol (let's call this new protocol P). After P is injected, a new test network
(let's call it P-net) will be spawned. It will run alongside the latest
test network until either P is rejected or activated. If P is rejected, P-net will
end, unless P is immediately re-submitted for injection. If, however,
P is activated, the previous test network will end and P-net will continue on its own.

.. _ghostnet:

Ghostnet
========

Ghostnet is a long running, centrally managed test network designed to follow (in fact, anticipate!) Tezos Mainnet protocol upgrades.
Indeed, Ghostnet generally updates to the same protocol as Mainnet a few days before the Mainnet itself.

Ghostnet was previously known as :ref:`ithacanet`, the testchain for the Ithaca protocol.

See also
========

An external description of the various test networks available can be found on https://teztnets.com/.

Old Networks
============

Mumbainet
---------

Mumbainet was a test network running the Mumbai protocol.
Following the activation of the Nairobi protocol replacing Mumbai on Mainnet,
Mumbainet was deprecated and block production stopped on June 23rd, 2023.

Limanet
-------

Limanet was a test network running the Lima protocol.
Following the activation of the Mumbai protocol replacing Lima on Mainnet,
Limanet was deprecated and block production stopped on March 30th, 2023.

Kathmandunet
------------

Kathmandunet was a test network running the Kathmandu protocol.
Following the activation of the Lima protocol replacing Kathmandunet on Mainnet,
Kathmandunet stopped being maintained in December 2022 (the bootstrap baker
is no longer producing blocks).

Jakartanet
----------

Jakartanet was a test network running the Jakarta protocol.
Following the activation of the Kathmandu protocol replacing Jakarta on Mainnet,
Jakartanet stopped being maintained in October 2022 (the bootstrap baker
is no longer producing blocks).

.. _ithacanet:

Ithacanet
---------

Ithacanet was a test network running the Ithaca protocol.
Following the activation of the Jakarta protocol replacing Ithaca on Mainnet,
Ithacanet was converted to :ref:`ghostnet` on June 28, 2022.

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
