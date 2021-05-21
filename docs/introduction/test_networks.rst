.. TODO nomadic-labs/tezos#462: search shifted protocol name/number & adapt

.. _test-networks:

Test Networks
=============

Mainnet is the main Tezos network, but is not appropriate for testing.
Other networks are available to this end. Test networks usually run
with different constants to speed up the chain.

There is one test network for the current protocol, and one test
network for the protocol which is being proposed for voting. The
former is obviously important as users need to test their development
with the current protocol. The latter is also needed to test the proposed
protocol and its new features, both to decide whether to vote yes and
to prepare for its activation. After the intended protocol of a test
network is activated (such as Florence for FLorencenet), the protocol
no longer changes because this could break the workflow of some users
while they are testing their development, as they may not be ready for
the new protocol. So every time a new protocol is proposed on Mainnet,
a new test network is spawned. This also makes synchronization much
faster than with a long-lived network.

Get Free Funds
--------------

Test networks have a list of built-in accounts with some funds.
You can obtain the key to these accounts from a faucet to claim the funds.
All networks share the same faucet: https://faucet.tzalpha.net/.
The keys obtained from this faucet can be used in all test networks.

Granadanet
----------

- Built-in network alias: ``granadanet`` (see :ref:`builtin_networks`)

  * Available in version 9.2

- Run Docker image: ``wget -O granadanet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh``

Granadanet is a test network running the ``PtGRANAD`` protocol.
Granadanet will run until Granadanet is rejected or replaced by another protocol on Mainnet.

On Granadanet, the following constants differ from Mainnet:

- ``preserved_cycles`` is 3 instead of 5;
- ``blocks_per_cycle`` is 2048 instead of 4096;
- ``blocks_per_voting_period`` is 1024 instead of 32768;
- ``time_between_blocks`` is ``[ 30, 20 ]`` instead of ``[ 60, 40 ]``;
- ``delay_per_missing_endorsement`` is 4 instead of 8.

This results in a faster chain than Mainnet:

- 2 blocks per minute;
- a cycle should last about 17 hours;
- a voting period lasts half a cycle and should thus last about 8 hours.

Florencenet
-----------

- Built-in network alias: ``florencenet`` (see :ref:`builtin_networks`)

  * Available in version 9.0

- Run Docker image: ``wget -O florencenet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh``

Florencenet is a test network running the ``PsFLoren`` protocol.
Florencenet will run until Florence is replaced by another protocol on Mainnet.

On Florencenet, the following constants differ from Mainnet:

- ``preserved_cycles`` is 3 instead of 5;
- ``blocks_per_cycle`` is 2048 instead of 4096;
- ``blocks_per_voting_period`` is 1024 instead of 32768;
- ``time_between_blocks`` is ``[ 30, 20 ]`` instead of ``[ 60, 40 ]``;
- ``test_chain_duration`` is 0 instead of 1966080;
- ``delay_per_missing_endorsement`` is 4 instead of 8.

This results in a faster chain than Mainnet:

- 2 blocks per minute;
- a cycle should last about 17 hours;
- a voting period lasts half a cycle and should thus last about 8 hours.

Future Networks
---------------

At some point, there will be a proposal for a successor to the current
protocol (let's call this new protocol P). After P is injected, a new test network
(let's call it P-net) will be spawned. It will run alongside the latest
test network until either P is rejected or activated. If P is rejected, P-net will
end, unless P is immediately re-submitted for injection. If, however,
P is activated, the previous test network will end and P-net will continue on its own.

Old Networks
============

Edo2net
-------

Edo2net was a test network running the Edo protocol.
Following the activation of the Florence protocol replacing Edo on Mainnet,
Edo2net stopped being maintained on May 11th, 2021.

Delphinet
---------

Delphinet was a test network running the Delphi protocol.
Following the activation of the Edo protocol replacing Delphi on Mainnet,
Delphinet stopped being maintained on Febuary 28th, 2021.

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
