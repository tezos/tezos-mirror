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
network is activated (such as Carthage for Carthagenet), the protocol
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

Carthagenet
-----------

- Git branch: ``carthagenet``
- Run Docker image: ``wget -O carthagenet.sh https://gitlab.com/tezos/tezos/raw/carthagenet/scripts/alphanet.sh``

Carthagenet is a test network running the Carthage protocol.
Carthagenet will run until Carthage is replaced by another protocol on Mainnet.

On Carthagenet, the following constants differ from Mainnet:

- ``preserved_cycles`` is 3 instead of 5;
- ``blocks_per_cycle`` is 2048 instead of 4096;
- ``blocks_per_voting_period`` is 2048 instead of 32768;
- ``time_between_blocks`` is ``[ 30, 40 ]`` instead of ``[ 60, 40 ]``;
- ``test_chain_duration`` is 43200 instead of 1966080;
- ``quorum_min`` is 3000 (i.e. 30%) instead of 2000 (i.e. 20%);

This results in a faster chain than Mainnet:

- 2 blocks per minute;
- a cycle should last about 17 hours;
- a voting period lasts 1 cycle and should thus also last about 17 hours.

Zeronet
-------

- Git branch: ``zeronet``
- Run Docker image: ``wget -O zeronet.sh https://gitlab.com/tezos/tezos/raw/zeronet/scripts/alphanet.sh``

Zeronet is an unstable test network.
It is often reset to keep the chain short and to allow testing of
protocols under development.

On Zeronet, the following constants differ from Mainnet:

- ``blocks_per_cycle`` is 128 instead of 4096;
- ``blocks_per_roll_snapshot`` is 8 instead of 256;
- ``blocks_per_voting_period`` is 2816 instead of 32768;
- ``time_between_blocks`` is ``[ 20 ]`` instead of ``[ 60, 40 ]``;
- ``test_chain_duration`` is 43200 instead of 1966080;
- ``quorum_min`` is 3000 (i.e. 30%) instead of 2000 (i.e. 20%);
- ``delay_per_missing_endorsement`` is 2 instead of 8.

This results in a chain which moves even faster than Carthagenet:

- 3 blocks per minute instead of 1,
- one cycle should last less than 43 minutes instead of 2 days and 20 hours,
- a voting period should last less than 16 hours instead of about 23 days.
  Note that in Zeronet, a voting period is 22 cycles instead of 8 cycles in Mainnet.

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
