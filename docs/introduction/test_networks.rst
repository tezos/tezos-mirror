Test Networks
=============

Mainnet is the main Tezos network, but is not appropriate for testing.
Other networks are available to this end.

Zeronet
-------

Zeronet is an unstable test network.
It is often reset to keep the chain short and to allow testing of
protocols which are being developed.

Babylonnet
----------

Babylonnet is a test network which runs the Babylon protocol.
It was spawned after the injection of the proposal for Babylon.
It will run until Babylon is no longer the activate protocol of Mainnet.

Future Networks
---------------

At some point, there will be a proposal for a successor to the Babylon
protocol (let's call it C for now). After C is injected, a new test network
(let's call it C-net) will be spawned. It will run alongside Babylonnet
until either C is rejected or activated. If C is rejected, C-net will
end, unless C is immediately re-submitted for injection. If, however,
C is activated, Babylonnet will end and C-net will continue on its own.

Old Networks
------------

Alphanet was the test network before Babylonnet. At the end of its life,
it was running the Athens protocol. Bootstrap nodes were shut down after
the Babylon protocol was activated on Mainnet.
