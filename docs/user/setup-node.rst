Setting up the node
===================

For running a node in the Tezos network with a basic setup, see the introductory :ref:`section on running the node <start_node>`.

This section describes different configuration possibilities provided by the Octez node:

- tune various parameters of the node using flexible combinations of: a configuration file, command-line options, and environment variables
- configure routing
- specify the Tezos network to connect to, which can be the Mainnet or different test networks.
- configure the amount of history kept by the node according to different tradeoffs, using history modes
- rapidly catch up with a given (main or test) network by loading network snapshots
- set up infrastructure for continuosly monitoring the node to help spotting operational or efficiency issues
- set up infrastructure for continuosly monitoring the chain consensus spotting operational or efficiency issues

These possibilities are described in the following pages.

.. toctree::
   :maxdepth: 2

   node-configuration

.. toctree::
   :maxdepth: 2

   routing

.. toctree::
   :maxdepth: 2

   multinetwork

.. toctree::
   :maxdepth: 2

   history_modes

.. toctree::
   :maxdepth: 2

   snapshots

.. toctree::
   :maxdepth: 2

   node-monitoring

.. toctree::
   :maxdepth: 2

   teztale
