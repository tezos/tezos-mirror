Setting up the node
===================

A node in the Tezos network provides different configuration possibilities:

- tune various parameters of the node using flexible combinations of: a configuration file, command-line options, and environment variables
- specify the Tezos network to connect to, which can be the Mainnet or different test networks.
- configure the amount of history kept by the node according to different tradeoffs, using history modes
- rapidly catch up with a given (main or test) network by loading network snapshots
- set up one or several proxy servers to decrease the load of the node by serving some RPC requests themselves
- set up infrastructure for continuosly monitoring the node to help spotting operational or efficiency issues

These possibilities are described in the following pages.

.. toctree::
   :maxdepth: 2

   node-configuration

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

   proxy-server

.. toctree::
   :maxdepth: 2

   node-monitoring
