.. _snoop:

Benchmarking with Snoop
=======================

If you have a piece of code for which you'd like to construct
a model predictive of its performance, ``tezos-snoop`` is the tool to
help you do that. It is in particular used to derive gas costs for
the Tezos protocol.

This tool allows to benchmark any given piece of OCaml code
and use these measures to fit cost models predictive of execution time.

.. toctree::
   :maxdepth: 2
   :caption: Architecture of tezos-snoop

   snoop_arch

.. toctree::
   :maxdepth: 2
   :caption: Using tezos-snoop by example

   snoop_example

.. toctree::
   :maxdepth: 2
   :caption: Rewriting Micheline terms

   tezos_micheline_rewriting
