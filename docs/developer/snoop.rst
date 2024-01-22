Benchmarking with Snoop
=======================

If you have a piece of code for which you'd like to construct
a model predictive of its performance, ``octez-snoop`` is the tool to
help you do that. This tool allows to benchmark any given piece of OCaml code
and use these measures to fit cost models predictive of execution time.

It is in particular used to derive the functions in the
:package-api:`Michelson gas cost API <tezos-protocol-alpha/Tezos_raw_protocol_alpha/Michelson_v1_gas/index.html>`,
computing the gas costs in the Tezos protocol.

.. toctree::
   :maxdepth: 2
   :caption: Architecture of octez-snoop

   snoop_arch

.. toctree::
   :maxdepth: 2
   :caption: Using octez-snoop by example

   snoop_tutorial

.. toctree::
   :maxdepth: 2
   :caption: octez-snoop: going further for more control

   snoop_example

.. toctree::
   :maxdepth: 2
   :caption: Rewriting Micheline terms

   tezos_micheline_rewriting

.. toctree::
   :maxdepth: 2
   :caption: Writing your very own benchmarks and models for the Michelson interpreter

   snoop_interpreter
