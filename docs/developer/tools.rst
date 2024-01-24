Platform Development tools
==========================

The development and maintenance process of the Tezos platform is facilitated by some specialized tools.
Some of these tools are included in the Octez repository, because of a close coupling with the code itself (see :doc:`repository_scope` for the policy of selecting such tools).
They provide, for example, support for profiling or for benchmarking different subsystems of Octez.

On the other hand, contributing to the development of the Octez repository requires installing some additional infrastructure, which is not needed by regular Octez users.
For instance, developers need Python for building the documentation.

The tools for platform developers, as well as the configuration of the additional infrastructure, are documented in the following pages.

.. toctree::
   :maxdepth: 2

   profiling
   snoop
   time_measurement_ppx
   python_environment
   pre_commit_hook
