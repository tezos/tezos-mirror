The Tezos/Tezos repository
===========================

The `Gitlab repository tezos/tezos <https://gitlab.com/tezos/tezos>`_
contains the source code for Octez, as well as :ref:`the embedded
economic protocols <embedded_protocols>` for Tezos. It contains
libraries constituting integral parts of the project like ``lib_rpc``
or ``lib_error_monad``.  Some of these libraries are directly linked
to Tezos executables, while others, like ``lib_benchmark`` are used in
the development process as tools which allow the developers to make
the software better.

Sometimes such helper projects are developed independently at first.
But during its development such a program becomes significantly
coupled to one or more libraries developed as part of the Octez project.
Maintaining such a tool is frustrating, because it often breaks due to
changes in the Tezos libraries it depends upon. A natural solution to
this problem would be to include the tool in ``tezos/tezos`` repository
and develop it together with Octez, which allows to discover breakages
more quickly and prevent or fix them immediately.

However, the Tezos ecosystem is still growing and it's impossible to
develop everything within a single repository. Therefore it is necessary
to select which projects can be merged into Octez respository and which
cannot. The currently accepted rule is that to be merged into Octez, an
external project should fulfill *all* the following criteria:

#. depend on internals of Octez libraries, as opposed to public APIs such as
   inter-process communication like RPC.
#. the dependency mentioned above should be unavoidable. If there is
   another way to build the project, for example by avoiding depending
   on internals of Tezos; adding such an optional dependency won't qualify
   a project to be included.
#. the dependency should be on a feature or part of the codebase that is
   unstable enough that depending on released versions of Octez wouldn't
   be enough.
#. be used regularly as a helper in developing Tezos (e.g. run in the
   CI pipeline).

