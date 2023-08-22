Adding a new protocol environment
=================================

The economic protocols of Tezos are compiled against a restricted set of libraries.
This is for security reasons (so that, e.g., the protocol can never be leveraged for accessing files
of the host machine) and safety reasons (so that, e.g., the protocol is immune to some categories of errors).
See the general overview in :doc:`Protocol environment <../shell/protocol_environment>`.

The set of libraries a protocol is compiled against is called the *protocol environment*, or simply, the *environment*.

Each protocol is compiled against a given environment, as declared in the protocol's ``TEZOS_PROTOCOL`` manifest file. In order to ensure that old protocols can still be compiled in the future, all the environments are set in stone: their interface cannot ever be modified. Consequently, when new features are needed for new protocols, a whole new environment must be created (otherwise, if no new feature is needed, a protocol can use the same environment as its predecessor's).

This page details the process of creating a new environment by copying the latest environment and building upon it. In the rest of this page, ``<N>`` is the version number for the new environment you are creating and ``<N-1>`` is the version number for the existing environment that you are copying from.


Bootstrap
---------

The following steps are roughly the steps taken in the `V6 bootstrap MR <https://gitlab.com/tezos/tezos/-/merge_requests/4961>`__

1. Copy the existing environment files:

   * Copy the directory ``src/lib_protocol_environment/sigs/v<N-1>`` into ``src/lib_protocol_environment/sigs/v<N>``.

   * Copy the file ``src/lib_protocol_environment/sigs/v<N-1>.in.ml`` into ``src/lib_protocol_environment/sigs/v<N>.in.ml`` and change any reference from ``v<N-1>`` to ``v<N>`` in the copied file.

2. Make the new environment buildable by updating ``manifest/main.ml``:

   * Bump the ``latest_environment_number`` in ``manifest/main.ml``.

   * Run ``make -C manifest``.

3. Copy the existing compatibility layer if any (see details in `Struct compatibility layer <#struct-compatibility-layer>`__).

   * Update  ``src/lib_protocol_environment/structs/tezos_protocol_environment_structs.ml`` to add a new submodule ``V<N>`` by copying the submodule ``V<N-1>``.

4. Copy and adapt the environment functor:

   * Copy ``src/lib_protocol_environment/environment_V<N-1>.ml[i]`` to ``src/lib_protocol_environment/environment_V<N>.ml[i]``.

   * Change any reference from ``V<N-1>`` to ``V<N>`` in all those copied files.

   * Update ``src/lib_protocol_environment/environment_context_intf.ml``.

5. If the protocol signature is expected to change then copy and adapt it otherwise leave it as is:

   ``Environment_protocol_T_V<X>`` is the current protocol signature and ``<X>`` is equal to the environment version that introduces it.

   * Copy ``src/lib_protocol_environment/environment_protocol_T_V<X>.ml`` to ``src/lib_protocol_environment/environment_protocol_T_V<N>.ml``.

   * Change ``Environment_protocol_T_V<X>`` to ``Environment_protocol_T_V<N>`` in ``src/lib_protocol_environment/environment_V<N>.ml``.


6. Add references to the new environment version number in the rest of the code:

   * ``src/lib_base/protocol.ml[i]``

   * ``src/lib_validation/block_validation.ml``

7. Adapt demo protocols to the new environment:

   * Modify the required environment in ``src/proto_demo_noops/lib_protocol/TEZOS_PROTOCOL`` and ``src/proto_demo_counter/lib_protocol/TEZOS_PROTOCOL``.

   * Verify they both compile with ``dune build @src/proto_demo_noops/runtest_compile_protocol`` and ``dune build @src/proto_demo_counter/runtest_compile_protocol``.

8. Commit all those changes and open an MR with your changes.

It is recommended that you test your work more comprehensively offline. To that end, follow the instructions below on how to activate the environment, and then run the protocol tests locally. Do not commit the changes or at least, do not push the changes.


Struct compatibility layer
^^^^^^^^^^^^^^^^^^^^^^^^^^

The struct compatibility layer is for providing compatibility between a signature of the protocol environment (which is set in stone) and the interface of an external library that provides it (which might change from version to version). E.g., at the time of the V0 environment the OCaml Stdlib did not include an ``Option`` module and so a custom one was provided in the whole of the Tezos project including the protocol environment; later, when the Tezos project switched to the now available and standard ``Stdlib.Option`` module, the struct compatibility module ``src/lib_protocol_environment/structs/v0_option.ml`` was added.

More recent protocol environments generally need fewer struct compatibility modules. Occasionally, the most recent environment needs no compatibility layer at all. You can know if this is the case by checking the file ``src/lib_protocol_environment/structs/tezos_protocol_environment_structs.ml``: if the submodule ``V<N>`` exists and is not empty then there is a compatibility layer, otherwise there isn't.

Either way, the instructions in the list above are sufficient for creating the new environment.


Activating the environment
--------------------------

The new environment as it stands now is not activated. More precisely, it cannot be used by any protocol. A few more changes are needed before it can be used.

When to activate
^^^^^^^^^^^^^^^^^

This is on purpose: we do not want to release an unfinished environment because it interferes with the distributed nature of Tezos protocol development. Specifically, if an unfinished protocol was made available in a release of the Octez suite, then anyone could propose a protocol built upon this version. But then further work on the protocol (to finish it) would create multiple different environments that have the same name. To avoid this, we only activate the environment once it is safe.

The new environment should only be activated after the last release that precedes the injection of the protocol that uses it. Don't worry too much about this, simply reach out to a release manager and work with them on the schedule.

How to activate
^^^^^^^^^^^^^^^^

To activate the environment you will need to change the following files, adding references to ``V<N>`` to match the references to ``V<N-1>``:

* ``src/lib_protocol_environment/tezos_protocol_environment.ml[i]``
* ``src/lib_protocol_updater/registered_protocol.ml[i]``
* ``src/lib_protocol_compiler/registerer/tezos_protocol_registerer.ml[i]``

Bump environment version in:

* ``src/bin_client/test/proto_test_injection/TEZOS_PROTOCOL``
* ``tezt/tests/voting.ml`` (in the embedded ``TEZOS_PROTOCOL``)
* ``src/lib_store/unix/test/test_consistency.ml``

And finally, bump environment version in ``src/proto_alpha/lib_protocol/TEZOS_PROTOCOL``, and run ``make -C manifest``.

For an example, check `the MR in which the environment V6 was activated <https://gitlab.com/tezos/tezos/-/merge_requests/4961>`__.

Additionally, you have to update the documentation of protocol Alpha to reflect the fact that it now uses environment ``V<N>``. For that, see meta-issue :gl:`#4155`, which explains all the necessary changes (don't worry, the changes are very limited).

Making changes in the environment
---------------------------------

You can make changes to the newly created environment until it is released. For this purpose release candidates do not count. Below are examples of changes from previous work on the environment.

* `Adding the Result module in environment V3 <https://gitlab.com/tezos/tezos/-/merge_requests/3154/diffs?commit_id=9aa7bee8a73f9495787dc9ee257e5021d31bee33>`__

  * Add the interface file ``src/lib_protocol_environment/sigs/v3/result.mli``

  * Add a reference to the file in ``src/lib_protocol_environment/sigs/v3.in.ml``

  * Declare the ``Result`` module in the functor in ``src/lib_protocol_environment/environment_V3.ml``

* `Updating the data-encoding dependency <https://gitlab.com/tezos/tezos/-/merge_requests/3149>`__

  * Provide backwards compatibility layers for older environments

  * Modify existing ``src/lib_protocol_environment/sigs/v3/data_encoding.mli``

* `Rehauling the List module in the environment V3 <https://gitlab.com/tezos/tezos/-/merge_requests/3116/diffs?commit_id=697b3da1e4b7135b0109dbdc6543e08a21038658>`__

  * Replace some of the environment modules with a new one (remove old files)

  * Remove struct compatibility module (the new interface is identical to the one in the most recent library)
