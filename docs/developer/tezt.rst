Tezt: OCaml Tezos Test Framework
================================

`Tezt <https://gitlab.com/nomadic-labs/tezt/>`_ is a generic test framework written in OCaml.
It focuses on integration tests that launch external processes.
In particular, Tezt can be used to orchestrate tests involving Tezos nodes and clients.

To facilitate the use with Tezos, Tezt has been complemented by a specific library called Tezt-Tezos.

Tezt is pronounced `/tɛzti/ <http://ipa-reader.xyz/?text=t%C9%9Bzti>`_
(think "tezty", as in *Tez* are tas*ty*).


The main benefits of using Tezt-Tezos are:

- tests are written in the same language as Octez itself (OCaml),
  which reduces context switch for developers;

- tests do not actively poll the node
  as they passively listen to node events instead,
  which results in faster and more reliable tests;

- in verbose mode, logs show the interleaved output of all external processes,
  while the tests are running;

- it should be easy to use and extend.

Therefore, Tezt and Tezt-Tezos have been leveraged to build a test suite for Octez. See :src:`tezt/README.md` for details on its implementation.

The rest of this page explains how to run the test suite and how to add new tests.

How to Run Tests
----------------

It is recommended to set up an alias::

    alias tezt='dune exec tezt/tests/main.exe --'

Try it with ``--help`` to get the list of command-line options as follows::

    tezt --help

Just run ``tezt`` with no options to run all tests.
However, most often you only want to run a subset of the tests.
You can get the list of tests with ``--list``::

    tezt --list

For instance, to run only tests from files ``basic.ml`` and ``RPC_test.ml``
that have tag ``alpha`` but not tag ``regression``, with log level "info", run::

    tezt -f basic.ml -f RPC_test.ml alpha /regression -i

You can also run tests in parallel, although in that case it is recommended
to use the default log level to avoid interleaving logs. For instance,
the following command runs tests declared in file ``encoding.ml``
with up to 8 tests in parallel::

    tezt -f encoding.ml -j 8

How to Write New Integration Tests
----------------------------------

The best way to get started is to have a look at existing tests in directory
``tezt/tests`` of the Octez repository.

Most integration tests are part of the same executable ``tezt/tests/main.exe``.
The source of this module is :src:`tezt/tests/main.ml`.
This executable runs all tests, but you can restrict the set of tests to run
by specifying tags on the command line, or even the titles of the tests to run
(with the ``--test`` option).

All tests do not have to be implemented in :src:`tezt/tests/main.ml` though.
You can of course add more modules and have them be linked into ``main.exe`` together.
The best way to do this is to write your tests as functions and call them from
the main module.

For instance, let's create a new basic test in a new file named :src:`tezt/tests/basic.ml`:

.. literalinclude:: ../../tezt/tests/basic.ml
   :start-at: check_node_initialization
   :language: ocaml

Then, let's launch the test from :src:`tezt/tests/main.ml` by calling:

.. code-block:: ocaml

    Basic.register ~protocols:[Alpha] ;
    Test.run () (* This call should already be there. *)

Finally, let's try it with::

    dune exec tezt/tests/main.exe -- basic --info

The ``--info`` flag allows you to see the ``Log.info`` messages. It is the same as ``-i``.
Here is what you should see::

    $ dune exec tezt/tests/main.exe -- basic --info
    [13:45:36.666] Starting test: Alpha: node initialization (archive mode)
    [13:45:37.525] Activated protocol.
    [13:45:38.215] Baked 10 blocks.
    [13:45:38.215] Level is now 11.
    [13:45:38.215] Identity is not empty.
    [13:45:38.231] [SUCCESS] (1/3) Alpha: node initialization (archive mode)
    [13:45:38.231] Starting test: Alpha: node initialization (full mode)
    [13:45:39.113] Activated protocol.
    [13:45:39.813] Baked 10 blocks.
    [13:45:39.813] Level is now 11.
    [13:45:39.813] Identity is not empty.
    [13:45:39.828] [SUCCESS] (2/3) Alpha: node initialization (full mode)
    [13:45:39.828] Starting test: Alpha: node initialization (rolling mode)
    [13:45:40.708] Activated protocol.
    [13:45:41.407] Baked 10 blocks.
    [13:45:41.407] Level is now 11.
    [13:45:41.407] Identity is not empty.
    [13:45:41.422] [SUCCESS] (3/3) Alpha: node initialization (rolling mode)

Detailed Walk through the Basic Test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's review what our basic test in the previous section does.

- First, note that the Tezt library and its Base module are opened automatically by Dune
  based on its configuration file.
  The Base module contains useful functions such as ``let*`` (which is ``Lwt.bind``)
  or ``sf`` (a short-hand for ``Printf.sprintf``).

- Then, we define a function ``check_node_initialization`` which registers one test.
  It is parameterized by the history mode. ``Protocol.register_test`` is partially
  applied here; ``check_node_initialization`` is also implicitly parameterized by the
  list of protocols to run the test on.

- The function ``Protocol.register_test`` registers a test.
  It is a wrapper over ``Test.register``.
  This wrapper is preferred when the test is parameterized by a list of protocols.
  The ``~__FILE__`` argument gives the source filename so that one can select this
  file with the ``--file`` argument, to only run tests declared in this file.
  Each test has a title which is used in logs and on the command-line with the ``--test``
  option (which allows to run a particular test from its title).
  Each test also has a list of tags.
  We gave our test the tag ``basic`` in particular.
  No other test has this tag, so it is easy to run all of the tests of our new ``Basic``
  module, and only them, by adding ``basic`` on the command-line.

- The function ``Protocol.register_test`` takes a function as an argument.
  This function contains the implementation of the test.

- First, we initialize a node with ``Node.init``.
  This creates a node and runs the node command ``identity generate``,
  then ``config init`` and finally ``run``.
  It then waits until the node is ready, and returns the node.
  Note that you do not have to call ``Node.init``.
  For instance, if you want to test the behavior of the node without an identity,
  you can call ``Node.create``, followed by ``Node.config_init`` and ``Node.run``.

- Then, we initialize a client with ``Client.init``.
  We give it a node, which is the node that the client will connect to by default.
  Note that we can still use this client to perform operations on other nodes
  if we want to, it's just convenient to specify it once and for all.

- Then, we activate the protocol with the ``activate protocol`` command of the client.
  By default, this activates the protocol given as argument,
  with some default parameters and using the default activator key
  (defined in the ``Constant`` module).
  This activator key was added to the client by ``Client.init``.
  You can override all of this.
  For instance, if you don't want the client to know the default activator key,
  use ``Client.create`` instead of ``Client.init``
  (you can use ``Client.import_secret_key`` to import another activator key, for instance).
  Or, if you want to change the fitness or the parameter file,
  you can use the ``?fitness`` and ``?parameter_file`` optional
  arguments of ``Client.activate_protocol``.

- Then, we log a message using ``Log.info``.
  This message is not visible with the default verbosity, but you can
  see it by running ``main.exe`` with the ``--info`` option (or ``--verbose``).

- Then, we repeat ``Client.bake_for`` 10 times, to bake 10 blocks.

- Then, we wait for the level of the node to be at least 11 (the activation block
  plus the 10 blocks that we baked) using ``Node.wait_for_level``. If you call this
  function and the level is already 11 or greater, ``Node.wait_for_level`` returns
  immediately. (Note: ``Node.wait_for_level`` makes the test fail if the node stops before
  reaching level 11.)

- Finally, we read the identity of the node using ``Node.wait_for_identity`` which
  returns as soon as the node reads the identity file. In fact, this was probably
  done much sooner, but Tezt stores the identity in case you try to query it
  later, just like the level. (Note: ``Node.wait_for_identity`` makes the test fail
  if the node stops before reading the identity file.)

- We check that the identity is not empty, and if it is we call ``Test.fail``.
  This causes the test to terminate immediately with an error.
  Note that it is not the only cause of failure for this test:
  we already saw that ``Node.wait_for_level`` and ``Node.wait_for_identity`` can
  cause a test failure, and if anything goes wrong (failing to initialize the
  node or the client, failing to activate the protocol...) ``Test.fail`` is called
  automatically as well.

- After the test succeeds or fails, ``Test.run`` cleans up everything.
  It terminates all running processes by sending ``SIGTERM``.
  It waits for them with ``waitpid`` to avoid zombie processes.
  And it removes all temporary files, in particular the data directory of the node
  and the base directory of the client.

- We run this test three times, once per history mode: ``archive``, ``full`` and ``rolling``.
  Note that we added the history mode as a tag to ``Test.register``, so if we want
  to run only the test for history mode ``full``, for instance,
  we can simply run ``dune exec tezt/tests/main.exe -- basic full``.
  You can see our list of basic tests and their tags
  with ``dune exec tezt/tests/main.exe -- basic --list``.

How to Write New Unit Tests
---------------------------

For Tezt, the main difference between a unit test and an integration test is
that a unit test does not use the ``Process`` module. Indeed, an integration
test tests an executable by running it, while a unit test links with a library
to test its functions directly.

Since the scope of unit tests is much smaller, they are usually registered
in executables that are dedicated to the library being tested. For instance,
tests for ``src/lib_base`` could be put in an executable named
``src/lib_base/tezt/main.exe``. Such executables are usually faster to
compile and run than ``tezt/tests/main.exe`` since their dependency cone is limited.

To add unit tests to a library which does not yet have a Tezt executable,
create files such as ``src/lib_base/tezt/example.ml`` and ``src/lib_base/tezt/other.ml``
and use ``Test.register`` to register tests in those files, at toplevel.
Here is a minimal example::

    let () =
      Test.register
        ~__FILE__
        ~title:"test title here"
        ~tags:["test"; "tags"; "here"]
      @@ fun () ->
      (* your test here *)
      unit

Then, declare those files in ``manifest/main.ml``::

    let _octez_base_tezts =
      tezt
        ["example"; "other"]
        ~path:"src/lib_base/tezt"
        ~opam:"tezos-base"
        ~deps:[octez_base]

This causes the manifest to generate executable ``src/lib_base/tezt/main.exe`` for you.
This executable calls ``Test.run``. It also declares a Dune alias ``runtest``
so that you can run your tests with either of the following commands::

    dune build @src/lib_base/runtest
    dune exec src/lib_base/tezt/main.exe

Note that your tests will actually also be available in ``tezt/tests/main.exe``.
This executable gathers all tests so that the CI can auto-balance them.

JavaScript
~~~~~~~~~~

If you want to be able to run your test with Node.js, declare them in the manifest
with ``~js_compatible:true`` and with ``JS`` in ``~modes``. For instance::

    let _octez_base_tezts =
      tezt
        ["example"; "other"]
        ~path:"src/lib_base/tezt"
        ~opam:"tezos-base"
        ~js_compatible:true
        ~modes:[Native; JS]
        ~deps:[octez_base]

Running ``dune build`` will generate not only a native executable
(``src/lib_base/tezt/main.exe``) but also a JavaScript file
(``src/lib_base/tezt/main_js.bc.js``) that you can run with Node.js::

    nodejs _build/default/src/lib_base/tezt/main_js.bc.js

Note however that tests that use ``Tezt.Process``, ``Tezt.Temp`` or ``Tezt.Runner``
cannot use the JavaScript backend. In other words, integration tests cannot
be run with Node.js, only unit tests.

Regression Tests
----------------

Regression tests are used to prevent unintended changes to existing
functionality by ensuring that the software behaves the same way as it did
before introduced changes.

Regression tests capture commands and output of commands executed during a test.
An output of some regression test is stored in the repository and is expected to
match exactly with the captured output on subsequent runs. An added advantage of
this is that when a change in behaviour is intentional, its effect is made
visible by the change in test's output.

To run all the regression tests, use the ``regression`` tag::

    dune exec tezt/tests/main.exe regression

When the change in behaviour is intentional or when a new regression test is
introduced, the output of regression test must be (re-)generated. This can be
done with the ``--reset-regressions`` option, e.g.::

    dune exec tezt/tests/main.exe regression -- --reset-regressions

Regression tests are registered with ``Regression.register`` instead of
``Test.register``. Use ``Regression.capture`` or ``Regression.hooks`` to
capture output that you want to be stable. Regression tests can be used
both in unit tests and integration tests.
