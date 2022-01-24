:orphan:

Compiling (part of) the Octez codebase to JavaScript
====================================================

We want to expose a JavaScript API while staying in sync with the
OCaml codebase. A way to achieve this is to compile OCaml code to
JavaScript using the ``js_of_ocaml`` compiler.

The goal of this document is to collect information regarding the
JavaScript compilation story inside the Octez codebase.

Current status
--------------

Before we can expose a javascript api, we need to make sure libraries
can be correctly compiled and tested with js_of_ocaml.  Pure ocaml
libraries are usually trivial to deal with.  Crypto libraries written
in C/C++ need extra work to provides the corresponsing javascript
stubs.

We track the libraries compatibility in the manifest file
``manifest/main.ml`` and enfore that all transitive dependencies of a
JS-compatible library are JS-compatible.  A library is JS-compatible if it
has ``~js_compile:true`` or ``~js_of_ocaml``.

In the first phase, the focus is restricted to testing libraries
needed to expose an Octez client API.

Installing node (nodejs)
------------------------

In order to run JavaScript tests, one needs ``node`` to be installed.

One way to achieve this is to rely on ``nvm``.  Use the following
commands to install ``nvm`` and ``node``:

::

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
    scripts/install_builld_deps.js.sh


Limitation with inline_tests
----------------------------

``Inline_tests`` (e.g. ``ppx_inline_test``) are compatible with
``js_of_ocaml``, but we currently can't configure the alias under which
they run.

Because we don't want to force a ``nodejs`` dependency on everybody, we
can't have JavaScript test run under the ``runtest`` alias by default.

To workaround this limitation, running
``dune exec ./src/tooling/run_js_inline_tests.exe`` will temporarily
patch dune files and run ``dune runtest`` for you.

Running tests
-------------

One can run JavaScript tests with ``make test-js`` in the project root
or directly using dune with ``dune builld @SOME-PATH/runtest_js``.

In addition, to run inline_tests, execute
``dune exec ./src/tooling/run_js_inline_tests.exe`` or
``dune exec ./src/tooling/run_js_inline_tests.exe SOME_PATH``.

Adding tests
------------

Alcotest
~~~~~~~~

Alcotest tests are compatible with Js_of_ocaml.  In order to run
alcotest tests in JavaScript:

- add ``js`` to modes in the tests stanza.
- and a new rule in the dune file to execute the test with node.
- ``dune build @runtest_js``

::

   (tests
     (names mytest)
     (libraries alcotest)
     (modes native js))

   (rule
     (alias runtest_js)
     (action (run node %{dep:./mytest.bc.js})))

Inline tests
~~~~~~~~~~~~

Inline tests (e.g. ``ppx_inline_test``) are compatible with jsoo but do
not run by default (see limitation above).

In order to run inline_tests in javascript:

- make sure to have both ``(js_of_ocaml)`` and ``(inline_tests)`` in your library stanza.
- run ``dune exec ./src/tooling/run_js_inline_tests.exe``

::

    (library
      (name mylib)
      (js_of_ocaml)
      (inline_tests)
    )

JavaScript test failures
------------------------

There are plenty of reasons that can explain why a test fails when
running in JavaScript and succeed otherwise.

Here is a non exhaustive list:

- Integer (``int``) are 32bit, not 63bit.
- The stack is much smaller by default on JavaScript VMs, it's easier to stackoverflow.
- There is no general tailcall optimization. In particular, cps will not be optimized.
  Only self tail recursive and mutually tail recursive functions are usually optimized.
- Some OCaml feature/lib are not (or only partially) supported: Unix, Marshal, ...
