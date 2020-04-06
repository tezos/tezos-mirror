Overview of Testing in Tezos
============================

Testing is important to ensure the quality of the Tezos codebase by
detecting bugs and ensuring the absence of regressions. Tezos and its
components use a variety of tools and framework for testing. The goal
of this document is to give an overview on how testing is done in
Tezos, and to help Tezos contributors use the test suite and
write tests by pointing them towards the most
appropriate testing framework for their use case. Finally, this guide
explains how tests can be :ref:`run automatically in the Tezos CI
<gitlab_test_ci>` and how to :ref:`measure test coverage
<measuring-test-coverage>`.

The frameworks used in Tezos can be categorized along two axes: the
type of component they test, and the type of test they perform. We
distinguish the following components:

 - Node

   - Protocol

     - Michelson interpreter
     - Stitching

 - Networked nodes
 - Client
 - Ledger application
 - Endorser
 - Baker

Secondly, these components can be tested at different levels of
granularity. Additionally, tests can verify functionality, but also
other non-functional properties such as performance (execution time, memory and disk
usage). We distinguish:

Unit testing
   Unit testing tests software units, typically functions, in isolation.
Integration testing
   Integration testing tests compositions of smaller units.
System testing
   System testing tests the final binaries directly.
Regression testing
   In general, regression testing aims to detect the re-introduction
   of previously identified bugs. It can also refer to a
   coarse-grained type of testing where the output of a test execution
   is compared to a pre-recorded log of expected output. Tezos uses
   tests that bugs are not re-introduced, but in this document we use
   regression testing to refer to the second meaning.
Property testing / Fuzzing
   Both property testing and fuzzing test
   code with automatically generated inputs. Property testing is
   typically used to ensure functional correctness, and gives the user
   more control over generated input and the expected output. Fuzzing
   is typically used to search for security weaknesses and often guides
   input generation with the goal of increasing test coverage.
Performance testing
   Testing of non-functional aspects such as run-time, memory and disk
   usage.
Acceptance testing
   Testing of the software in real conditions. It is usually slower,
   more costly and less amenable to automation than integration or
   system testing. It is often the final step in the testing process
   and is performed before a release. In Tezos, acceptance testing is
   done by running a test net.

..
   Inline testing
      Inline testing refers to a fine-grained type of testing, where
      tests are interleaved with the tested code. The inline tests are
      run when the tested code is executed, and typically removed in
      production builds.


We obtain the following matrix. Each cell contains the frameworks
appropriate for the corresponding component and testing type. The frameworks
are linked to a sub-section of this page where the framework is presented
in more detail, with pointers to more details.

                    ..
                       MT: :ref:`Michelson unit tests <michelson_unit_tests>`.


.. csv-table:: Testing frameworks and their applications in Tezos. PT:
               :ref:`Python testing and execution framework <pytest_section>`, AT: :ref:`alcotest_section`, CB: :ref:`crowbar_test`, FT: :ref:`flextesa`,
   :header: "Component","Unit","Property","Integration","System","Regression"

   "Node",":ref:`AT <alcotest_section>`",":ref:`CB <crowbar_test>`",":ref:`AT <alcotest_section>`",":ref:`PT <pytest_section>`, :ref:`FT <flextesa>`"
   "-- Protocol",":ref:`AT <alcotest_section>`","",""
   "-- -- Michelson interpreter",":ref:`AT <alcotest_section>`","","",":ref:`PT <pytest_section>`",":ref:`PT <pytest_section>`"
   "Client","","","",":ref:`PT <pytest_section>`, :ref:`FT <flextesa>`"
   "Networked nodes","--","",":ref:`PT <pytest_section>`, :ref:`FT <flextesa>`","", ""
   "Endorser","","","",":ref:`FT <flextesa>`"
   "Baker","","","",":ref:`FT <flextesa>`"


Testing frameworks
------------------

.. _alcotest_section:

Alcotest
~~~~~~~~

`Alcotest <https://github.com/mirage/alcotest>`_ is a library for unit
and integration testing in OCaml. Alcotest is the primary tool in
Tezos for unit and integration for testing OCaml code.

Typical use cases:
 - Verifying simple input-output specifications for functions with a
   hard-coded set of input-output pairs.
 - OCaml integration tests.

Example tests:
 - Unit tests for :src:`src/lib_requester`, in :src:`src/lib_requester/test/test_requester.ml`. To
   execute them locally, run ``dune build @src/lib_requester/runtest`` in
   the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker unit:requester``.
 - Integration tests for P2P in the shell.  For instance
   :src:`src/lib_p2p/test/test_p2p_pool.ml`. This test forks a set of
   processes that exercise large parts of the P2P layer.  To execute
   it locally, run ``dune build @runtest_p2p_pool`` in the Tezos
   root. To execute all P2P tests on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker unit:p2p``.

References:
 - `Alcotest README <https://github.com/mirage/alcotest>`_.

.. _crowbar_test:

Crowbar
~~~~~~~

`Crowbar <https://github.com/stedolan/crowbar>`_ is a library for
property-based testing in OCaml. It also interfaces with `afl
<http://lcamtuf.coredump.cx/afl/>`_ to enable fuzzing.

Typical use cases:
 - Verifying input-output invariants for functions with
   randomized inputs.

Example test:
 - Crowbar is used in :opam:`data-encoding`, a Tezos component that
   has been spun off into its own opam package. For instance, :opam:`data-encoding` uses
   Crowbar to `verify that serializing and
   deserializing a value
   <https://gitlab.com/nomadic-labs/data-encoding/-/blob/master/test/test_generated.ml>`_
   results in the initial value.  To run this test, you need to
   checkout and build :opam:`data-encoding`. Then, run ``dune
   @runtest_test_generated``.

References:
 - `Crowbar README <https://github.com/stedolan/crowbar>`_

.. _pytest_section:

Python testing and execution framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Tezos project uses `pytest <http://pytest.org/>`_, a Python testing
framework, combined with :ref:`tezos-launchers <python_testing_framework>`, a Python wrapper
``tezos-node`` and ``tezos-client``, to perform integration testing
of the node, the client, networks of nodes and daemons such as the baker
and endorser.


We also use `pytest-regtest
<https://pypi.org/project/pytest-regtest/>`_, a pytest plugin that
enables regression testing.


Typical use cases:
 - Testing the commands of ``tezos-client``. This allows to test the
   full chain: from client, to node RPC to the implementation of the
   economic protocol.
 - Test networks of nodes, with daemons.
 - Detecting unintended changes in the output of a component, using
   ``pytest-regtest``.

Example tests:
 - Testing the node's script interpreter through ``tezos-client run script`` (in :src:`pytest tests_python/tests/test_contract_opcodes.py`).
   To execute it locally, run ``pytest tests_python/tests/test_contract_opcodes.py`` in
   the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker integration:contract_opcodes``.
 - Setting up networks of nodes and ensuring their connection
   (in :src:`tests_python/tests/test_p2p.py`).
   To execute it locally, run ``pytest tests_python/tests/test_p2p.py`` in
   the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker integration:p2p``.
 - Detecting unintended changes in the behavior of the Michelson
   interpreter (in
   :src:`tests_python/tests/test_contract_opcodes.py`).  To execute it
   locally, run ``pytest tests_python/tests/test_contract_opcodes.py``
   in the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker integration:contract_opcodes``.

References:
 - `Pytest Documentation <https://github.com/stedolan/crowbar>`_
 - :ref:`python_testing_framework`
 - `pytest-regtest README <https://gitlab.com/uweschmitt/pytest-regtest>`_
 - `pytest-regtest pip package <https://pypi.org/project/pytest-regtest/>`_
 - `Section in Tezos documentation on pytest-regtest <pytest_regression_testing>`_

.. _flextesa:

Flextesa
~~~~~~~~

Flextesa (Flexible Test Sandboxes) is an OCaml library for setting up
configurable and scriptable sandboxes to meet specific testing
needs. Flextesa can also be used for interactive tests. This is used,
for instance, in some tests that require the user to interact with the
Ledger application.

Typical use cases:
 - In terms of use cases, Flextesa is similar to the `Python testing
   and execution framework <pytest>`_.

Example test:
 - Testing double baking and double endorsement scenarios (in
   :src:`bin_flextesa/command_accusations.ml`)

References:
 - :ref:`Section in Tezos Developer Documentation <flexible_network_sandboxes>`
 - `Blog post introducing Flextesa
   <https://medium.com/@obsidian.systems/introducing-flextesa-robust-testing-tools-for-tezos-and-its-applications-edc1e336a209>`_
 - `GitLab repository <https://gitlab.com/tezos/flextesa>`_
 - `An example setting up a Babylon docker sandbox <https://assets.tqtezos.com/docs/setup/2-sandbox/>`_
 - `API documentation <https://tezos.gitlab.io/flextesa/lib-index.html>`_

..
   .. _michelson_unit_tests:

   Michelson unit tests
   --------------------

   The `Michelson unit test proposal
   <https://gitlab.com/tezos/tezos/-/merge_requests/1487>`__ defines a
   format for unit tests for Michelson snippets. If the proposal is eventually accepted, then these
   tests will be executable through ``tezos-client``.

   Example use cases:
    - Verifying the functional (input--output) behavior of snippets of
      Michelson instructions.
    - Conformance testing for Michelson interpreters.

   References:
    - `Merge request defining the Michelson unit test format <https://gitlab.com/tezos/tezos/-/merge_requests/1487>`_
    - `A conformance test suite for Michelson interpreter using the Michelson unit test format <https://github.com/runtimeverification/michelson-semantics/tree/master/tests/unit>`_


.. _gitlab_test_ci:

Executing tests
---------------

Executing tests locally
~~~~~~~~~~~~~~~~~~~~~~~

Whereas executing the tests through the CI, as described below, is the
standard and most convenient way of running the full test suite, it
can also be executed locally.

Flextesa and Alcotest tests are run with ``make test`` in the project root.

The Python tests are run with ``make all`` in the directory ``tests_python``.

Executing tests through the GitLab CI
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All tests are executed on all branches for each commit.  For
instances, to see the latest runs of the CI on the master branch,
visit `this page
<https://gitlab.com/tezos/tezos/-/commits/master>_`. Each commit is
annotated with a green checkmark icon if the CI passed, and a red
cross icon if not. You can click the icon for more details.

Note that the CI does not simply execute ``make test`` and ``make
all`` in the directory ``tests_python``.  Instead, it runs the tests
as a set of independent jobs, to better exploit GitLab runner
parallelism: one job per ``pytest`` test file and one job for each
OCaml package containing tests.

When adding a new test that should be run in the CI (which should be
the case for most automatic tests), you need to make sure that it is
properly specified in the :src:`.gitlab-ci.yml` file. The procedure
for doing this depends on the type of test you've added:

Python integration and regression tests
  Run ``./scripts/update_integration_test.sh`` in Tezos home. This
  will include your new test in :src:`.gitlab-ci.yml`.

Tests executed through Dune (Alcotest, Flextesa)
  Run ``./scripts/update_unit_test.sh`` in Tezos home. This will
  include your new test in :src:`.gitlab-ci.yml`.

Other
  For other types of tests, you need to manually modify the
  :src:`.gitlab-ci.yml`. Please refer to the `GitLab CI Pipeline
  Reference <https://docs.gitlab.com/ee/ci/>`_. A helpful tool for
  this task is the `CI linter <https://gitlab.com/ci/lint>`_, and ``gitlab-runner``,
  introduced in the :ref:`next section <executing_gitlab_ci_locally>`.

.. _executing_gitlab_ci_locally:

Executing the GitLab CI locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GitLab offers the ability to run jobs defined in the :src:`.gitlab-ci.yml` file on your own machine.
This is helpful to debug the CI pipeline.
For this, you need to setup ``gitlab-runner`` on your machine.
To avoid using outdated versions of the binary, it is recommended to install a
`release from the development repository <https://gitlab.com/gitlab-org/gitlab-runner/-/releases>`_.

``gitlab-runner`` works with the concept of `executor`. We recommend to use the
``docker`` executor to sandbox the environment the job will be executed in. This
supposes that you have docker installed on your machine.

For example, if you want to run the job ``check_python_linting`` which checks the Python syntax, you can use:

.. code-block:: bash

    gitlab-runner exec docker check_python_linting

Note that the first time you execute a job, it may take a long time because it
requires downloading the docker image, and ``gitlab-runner`` is not verbose on this
subject. It may be the case if the opam repository Tezos uses has been changed, requiring
the refresh of the locally cached docker image.

Local changes must be committed (but not necessarily pushed remotely)
before executing the job locally. Indeed, ``gitlab-runner`` will clone
the head of the current local branch to execute the job.

Another limitation is that only single jobs can be executed using
`gitlab-runner`. For instance, there is no direct way of executing all
jobs in the stage `test`.

.. _measuring-test-coverage:

Measuring test coverage
-----------------------

We measure `test coverage <https://en.wikipedia.org/wiki/Code_coverage>`_
with `bisect_ppx <https://github.com/aantron/bisect_ppx/>`_. This tool
is used to see which lines in the code source are actually executed when
running one or several tests. Importantly, it tells us which parts of the
code aren't tested.

We describe here how ``bisect_ppx`` can be used locally (code coverage isn't
integrated in the CI yet).

To install ``bisect_ppx``. Run the following command from the root of the
project directory:

::

    make build-dev-deps

Then set up the ``BISECT_FILE`` environment variable:

::

    export BISECT_FILE=${PWD}/_coverage_output/bisect

It tells ``bisect_ppx`` to generate coverage data in
``${PWD}/_coverage_output/bisect*.out`` files. Then,

::

    make coverage-setup

creates the required coverage data directory ``_coverage_output``. It also
checks if the ``BISECT_FILE`` environment variable is set up properly (and
remind the user how to do so if not).

The OCaml code should be instrumented in order to generate coverage data. This
has to be specified in ``dune`` files (or ``dune.inc`` for protocols)
on a per-package basis by adding the following line in the ``library``
or ``executable`` stanza.

::

    (preprocess (pps bisect_ppx))

The convenience script ``./scripts/instrument_dune_bisect.sh`` does
this automatically. For instance,

::

    ./scripts/instrument_dune_bisect.sh src/lib_p2p/dune src/proto_alpha/lib_protocol/dune.inc

enables code coverage analysis for ``lib_p2p`` and ``proto_alpha``.

Finally, compile the code using ``make``, run any number of tests, and
generate the HTML report from the coverage files using

::

    make coverage-report

The generated report is available in ``_coverage_report/index.html``. It shows
for each file, which lines have been executed at least once, by at least
one of the tests.

Clean up coverage data (output and report) with:

::

    make coverage-clean


Reset the updated ``dune`` files using ``git``. For instance:

::

    git checkout -- src/lib_p2p/dune src/proto_alpha/lib_protocol/dune.inc
