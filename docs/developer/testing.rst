Overview of Testing in Tezos
============================

The goal of this document is to give an overview on how testing is done in
Tezos, and to help Tezos contributors use the test suite and
write tests by pointing them towards the most
appropriate testing framework for their use case. Finally, this guide
explains how tests can be :ref:`run automatically in the Tezos CI
<gitlab_test_ci>` and how to :ref:`measure test coverage
<measuring-test-coverage>`.

The frameworks used in Tezos can be categorized along two axes: the
type of component they test, and the type of testing they perform. We
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
non-functional properties such as performance (execution time, memory and disk
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
   is compared to a pre-recorded log of expected output. We here use
   "regression testing" to refer to the second meaning.
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


By combining the two axes,
we obtain the following matrix. Each cell contains the frameworks
appropriate for the corresponding component and testing type. The frameworks
are linked to a sub-section of this page where the framework is presented
in more detail.

                    ..
                       MT: :ref:`Michelson unit tests <michelson_unit_tests>`.


.. csv-table:: Testing frameworks and their applications in Tezos. PT:
               :ref:`Python testing and execution framework <pytest_section>`, AT: :ref:`alcotest_section`, PBT: :ref:`property_based_test`, FT: :ref:`flextesa_section`, TZ: :ref:`tezt_section`
   :header: "Component","Unit","Property","Integration","System","Regression"

   "Node",":ref:`AT <alcotest_section>`",":ref:`PBT <property_based_test>`",":ref:`AT <alcotest_section>`",":ref:`PT <pytest_section>`, :ref:`FT <flextesa_section>`, :ref:`TZ <tezt_section>`"
   "-- Protocol",":ref:`AT <alcotest_section>`",":ref:`PBT <property_based_test>`",""
   "-- -- Michelson interpreter",":ref:`AT <alcotest_section>`","","",":ref:`PT <pytest_section>`",":ref:`PT <pytest_section>`"
   "Client","",":ref:`PBT <property_based_test>`","",":ref:`PT <pytest_section>`, :ref:`FT <flextesa_section>`, :ref:`TZ <tezt_section>`"
   "Networked nodes","--","",":ref:`PT <pytest_section>`, :ref:`FT <flextesa_section>`","", ""
   "Endorser","","","",":ref:`FT <flextesa_section>`"
   "Baker","","","",":ref:`FT <flextesa_section>`"


Testing frameworks
------------------

.. _alcotest_section:

Alcotest
~~~~~~~~

`Alcotest <https://github.com/mirage/alcotest>`_ is a library for unit
and integration testing in OCaml. Alcotest is the primary tool in
Tezos for unit and integration testing of OCaml code.

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
 - Integration tests for the P2P layer in the shell.  For instance
   :src:`src/lib_p2p/test/test_p2p_pool.ml`. This test forks a set of
   processes that exercise large parts of the P2P layer.  To execute
   it locally, run ``dune build @runtest_p2p_pool`` in the Tezos
   root. To execute the P2P tests on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker unit:p2p``. The job-name
   ``unit:p2p`` is ill-chosen, since the test is in fact an
   integration test.

References:
 - `Alcotest README <https://github.com/mirage/alcotest>`_.

.. _property_based_test:

QCheck
~~~~~~~

`QCheck <https://github.com/c-cube/qcheck>`_ is a library for
property-based testing in OCaml.

Typical use cases:
 - Verifying input-output invariants for functions with
   randomized inputs.

Example test:
 - QCheck is used in :src:`src/lib_base/test/test_time.ml` to test the `Tezos_base.Time <https://tezos.gitlab.io/api/odoc/_html/tezos-base/Tezos_base/Time/index.html>`_ module. For instance, subtracting and then adding a random amount of seconds to a random time should give back the original time: this tests that ``add`` and ``diff`` are consistent (and the inverse of each other). To run this test, you need to run ``dune exec src/lib_base/test/test_time.exe``.

References:
 - `QCheck README <https://github.com/c-cube/qcheck>`_
 - `QCheck module documentation <https://c-cube.github.io/qcheck/>`_

.. _pytest_section:

Python testing and execution framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Tezos project uses `pytest <https://docs.pytest.org/>`_, a Python testing
framework, combined with :doc:`tezos-launchers <python_testing_framework>`, a Python wrapper
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
 - Detecting unintended changes in the behavior of the node's Michelson
   interpreter (in
   :src:`tests_python/tests_alpha/test_contract_opcodes.py`).  To execute it
   locally, run ``cd tests_python && poetry run pytest tests/test_contract_opcodes.py``
   in the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker integration:contract_opcodes``.
 - Setting up networks of nodes and ensuring their connection
   (in :src:`tests_python/tests_alpha/test_p2p.py`).
   To execute it locally, run ``cd tests_python && poetry run pytest tests/test_p2p.py`` in
   the Tezos root. To execute them on :ref:`your own machine
   <executing_gitlab_ci_locally>` using the GitLab CI system, run
   ``gitlab-runner exec docker integration:p2p``.

References:
 - `Pytest Documentation <https://docs.pytest.org/en/stable/contents.html>`_
 - :doc:`python_testing_framework`
 - `pytest-regtest README <https://gitlab.com/uweschmitt/pytest-regtest>`_
 - `pytest-regtest pip package <https://pypi.org/project/pytest-regtest/>`_
 - :ref:`Section in Tezos Developer Documentation on pytest-regtest <pytest_regression_testing>`

.. _flextesa_section:

Flextesa
~~~~~~~~

Flextesa (Flexible Test Sandboxes) is an OCaml library for setting up
configurable and scriptable sandboxes to meet specific testing
needs. Flextesa can also be used for interactive tests. This is used,
for instance, in some tests that require the user to interact with the
Ledger application.

Typical use cases:
 - In terms of use cases, Flextesa is similar to the :ref:`Python testing
   and execution framework <pytest_section>`.

Example test:
 - Testing double baking, accusations and double-baking accusation
   scenarios (in :src:`src/bin_sandbox/command_accusations.ml`)

References:
 - :doc:`Section in Tezos Developer Documentation on Flextesa <flextesa>`
 - `Blog post introducing Flextesa
   <https://medium.com/@obsidian.systems/introducing-flextesa-robust-testing-tools-for-tezos-and-its-applications-edc1e336a209>`_
 - `GitLab repository <https://gitlab.com/tezos/flextesa>`_
 - `An example setting up a Babylon docker sandbox <https://assets.tqtezos.com/docs/setup/2-sandbox/>`_
 - `API documentation <https://tezos.gitlab.io/flextesa/lib-index.html>`_

.. _tezt_section:

Tezt
~~~~

:doc:`Tezt <tezt>` is a system testing framework for Tezos. It is
intended as a replacement to Flextesa and as an OCaml-based alternative
to :ref:`Python testing and execution framework
<pytest_section>`. Like the latter, Tezt is also capable of regression
testing. Tezt focuses on tests that run in the CI, although it is also
used for some manual tests (see the :src:`tezt/manual_tests`
folder). Its main strengths are summarized in its :doc:`section in the
Tezos Developer Documentation <tezt>`. Conceptually Tezt consists of a
generic framework for writing tests interacting with external
processes, and a set of Tezos-specific modules for interacting with
the Tezos binaries: the client, baker, etc.

Typical use cases:
 - In terms of use cases, Tezt is similar to the :ref:`Python testing and
   execution framework <pytest_section>` and :ref:`Flextesa
   <flextesa_section>`. It can be used by authors that prefer OCaml
   for writing system tests.

Example tests:
 - Testing baking (in :src:`tezt/tests/basic.ml`)
 - Testing double baking and double endorsement scenarios (in
   :src:`tezt/tests/double_bake.ml`). This test is a rewrite of the
   Flextesa double baking scenario mentioned above, that demonstrates
   the difference between the two frameworks.
 - Testing absence of regressions in encodings (in :src:`tezt/tests/encoding.ml`)

References:
 - :doc:`Section in Tezos Developer Documentation on Tezt <tezt>`
 - `General API documentation <http://tezos.gitlab.io/api/odoc/_html/tezt/index.html>`_
 - `Tezos-specific API documentation <http://tezos.gitlab.io/api/odoc/_html/tezt-tezos/index.html>`_

Long Tests
""""""""""

Tezt is also used for tests that are too long to run in the CI. Those
tests are run on dedicated machines and can send data points to an
InfluxDB instance to produce graphs using Grafana and/or detect
performance regressions. See :doc:`long-tezts`.

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

Executing tests locally
-----------------------

Whereas executing the tests through the CI, as described below, is the
standard and most convenient way of running the full test suite, they
can also be executed locally.

All tests can be run with ``make test`` in the project root. However, this
can take some time, and some tests are resource-intensive or require additional
configuration. Alternatively, one can run subsets of tests identified
by a specialized target ``test-*``. For instance, ``make test-unit``
runs the alcotest tests and should be quite fast. See the project
``Makefile`` for the full list of testing targets.

.. _measuring-test-coverage:

Measuring test coverage
~~~~~~~~~~~~~~~~~~~~~~~

We measure `test coverage <https://en.wikipedia.org/wiki/Code_coverage>`_
with `bisect_ppx <https://github.com/aantron/bisect_ppx/>`_. This tool
is used to see which lines in the code source are actually executed when
running one or several tests. Importantly, it tells us which parts of the
code aren't tested.

We describe here how ``bisect_ppx`` can be used locally. See below for usage
with CI.

To install ``bisect_ppx``, run the following command from the root of the
project directory:

::

    make build-dev-deps

The OCaml code should be instrumented in order to generate coverage data. This
is done by prepending

::

   COVERAGE_OPTIONS="--instrument-with bisect_ppx" BISECT_FILE=$(pwd)/_coverage_output/

to build and test commands run from the root of the project directory. For example,

::

   COVERAGE_OPTIONS="--instrument-with bisect_ppx" BISECT_FILE=$(pwd)/_coverage_output/ make
   COVERAGE_OPTIONS="--instrument-with bisect_ppx" BISECT_FILE=$(pwd)/_coverage_output/ make test-coverage

Generate the HTML report from the coverage files using

::

    make coverage-report

The generated report is available in ``_coverage_report/index.html``. It shows
for each file, which lines have been executed at least once, by at least
one of the tests.

Clean up coverage data (output and report) with:

::

    make coverage-clean


If calling ``dune`` directly, instrumentation is achieved by setting
``BISECT_FILE`` environment variable to an existing directory and
appending the flag ``--instrument-with``. Let's consider
``lib_mockup`` as an example:

::

   cd src/lib_mockup
   mkdir -p _coverage_output/
   BISECT_FILE=$(pwd)/_coverage_output/ dune build --instrument-with bisect_ppx

Now, still in the ``src/lib_mockup`` directory, run test commands:

::

   BISECT_FILE=$(pwd)/_coverage_output/ dune test -f --instrument-with bisect_ppx

In this folder, we do not have the ``make coverage-report``. However,
this target is simply a shortcut to the ``bisect-ppx-report`` binary. This
command must be run from the root of the project:

::

   cd ../../
   bisect-ppx-report html -o _coverage_report_mockup --coverage-path src/lib_mockup/_coverage_output/

The report will now be found in ``_coverage_report_mockup``.


Enabling instrumentation for new libraries and executables
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

To ensure that all libraries and executables are included in the
coverage report, the following field should be added to all ``library``
and ``executable(s)`` stanzas in all ``dune`` files, e.g.:

::

 (library
   (name ...)
   (instrumentation
     (backend bisect_ppx)))

This enables the conditional instrumentation of the compilation unit
through the ``--instrument-with bisect_ppx`` flag as described above.

Exempted from this rule are the ``dune`` files that belong to tests,
developer utilities and old protocols. In particular:

 - benchmarks, e.g. ``src/lib_shell/bench/dune``
 - bindings, e.g. ``src/lib_sapling/bindings/dune``
 - test frameworks, e.g. ``src/bin_sandbox/dune``
 - test packages, e.g. ``src/*/test/dune``
 - old protocols, e.g. ``src/proto_00*/*/*dune``
 - helper utilities, e.g.:

   - ``src/openapi/dune``, (executable name ``openapi``)
   - ``src/lib_client_base/gen/dune`` (executable name ``bip39_generator``)
   - ``src/lib_protocol_compiler/dune`` (executable name ``replace``)
   - ``src/proto_alpha/lib_parameters/dune`` (executable name ``gen``)
   - ``src/proto_010_PtGRANAD/lib_parameters/dune`` (executable name ``gen``)
   - ``src/lib_protocol_environment/s_packer/dune`` (executable name ``s_packer``)
   - ``src/lib_store/legacy_store/dune`` (executable name ``legacy_store_builder``)



Known issues
""""""""""""

1. Report generation may fail spuriously.

   ::

       $ make coverage-report
       4409 Info: found coverage files in '_coverage_output/'
       4410  *** invalid file: '_coverage_output/819770417.coverage' error: "unexpected end of file while reading magic number"

   In that case, either delete the problematic files or re-launch the tests and re-generate the report.

Executing tests through the GitLab CI
-------------------------------------

All tests are executed on all branches for each commit.  For
instances, to see the latest runs of the CI on the master branch,
visit `this page
<https://gitlab.com/tezos/tezos/-/commits/master>`_. Each commit is
annotated with a green checkmark icon if the CI passed, and a red
cross icon if not. You can click the icon for more details.

The results of the test suite on terminated pipelines is presented on
the details of the merge request page corresponding to the
pipeline's branch (if any). For more information, see the `GitLab
documentation on Unit test reports
<https://docs.gitlab.com/ee/ci/unit_test_reports.html>`__.

By default, the ``test`` of the CI runs the tests as a set of independent jobs
that cluster the tests with a varying grain. This strikes a balance between exploiting GitLab
runner parallelism while limiting the number of jobs per
pipeline. The grain used varies slightly for different types of
tests:

Python integration and regression tests
   Python tests are grouped in a number of batch jobs (chosen in ``.gitlab/ci/integration.yml``). This number is
   chosen to keep the duration of job each lower under 10 minutes on
   average, and to accommodate the addition of new protocol test
   suites.

Tezt integration and regression tests
   Tezt tests are grouped in 3 batch jobs. New tests increases the
   size of the last batch.

The OCaml package tests (Alcotest & QCheck)
   The OCaml package tests are regrouped analogously to the ``pytest``\ s:
   one job per protocol package, in addition to one job regrouping
   tests for remaining packages.

Flextesa
   Flextesa tests run in one job per test.

Adding tests to the CI
~~~~~~~~~~~~~~~~~~~~~~

When adding a new test that should be run in the CI (which should be
the case for most automatic tests), you need to make sure that it is
properly specified in the :src:`.gitlab-ci.yml` file. The procedure
for doing this depends on the type of test you've added:

Python integration and regression tests
  New Pytest tests will be included automatically in the CI.
  To rebalance the Pytest batches based on a previous pipeline,
  run (from the root of the Tezos repository):
  ``cd tests_python && poetry run ./scripts/jobs_fetch_reports.py <PROJECT_ID> <PIPELINE_ID> test-results.xml``
  setting ``<PROJECT_ID>`` to a GitLab project id (e.g. ``3836952`` or `tezos/tezos <https://gitlab.com/tezos/tezos>`_)
  and ``<PIPELINE_ID>`` to the id of a pipeline in this project for which integration tests have executed
  (e.g. `391861162 <https://gitlab.com/tezos/tezos/-/pipelines/391861162>`_).
  and then commit the resulting :src:`tests_python/test-results.xml`.

Tezt integration and regression tests
  New Tezt tests will be included automatically in the CI.
  To rebalance the Tezt batches, run (from the root of the Tezos repository):
  ``make && dune exec tezt/tests/main.exe -- --record tezt/test-results.json``

The OCaml package tests (Alcotest & QCheck)
  
  Any non-protocol tests located in a folder named ``src/**/test/`` will be
  picked up automatically by the CI. No intervention is necessary.

  Protocol tests must be added to :src:`.gitlab/ci/unittest.yml` under the
  protocol that they are testing. For example, to run a new protocol test for
  ``proto_XXX_YYYYYYYY``, add the corresponding
  ``src/proto_XXX_YYYYYYYY/lib_\*.test_proto`` to the ``unit:XXX_YYYYYYYY``
  ``make`` invocation.

Other (including Flextesa)
  For other types of tests, you need to manually modify the
  :src:`.gitlab-ci.yml`. Please refer to the `GitLab CI Pipeline
  Reference <https://docs.gitlab.com/ee/ci/>`_. A helpful tool for
  this task is the `CI Lint tool <https://docs.gitlab.com/ee/ci/lint.html>`_, and ``gitlab-runner``,
  introduced in the :ref:`next section <executing_gitlab_ci_locally>`.

Measuring test coverage in the CI
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To measure test coverage in the CI, trigger the jobs ``test_coverage[...]`` in
stage ``test_coverage`` These jobs can either be triggered manually
from the Gitlab CI web interface. These jobs build Octez with coverage
instrumentation, and then run respectively:

 - ``make test-unit``, executing all unit, integration and property-based tests
 - ``make test-python-alpha``, executing the Python system tests for
   protocol Alpha. We restrict to protocol Alpha to avoid CI timeouts.
 - ``make test-tezt-coverage``, executing the full set of Tezt tests with a
   timeout of 30 minutes per test case.

Finally the coverage reports are generated: one for
unit tests, one for the Python system tests and one for the Tezt suite.

The resulting coverage reports are stored as artifacts that can be
downloaded or browsed from the CI page upon completion of the job.

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
subject. For instance, if Tezos' opam repository has changed, requiring
a refresh of the locally cached docker image.

Local changes must be committed (but not necessarily pushed remotely)
before executing the job locally. Indeed, ``gitlab-runner`` will clone
the head of the current local branch to execute the job.

Another limitation is that only single jobs can be executed using
``gitlab-runner``. For instance, there is no direct way of executing all
jobs in the stage ``test``. However, you can run the ``test_coverage`` job
which runs most tests (alcotest and python tests) in a single job.

.. code-block:: bash

    gitlab-runner exec docker test_coverage

Conventions
-----------

Besides implementing tests, it is necessary to comment test files as
much as possible to keep a maintainable project for future
contributors. As part of this effort, we require that contributors
follow this convention:

1. For each unit test module, add a header that explains the overall
   goal of the tests in the file (i.e., tested component and nature of
   the tests). Such header must follow this template, and be added
   after license:

::

    (** Testing
        -------
        Component:    (component to test, e.g. Shell, Micheline)
        Invocation:   (command to invoke tests)
        Dependencies: (e.g., helper files, optional so this line can be removed)
        Subject:      (brief description of the test goals)
    *)

2. For each test in the unit test module, the function name shall
   start with `test_` and one must add a small doc comment that
   explains what the test actually asserts (2-4 lines are
   enough). These lines should appear at the beginning of each test
   unit function that is called by e.g. ``Alcotest_lwt.test_case``. For
   instance,

::

    (** Transfer to an unactivated account and then activate it. *)
    let test_transfer_to_unactivated_then_activate () =
    ...

3. Each file name must be prefixed by ``test_`` to preserve a uniform
   directory structure.

4. OCaml comments must be valid ``ocamldoc`` `special comments <https://ocaml.org/manual/ocamldoc.html#s:ocamldoc-comments>`_.
