Python Execution and Testing Environment
========================================

The directory ``tests_python`` contains:

- A scripting API to write execution scenarios involving several node, bakers, endorsers,
- a system testing environment based on the `pytest  <https://docs.pytest.org/en/latest>`_ package.

Code organization
-----------------

It contains the following python packages.

- ``process`` defining utility functions for interacting with processes
- ``daemons`` defines classes to run Tezos node and daemons,
- ``client`` mainly defines the ``Client`` class, that provides a programmatic interface to a client,
- ``codec`` defines a ``Codec`` class, that provides a interface for ``tezos-codec`` binary,
- ``launcher`` defines classes used to launch a nodes and daemons with specific settings,
- ``tools`` contains utility functions and constants shared by the tests,
- ``examples`` contains example of tests and scripts that run scenarios of interactions between tezos nodes and clients,
- ``tests`` contains ``pytest`` tests,
- ``scripts`` contains utility scripts.

They are organized in four layers.

1. ``process``
2. ``daemons``, ``client`` and ``codec``,
3. ``launchers``,
4. ``tests``, ``examples``, ``tools``.

Installation
------------

Prerequisites:

- The Tezos binaries :ref:`compiled from sources <build_from_sources>`
- `python 3.9.5`. It is recommended to use `pyenv
  <https://github.com/pyenv/pyenv>`_ to manage the python versions.
  If you want to use ``pyenv``:

  * Follow the `installation instructions <https://github.com/pyenv/pyenv/#installation>`__.
    In particular, this ensures that ``eval "$(pyenv init -)"``
    has been executed first during the shell session, by adding this line to an
    environment script sourced automatically.

  * You can use then ``pyenv install 3.9.5`` followed by:

    + ``pyenv local 3.9.5`` to use ``python 3.9.5`` only in the current directory (and its subdirectories, unless redefined)
    + ``pyenv global 3.9.5`` to set the python version to ``3.9.5`` globally
    + ``pyenv shell 3.9.5`` to use ``python 3.9.5`` only in the current shell

- `poetry <https://python-poetry.org/>`_ to manage the python dependencies and
  run the tests in a sandboxed python environment. Follow the `installation instructions <https://python-poetry.org/docs/#installation>`__.

  Before running the tests for the first time, the
  dependencies must be installed. To achieve this, run ``poetry install``
  in the root of the project.

  All subsequent poetry commands are to be run in ``tests_python/``.

Summing up, a typical installation proceeds as follows::

  # 1. install poetry
  # 2. install pyenv
  # 3. restart shell, to ensure "pyenv init -" has been evaluated
  # 4. then:
  pyenv install 3.9.5
  pyenv local 3.9.5
  poetry install
  cd tests_python/

Examples of test executions:

::

    poetry run pytest examples/test_example.py  # simple test example
    poetry run pytest -m "not slow"  # run all tests not marked as slow
    poetry run pytest -s tests_alpha/test_injection.py  # run a specific test with traces
    poetry run pytest  # run all tests


A simple sandbox scenario
-------------------------

The following example runs a couple of nodes and performs
a transfer operation.

.. code-block:: python

    import time
    from tools import constants, paths, utils
    from launchers.sandbox import Sandbox


    def scenario():
        """ a private tezos network, initialized with network parameters
            and some accounts. """
        with Sandbox(paths.TEZOS_HOME,
                    constants.IDENTITIES,
                    constants.GENESIS_PK) as sandbox:
            # Launch node running protocol Alpha
            sandbox.add_node(0)
            utils.activate_alpha(sandbox.client(0))
            # Launch a second node on the same private tezos network
            sandbox.add_node(1)
            # Launch a baker associated to node 0, baking on behalf of delegate
            # bootstrap5
            sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)
            # first client tells node 0 to transfer money for an account to another
            # receipt is an object representing the client answer
            receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
            transfer_hash = receipt.operation_hash
            # Wait for second node to update its protocol to Alpha, if not
            # it may not know yet the ``wait_for_inclusion`` operation which is
            # protocol specific
            time.sleep(5)
            # second client waits for inclusion of operation by the second node
            sandbox.client(1).wait_for_inclusion(transfer_hash)


    if __name__ == "__main__":
        scenario()


This can be run with
``PYTHONPATH=./:$PYTHONPATH poetry run python examples/example.py``.
It should display all the clients commands and their results.

The ``sandbox`` object allows users to add nodes, bakers or endorsers
running in tezos sandboxed mode. Whenever a node has been added, one can
access it using a client object.

The client object is a wrapper on the ``tezos-client`` command. It runs
``tezos-client`` with "administrative" parameters, plus the parameters determined
by the  method called by the user.

For instance

::

    receipt = client.transfer(500, 'bootstrap1', 'bootstrap3')

will run something like

::

    tezos-client -base-dir /tmp/tezos-client.be22ya16 -addr 127.0.0.1 -port 18730 transfer 500 from bootstrap1 to bootstrap3

``receipt`` is an object of type ``client_output.TransferResult`` which gives
access to some data of the ``tezos-client`` output.

Alternatively, one can always construct the command manually:

::

    client_output = client.run(['transfer', '500', 'from', 'bootstrap1', 'bootstrap3'])

In that case, ``client_output`` is the string returned by the client, such as

::

    Node is bootstrapped, ready for injecting operations.
    Estimated gas: 10100 units (will add 100 for safety)
    Estimated storage: no bytes added
    Operation successfully injected in the node.
    Operation hash is 'op9K2VJjKJLaFnfQKzsoz9rzr5v1PrLjpefiPtVhuiiXYgkZes1'
    ...

The first method is more convenient and less error prone. But the second
method is more generic and sometimes the only option if the specialized method
isn't implemented.

Test suite and ``pytest``
-------------------------

Tests are located in the ``tests`` directory and rely on the ``pytest`` library.

Tests are divided into modules, and are furthermore subdivided into classes.
A class defines a full testing scenario. A typical scenario is a sequence of
client commands and assertions, operating on a set of Tezos nodes running in
a private network (a.k.a *sandbox* mode).

Running tests
~~~~~~~~~~~~~

Useful options
""""""""""""""

``pytest`` has a variety of launching options. Convenient options include:

- ``-v`` display test names,
- ``-x`` stop at first failure,
- ``-s`` display output, including commands launched and stdout from client
  (by default, pytest captures all *passing* test output and show failed tests
  output),
- ``--tb=short``, ``--tb=long``, ``--tb=no``, set size of python trace back in case of failure. Default is ``long`` and is too verbose in most case. The python trace back is useful to detect bugs in the python scripts,
- ``--log-dir=<dir>`` saves all servers log in the given dir (CREATE ``<DIR>`` FIRST).
- ``-x --pdb``, start python debugger at first failure, this allows interacting with the node in the same context of the test,
- ``-m TAGS_EXPR``, run all tests containing some combination of tags.

``-v`` and ``--tb=short`` are set by default in ``pytest`` initialization file.

Tags
""""

Tests can be classified with tags. Tags are added with the annotation

.. code-block:: python

    @pytest.mark.TAG

The configuration file ``pytest.ini`` defines the list of allowed tags.
It includes ``vote``, ``multinode``, ``baker``, ``endorser``, ``contract``, ``slow``.

Examples
""""""""

There are typically two ways of using ``pytest``:

- run a subset of the tests (batch mode),
- or run a specific test.

In batch mode, we usually don't care about traces. No particular option is
needed, but sometimes we want to stop at first failure using ``-x``, and some
tests require the server logs to be saved (``--log-dir=tmp/``) as they check some assertions in the
logs at some point in the test.

To run a specific test, we usually want client and server traces
(``-s --log-dir=tmp/``).

::

    # Launch a simple test without capturing stdout
    > poetry run pytest -s examples/test_example.py
    # run all tests about vote
    > poetry run pytest -m "vote"
    # run all vote and non-slow tests
    > poetry run pytest -m "vote and not slow"
    # run module test_voting.py, display all output, save server logs in tmp
    > poetry run pytest -s tests/test_voting.py --log-dir=tmp
    # run all tests using a daemon
    > poetry run pytest -m "endorser or baker"
    # run everything
    > poetry run pytest

Pre-commit hook
"""""""""""""""

The `pre-commit <https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks>`_
hook located in ``scripts/pre_commit/pre_commit.py``
executes modified python tests automatically. It looks for staged files
(the default) or modified files (if ``--unstaged`` is passed) in
``tests_python/tests`` and calls ``pytest`` on those files. This avoids
pushing commits that will break the CI. It is also handy to execute
the relevant subset of tests by calling
``./scripts/pre_commit/pre_commit.py [--unstaged]`` manually.

We refer to the header of ``pre_commit.py`` and its ``--help`` flag
for additional instructions.

Anatomy of a test
~~~~~~~~~~~~~~~~~

A typical testing scenario consists in:

 1. initializing the context (starting servers, setting up clients)
 2. running a sequence of commands and assertions
 3. releasing resources, terminating servers

This is done by grouping tests in a class, and managing the context in
a *fixture*.

The following ``test_example.py`` is the ``pytest`` counterpart of the first example.

.. code-block:: python

    import pytest
    from tools import constants, paths, utils
    from launchers.sandbox import Sandbox


    @pytest.fixture(scope="class")
    def sandbox():
        """Example of sandbox fixture."""
        with Sandbox(paths.TEZOS_HOME,
                     constants.IDENTITIES,
                     constants.GENESIS_PK) as sandbox:
            sandbox.add_node(0, params=constants.NODE_PARAMS)
            utils.activate_alpha(sandbox.client(0))
            sandbox.add_node(1, params=constants.NODE_PARAMS)
            sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)
            yield sandbox
            assert sandbox.are_daemons_alive()


    @pytest.fixture(scope="class")
    def session():
        """Example of dictionary fixture. Used for keeping data between tests."""
        yield {}


    @pytest.mark.incremental
    class TestExample:

        def test_wait_sync_proto(self, sandbox, session):
            session['head_hash'] = sandbox.client(0).get_head()['hash']
            clients = sandbox.all_clients()
            for client in clients:
                proto = constants.ALPHA
                assert utils.check_protocol(client, proto)

        def test_transfer(self, sandbox, session):
            receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
            session['operation_hash'] = receipt.operation_hash

        @pytest.mark.timeout(5)
        def test_inclusion(self, sandbox, session):
            operation_hash = session['operation_hash']
            sandbox.client(0).wait_for_inclusion(operation_hash,
                                                 branch=session['head_hash'])

In this example, we defined the fixtures in the same module, but they are
generally shared between tests and put in ``conftest.py``.

Currently, all tests scenarios in the test suite are defined as classes,
consisting of a sequence of methods that are run incrementally (as
specified with the annotation ``@pytest.mark.incremental``). Classes are
used to define the scope of a fixture, and a unit of incremental
testing sequence. We don't directly instantiate them, or use ``self``.

Data between methods are shared using a dictionary ``session``. For instance,
we save the result of the ``transfer`` operation, and retrieve it in the next
method.

Fixtures
~~~~~~~~

The list of fixtures available is given by

::

    poetry run pytest --fixtures

Most fixtures are defined in ``conftest.py``.
The most general fixture is ``sandbox``. It allows to instantiate an arbitrary
number of nodes and daemons. Other fixtures, such as ``client``,
are specialized versions (slightly more convenient than using
``sandbox`` directly). Fixtures can be defined directly in a module defining a
test, or they can be shared.

Skipping tests
~~~~~~~~~~~~~~

Sometimes, a test can't be run. For instance, it is known to fail, or it
relies on some resources that may not be available. In that case, the test
can be skipped (instead of failing).

For instance, if no log dir has been specified, the `test_check_logs` tests are
skipped using ``pytest.skip()``.

::

    def test_check_logs(self, sandbox):
            if not sandbox.log_dir:
                pytest.skip()

Alternatively, one can use the ``skip`` annotation:

::

    @pytest.mark.skip(reason="Not yet implemented")

Adding a test
~~~~~~~~~~~~~

- By imitation, choose an existing test that looks similar,
- use the proper tags,
- say briefly what the test is supposed to test in the class docstring,
- *Run the linters* and typechecker ``make lint``, and ``make typecheck``
  in ``tests_python/``, or simple ``make check-python-linting`` from the Tezos home
  directory. Note that linting and typechecking are enforced by the CI
  in the build stage.
- If you modify the API (launchers or daemons), make sure you maintain the
  layers structure. API shouldn't rely testing constants (``tools/constant.py``
  or ``tools/paths.py``).

Testing on a production branch (``zeronet``, ``mainnet``,...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On ``master``, protocol Alpha is named
``ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK``, and daemons binary
name are suffixed with ``alpha`` (``tezos-baker-alpha``,
``tezos-accuser-alpha``...). However, on *production* branches, an actual
hash of the protocol is used, and a shortened string is used to specify
daemons.

For instance, on revision ``816625bed0983f7201e4c369440a910f006beb1a`` of
zeronet, protocol Alpha is named
``PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP`` and daemons are
suffixed by ``003-PsddFKi3`` (``tezos-baker-003-PsddFKi3``).

To reduce coupling between tests and the actual branch to be tested, tests
refer to protocol Alpha using ``constants.ALPHA`` and
``constants.ALPHA_DAEMON`` rather than by hard-coded identifiers.

.. _pytest_regression_testing:

Regression testing
------------------

Some tests in the test suite are regression tests.
Regression testing is a coarse-grained testing method for detecting
unintended changes in the system under test.
In addition to standard assertions, a regression
test compares the "output" of the test to a stored test log. The
regression test fails if the output and the stored test log do not
match. We apply regression testing using the `pytest-regtest
<https://gitlab.com/uweschmitt/pytest-regtest>`_ plugin.

To simplify the writing of regression tests, we provide a
specialized version of the ``client`` fixture, ``client_regtest``. It
registers all output of the ``tezos-client``.

Output conversion
~~~~~~~~~~~~~~~~~

The output of the client might differ slightly from one test run to
another, for instance due to timestamps. A specialized fixture
``client_regtest_scrubbed`` applies a series of conversions to the
output. For example, a timestamp such as ``2019-09-23T10:59:00Z`` is
replaced by ``[TIMESTAMP]``. These conversions are defined in the function
``client_output_converter`` of ``conftest.py``.


Running regression tests
~~~~~~~~~~~~~~~~~~~~~~~~

Regression tests are run during normal tests runs.

Updating regression tests
~~~~~~~~~~~~~~~~~~~~~~~~~

The test logs are stored in ``tests_python/tests/_regtest_outputs/``.
If the logs need to be updated, pass ``--regtest-reset`` to ``pytest``:

::

    poetry run pytest --regtest-reset <test-file>

The resulting changes should be committed after thoroughly verifying
that they are as expected.

Writing regression tests
~~~~~~~~~~~~~~~~~~~~~~~~

To write regression tests targeting the ``tezos-client``, write a test
as usual, but request the ``client_regtest`` (or
``client_regtest_scrubbed`` to enable output conversion) fixture
instead of the ``client`` fixture.

In this example test, we test the output of the `hash data` command of
`tezos-client`:

.. code-block:: python

    class TestDemonstrateRegtest:
        """Tests demonstrating regression testing."""

        def test_hash_regtest(self, client_regtest):
            assert client_regtest.hash('(Pair 1 "foo")', '(pair nat string)').blake2b == \
                "Hadaf2hW4QwbgTdhtAfFTofrCbmnnPhkGy2Sa5ZneUDs"


Before running the test we must generate the test log, that contains
the expected output.  This is done by passing the `--regtest-reset`
flag as described above:

.. code-block:: bash

    $ poetry run pytest --regtest-reset tests_python/tests/test_regtest.py

We find the generated test log in ``tests_python/tests/_regtest_outputs/test_regtest.TestDemonstrateRegtest\:\:test_hash_regtest.out``:

.. code-block:: bash

    $ cat tests_python/tests/_regtest_outputs/test_regtest.TestDemonstrateRegtest\:\:test_hash_regtest.out
    Raw packed data: 0x05070700010100000003666f6f
    Script-expression-ID-Hash: exprvPNUJQXpct6VrbJQCazrDgh7pN8d8SH8P1UFHMrRPmQnxC16nr
    Raw Script-expression-ID-Hash: 0xf65884dadd3a5ff1a6f8057fa442a2e8ecdbe1217f7759512509b36c016c5bce
    Ledger Blake2b hash: Hadaf2hW4QwbgTdhtAfFTofrCbmnnPhkGy2Sa5ZneUDs
    Raw Sha256 hash: 0xb01925b6b6180a31a17f74d92ac87e551ab08e1890211741abde5345b38cb61f
    Raw Sha512 hash: 0x75547d33aca115154e5a0ec22e965237ec3c32a81b64f827668bbef3b3310d8c237ae06211ee63edf743fcf0a98a970bb159782c6b75fac42d6efc20b3fa5e82
    Gas remaining: 799862 units remaining

This is exactly the output of the command that was executed by the
test, namely ``tezos-client hash data '(Pair 1 "foo")' of type '(pair
nat string)'``.

As discussed below in the section :ref:`Pitfalls to regression testing
<pitfalls_to_regression_testing>`, regression tests cannot be put in a test
class where the normal ``client`` fixture is used.

For other aspects of regression testing, we refer to the
`pytest-regtest documentation
<https://gitlab.com/uweschmitt/pytest-regtest>`_.


.. _pitfalls_to_regression_testing:

Typechecking python code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We also enforce the types on the python codebase. We use `mypy`, a typechecker for python.
Code can be typechecked using the Makefile target `make typecheck`. It is also
enforced in the CI with the job `check_python_types`.


Pitfalls to regression testing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``client`` and the ``client_regtest`` fixtures cannot be used in the same
test class.  If they are, then two nodes will be added to the
sandbox. Their interference might cause unintended consequence
disturbing the tests.

TODO
----

There are few simple possible improvements.

- Many ``client`` methods and ``client_output`` classes haven't been
  implemented yet,
- Be more consistent in the use of retries, timeout, to make tests less
  sensitive on timing assumption,
- Implement new launchers (i.e. zeronet),
- Use parametric fixtures more consistently: one can relaunch the same tests,
  with different parameters such as the number of peers,
- Finish porting bash scripts,

Known issues
------------

- On rare occasions, some servers may not be properly killed upon test
  termination,

- One some occasions, the ``timeout`` marker doesn't play well with
  blocking client commands. for instance, this may not stop the test if
  ``wait_for_inclusion`` is stuck.

::

    @pytest.mark.timeout(5)
    def test_inclusion(self, sandbox, session):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(operation_hash)

The ``thread`` methods terminates the test but the resources aren't properly
cleaned up.

::

    @pytest.mark.timeout(5, method='thread')

See discussion `here <https://pypi.org/project/pytest-timeout/>`__.

To avoid this issue, one can use polling functions
such as ``utils.check_contains_operations(client, [op_hash])``
instead of using blocking commands.

.. _python_adding_new_dependencies:

Adding new dependencies
-----------------------

Dependencies are managed by poetry in the file :src:`pyproject.toml`.
See `the reference for the pyproject.toml files <https://python-poetry.org/docs/pyproject/>`__.
The file :src:`poetry.lock` is generated by running ``poetry lock``, and must never be changed manually.
The resulting ``poetry.lock`` and its generator ``pyproject.toml`` must be
copied in `this repository <https://gitlab.com/tezos/opam-repository>`__.
