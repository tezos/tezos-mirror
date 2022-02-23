"""Hooks and fixtures.

A fixture defines code to be run before and after a (sequence of) test,
E.g. start and stop a server. The fixture is simply specified as a parameter
in the test function, and the yielded values is then accessible with this
parameter.
"""
import os
from typing import Optional, Iterator
import pytest

# Should only be used for type hints
import _pytest

from pytest_regtest import (
    register_converter_pre,
    deregister_converter_pre,
    _std_conversion,
)
from launchers.sandbox import Sandbox
from tools import constants, paths, utils
from tools.client_regression import ClientRegression
from tools.utils import bake
from client.client import Client

pytest_plugins = ("pytest_plugins.job_selection",)


@pytest.fixture(scope="session", autouse=True)
def sanity_check(request) -> None:
    """Sanity checks before running the tests."""
    log_dir = request.config.getoption("--log-dir")
    if not (log_dir is None or os.path.isdir(log_dir)):
        pytest.exit(f"{log_dir} doesn't exist")


@pytest.fixture(scope="session")
def log_dir(request) -> str:
    """Retrieve user-provided logging directory on the command line."""
    return request.config.getoption("--log-dir")


@pytest.fixture(scope="session")
def singleprocess(request) -> Iterator[bool]:
    """Retrieve user-provided single process mode on the command line."""
    yield request.config.getoption("--singleprocess")


@pytest.fixture(scope="class")
def session() -> Iterator[dict]:
    """Dictionary to store data between tests."""
    yield {}


def pytest_runtest_makereport(item, call) -> None:
    # hook for incremental test
    # from https://docs.pytest.org/en/latest/example/simple.html
    if "incremental" in item.keywords:
        if call.excinfo is not None:
            parent = item.parent
            # TODO can we do without this hack?
            parent._previousfailed = item  # pylint: disable=protected-access


def pytest_runtest_setup(item) -> None:
    if "incremental" in item.keywords:
        previousfailed = getattr(item.parent, "_previousfailed", None)
        if previousfailed is not None:
            pytest.xfail("previous test failed (%s)" % previousfailed.name)


DEAD_DAEMONS_WARN = '''
It seems some daemons terminated unexpectedly, or didn't launch properly.
You can investigate daemon logs by running this test using the
`--log-dir=LOG_DIR` option.'''


@pytest.fixture(scope="class")
def sandbox(log_dir: Optional[str], singleprocess: bool) -> Iterator[Sandbox]:
    """Sandboxed network of nodes.

    Nodes, bakers and endorsers are added/removed dynamically."""
    # log_dir is None if not provided on command-line
    # singleprocess is false if not provided on command-line
    with Sandbox(
        paths.TEZOS_HOME,
        constants.IDENTITIES,
        log_dir=log_dir,
        singleprocess=singleprocess,
    ) as sandbox:
        yield sandbox
        assert sandbox.are_daemons_alive(), DEAD_DAEMONS_WARN


@pytest.fixture(scope="function")
def client_regtest(
    client_regtest_bis: ClientRegression, regtest
) -> Iterator[Client]:
    """The client for one node with protocol alpha, with a function level
    regression test fixture."""
    deregister_converter_pre(_std_conversion)
    client_regtest_bis.set_regtest(regtest)
    register_converter_pre(utils.client_always_output_converter)
    yield client_regtest_bis
    deregister_converter_pre(utils.client_always_output_converter)


@pytest.fixture(scope="function")
def client_regtest_scrubbed(
    client_regtest: ClientRegression,
) -> Iterator[Client]:
    """One node with protocol alpha, regression test and scrubbing enabled."""
    register_converter_pre(utils.client_output_converter)
    yield client_regtest
    deregister_converter_pre(utils.client_output_converter)


@pytest.fixture(scope="function")
def client_regtest_custom_scrubber(
    client_regtest: ClientRegression, request
) -> Iterator[Client]:
    """One node with protocol alpha, regression test and custom scrubbing.

    The custom scrubbing is configured by in direct parameterization. For
    example, to replace all `foo` with `bar` and `baz` to `gaz`:

    @pytest.mark.parametrize('client_regtest_custom_scrubber', [
        [(r'foo', 'bar'), (r'baz', 'gaz')]
    ], indirect=True)"""

    def scrubber(string):
        print(request.param)
        return utils.suball(request.param, string)

    register_converter_pre(scrubber)
    yield client_regtest
    deregister_converter_pre(scrubber)


def pytest_collection_modifyitems(config, items):
    """Adapted from pytest-fixture-marker: adds the regression marker
    to all tests that use the regtest fixture.
    """
    # pylint: disable=unused-argument

    for item in items:
        if 'regtest' in item.fixturenames:
            item.add_marker('regression')


def _wrap_path(binary: str) -> str:
    res = os.path.join(paths.TEZOS_HOME, binary)
    assert os.path.isfile(res), f'{res} is not a file'
    return res


CLIENT = 'tezos-client'
CLIENT_ADMIN = 'tezos-admin-client'


@pytest.fixture(scope="class")
def nodeless_client():
    """
    A client that is suited for being used in tests that do not
    require a node
    """
    client_path = _wrap_path(CLIENT)
    client_admin_path = _wrap_path(CLIENT_ADMIN)
    client = Client(client_path, client_admin_path, endpoint=None)
    yield client
    client.cleanup()


@pytest.fixture(scope="class")
def encrypted_account_with_tez(client: Client):
    """
    Import an encrypted account with some tez
    """
    client.import_secret_key(
        "encrypted_account",
        (
            "encrypted:edesk1n2uGpPtVaeyhWkZzTEcaPRzkQHrqkw5pk8VkZv"
            "p3rM5KSc3mYNH5cJEuNcfB91B3G3JakKzfLQSmrgF4ht"
        ),
        "password",
    )
    client.transfer(
        100, "bootstrap1", "encrypted_account", ["--burn-cap", "1.0"]
    )
    bake(client, bake_for="bootstrap1")


def pytest_addoption(parser: _pytest.config.argparsing.Parser) -> None:
    parser.addoption("--log-dir", action="store", help="specify log directory")
    parser.addoption(
        "--singleprocess",
        action='store_true',
        default=False,
        help="the node validates blocks using only one process,\
            useful for debugging",
    )
