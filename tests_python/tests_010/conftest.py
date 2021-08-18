"""Hooks and fixtures.

A fixture defines code to be run before and after a (sequence of) test,
E.g. start and stop a server. The fixture is simply specified as a parameter
in the test function, and the yielded values is then accessible with this
parameter.
"""
import subprocess
import shutil
import os
import tempfile
from typing import Optional, Iterator, List
import pytest
from pytest_regtest import (
    register_converter_pre,
    deregister_converter_pre,
    _std_conversion,
)
from launchers.sandbox import Sandbox
from tools import constants, paths, utils
from tools.client_regression import ClientRegression
from client.client import Client
from client.client_output import CreateMockupResult

from . import protocol


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


@pytest.fixture(scope="class")
def client(sandbox: Sandbox) -> Iterator[Client]:
    """One node with protocol 010.

    Activate protocol 010 one year in the past. This avoids waiting
    when baking blocks manually from the client using `bake for`
    """
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    client = sandbox.client(0)
    protocol.activate(client, activate_in_the_past=True)
    yield client


@pytest.fixture(scope="class")
def client_regtest_bis(sandbox: Sandbox) -> Iterator[Client]:
    """One node with protocol 010, regression test enabled.

    Activate protocol 010 one year in the past. (see fixture client).
    """

    def reg_client_factory(
        client_path: str,
        admin_client_path: str,
        host: Optional[str] = None,
        base_dir: Optional[str] = None,
        rpc_port: Optional[int] = None,
        use_tls: Optional[bool] = None,
        endpoint: Optional[str] = 'http://127.0.0.1:8732',
        mode: str = None,
        disable_disclaimer: bool = True,
    ) -> ClientRegression:
        client = ClientRegression(
            client_path=client_path,
            admin_client_path=admin_client_path,
            host=host,
            base_dir=base_dir,
            rpc_port=rpc_port,
            use_tls=use_tls,
            endpoint=endpoint,
            mode=mode,
            disable_disclaimer=disable_disclaimer,
        )
        return client

    sandbox.add_node(
        1, client_factory=reg_client_factory, params=constants.NODE_PARAMS
    )
    client = sandbox.client(1)
    protocol.activate(client, activate_in_the_past=True)
    yield client


@pytest.fixture(scope="function")
def client_regtest(
    client_regtest_bis: ClientRegression, regtest
) -> Iterator[Client]:
    """The client for one node with protocol 010, with a function level
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
    """One node with protocol 010, regression test and scrubbing enabled."""
    register_converter_pre(utils.client_output_converter)
    yield client_regtest
    deregister_converter_pre(utils.client_output_converter)


@pytest.fixture(scope="class")
def clients(sandbox: Sandbox, request) -> Iterator[List[Client]]:
    """N node with protocol 010. Parameterized by the number of nodes.

    Number of nodes is specified as a class annotation.
    @pytest.mark.parametrize('clients', [N], indirect=True)

    Activate protocol 010 one year in the past. (see fixture client).
    """
    assert request.param is not None
    num_nodes = request.param
    for i in range(num_nodes):
        # Large number may increases peers connection time
        sandbox.add_node(i, params=constants.NODE_PARAMS)
    protocol.activate(sandbox.client(0), activate_in_the_past=True)
    clients = sandbox.all_clients()
    for client in clients:
        proto = protocol.HASH
        assert utils.check_protocol(client, proto)
    yield clients


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


@pytest.fixture
def mockup_client(sandbox: Sandbox) -> Iterator[Client]:
    """
    Returns a mockup client with its persistent directory created

    This is done in two steps, because we want to create the mockup
    with a client that doesn't have "--mode mockup" (as per
    the public documentation) but we want to return a
    client that has "--mode mockup" and uses the base-dir created
    in the first step.

    There is no way around this pattern. If you want to create
    a mockup using custom arguments; you MUST do the same
    as this method.
    """
    with tempfile.TemporaryDirectory(prefix='tezos-client.') as base_dir:
        unmanaged_client = sandbox.create_client(base_dir=base_dir)
        res = unmanaged_client.create_mockup(
            protocol=protocol.HASH
        ).create_mockup_result
        assert res == CreateMockupResult.OK
        yield sandbox.create_client(base_dir=base_dir, mode="mockup")


@pytest.fixture(scope="class")
def legacy_stores(request):
    """Aims to generate legacy stores.

    The number of blocks to bake (batch), the home path and the
    export_snapshots variables are pecified as a class annotation.
    @pytest.mark.parametrize('legacy_stores', […], indirect=True)
    """
    assert request.param is not None
    home = request.param['home']
    batch = request.param['batch']
    export_snapshots = request.param['snapshot']
    session = {}
    data_dir = tempfile.mkdtemp(prefix='tezos-legacy-stores.')
    build_dir = '_build/'
    builder_target = 'legacy_store_builder'
    builder_path = f'src/lib_store/legacy_store/{builder_target}.exe'
    builder_bin = f'{build_dir}default/{builder_path}'
    maker_target = 'legacy_store_maker'
    maker_path = f'src/lib_store/test/{maker_target}.exe'
    maker_bin = f'{build_dir}default/{maker_path}'
    subprocess.run(
        [
            'dune',
            'build',
            '--build-dir',
            f'{home}{build_dir}',
            f'{builder_path}',
        ],
        check=True,
        cwd=home,
    )
    subprocess.run(
        ['dune', 'build', '--build-dir', f'{home}{build_dir}', f'{maker_path}'],
        check=True,
        cwd=home,
    )
    # Call the magic binary which generates legacy stores such as:
    # data_dir/archive_store_to_upgrade
    #         /full_store_to_upgrade
    #         /rolling_store_to_upgrade
    # where every store contains the "same chain"
    subprocess.run(
        [
            maker_bin,
            data_dir,
            builder_bin,
            str(batch),
            str(export_snapshots).lower(),
        ],
        check=True,
        cwd=home,
    )

    # Store data paths in session
    for history_mode in ['archive', 'full', 'rolling']:
        path = f'{data_dir}/{history_mode}_store_to_upgrade'
        session[f'{history_mode}_path'] = path

    # Store snapshot paths in legacy_stores
    if export_snapshots:
        for history_mode in ['archive', 'full']:
            full_path = f'{data_dir}/snapshot_from_{history_mode}_storage.full'
            session[f'from_{history_mode}.full'] = full_path
            rolling_path = (
                f'{data_dir}' + f'/snapshot_from_{history_mode}_storage.rolling'
            )
            session[f'from_{history_mode}.rolling'] = rolling_path
        # Store the rolling path
        session['from_rolling.rolling'] = (
            f'{data_dir}/snapshot_from' + '_rolling_storage.rolling'
        )

    yield session
    shutil.rmtree(data_dir)
