from typing import Iterator
import pytest
from tools import constants, paths, utils
from launchers.sandbox import Sandbox


@pytest.fixture(scope="class")
def sandbox() -> Iterator[Sandbox]:
    """Example of sandbox fixture."""
    with Sandbox(paths.TEZOS_HOME, constants.IDENTITIES) as sandbox:
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_node(1, params=constants.NODE_PARAMS)
        # Empty list makes everyone bake
        sandbox.add_baker(
            0,
            [],
            proto=constants.ALPHA_DAEMON,
            run_params=['--liquidity-baking-toggle-vote', 'pass'],
        )
        yield sandbox
        assert sandbox.are_daemons_alive()


@pytest.fixture(scope="class")
def session() -> Iterator[dict]:
    """Example of dictionary fixture. Used for keeping data between tests."""
    yield {}


@pytest.mark.incremental
class TestExample:
    def test_wait_sync_proto(self, sandbox: Sandbox, session: dict):
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        clients = sandbox.all_clients()
        for client in clients:
            proto = constants.ALPHA
            assert utils.check_protocol(client, proto)

    def test_transfer(self, sandbox: Sandbox, session: dict):
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        session['operation_hash'] = receipt.operation_hash

    @pytest.mark.timeout(10)
    def test_inclusion(self, sandbox: Sandbox, session: dict):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(
            operation_hash, branch=session['head_hash']
        )

    @pytest.mark.timeout(10)
    def test_inclusion_check_previous(self, sandbox: Sandbox, session: dict):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(operation_hash, check_previous=2)
