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

    def test_wait_sync_proto(self, sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            proto = constants.ALPHA
            assert utils.check_protocol(client, proto)

    def test_transfer(self, sandbox, session):
        receipt = sandbox.client(0).transfer(500, 'bootstrap1', 'bootstrap3')
        session['operation_hash'] = receipt.operation_hash

    # TODO The next test fails due to a bug. It runs
    #
    # tezos-client wait for ooA4Gaa1xnT7DT6C42acy4b6NsvjJfzVYyX5CaD6vft9Y4p9ktu
    # to be included --check-previous 2
    #
    # Which calls the RPC
    # /chains/main/blocks/BLjJu....SftfYu7pDGTZx~3/hash
    #
    # It fails with `Did not find service` if the previous block
    # HEAD~3 doesn't exist yet which happens if the chain is too short.
    #
    @pytest.mark.skip
    @pytest.mark.timeout(5)
    def test_inclusion(self, sandbox, session):
        operation_hash = session['operation_hash']
        sandbox.client(0).wait_for_inclusion(operation_hash, check_previous=2)
