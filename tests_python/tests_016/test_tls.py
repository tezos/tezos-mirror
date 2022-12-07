import pytest
from tools import constants
from client.client import Client


@pytest.fixture(scope="class")
def client(sandbox):
    sandbox.add_node(
        0,
        use_tls=(constants.TEZOS_CRT, constants.TEZOS_KEY),
        params=constants.NODE_PARAMS,
    )
    yield sandbox.client(0)


@pytest.mark.vote
@pytest.mark.incremental
@pytest.mark.skip(reason="requires to install a custom CA")
class TestTLS:
    """Test voting protocol with manual baking, 4 blocks per voting period."""

    def test_bootstrapped(self, client: Client):
        assert client.bootstrapped()
