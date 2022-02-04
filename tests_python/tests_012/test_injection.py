import os
from typing import List
import subprocess
import pytest
from tools import utils, paths, constants
from tools.constants import PROTO_DEMO_NOOPS, PROTO_GENESIS
from client.client import Client


@pytest.fixture(scope="class")
def clients(sandbox):
    """Launches 3 nodes in sandbox mode (genesis, doesn't activate 012)."""
    num_nodes = 3
    for i in range(num_nodes):
        sandbox.add_node(i, params=constants.NODE_PARAMS)
    yield sandbox.all_clients()


PROTO = f'{paths.TEZOS_HOME}/src/bin_client/test/proto_test_injection'
PARAMS = ['-p', PROTO_GENESIS]


pytestmark = pytest.mark.skipif(
    utils.check_static_binary(constants.COMPILER),
    reason="cannot inject with statically compiled binaries",
)


@pytest.mark.incremental
class TestInjectionAndActivation:
    """Protocol injection and activation"""

    def test_check_resources(self):
        assert os.path.isfile(constants.COMPILER)
        assert os.path.isdir(PROTO)

    def test_compute_hash(self, session: dict):
        cmd = [constants.COMPILER, '-hash-only', PROTO]
        res = subprocess.run(
            cmd, universal_newlines=True, check=True, stdout=subprocess.PIPE
        )
        proto_hash = res.stdout[:-1]
        assert len(proto_hash) == 51
        session['proto_hash'] = proto_hash

    def test_injection(self, clients: List[Client]):
        clients[0].inject_protocol(PROTO)

    def test_check_injected(self, clients: List[Client], session: dict):
        proto = session['proto_hash']
        protos = clients[0].list_protocols()
        assert proto in protos

    def test_environment_version(self, clients: List[Client], session: dict):
        proto = session['proto_hash']
        assert clients[0].environment_protocol(proto) == "V3"

    def test_activation(self, clients: List[Client], session: dict):
        proto = session['proto_hash']
        parameters = {}  # type: dict
        res = clients[0].activate_protocol_json(
            proto, parameters, key='activator', fitness='1'
        )
        assert res.block_hash

    def test_check_protocol(self, clients: List[Client], session: dict):
        proto = session['proto_hash']
        for client in clients:
            assert utils.check_protocol(client, proto, params=PARAMS)


@pytest.fixture(scope="class")
def client(sandbox):
    """One node in sandbox mode (genesis, doesn't activate 012)."""
    sandbox.add_node(0)
    client = sandbox.client(0)
    yield client


@pytest.mark.incremental
class TestActivation:
    """Protocol activation (protocol already linked to the node)"""

    def test_proto_known(self, client: Client):
        res = client.list_protocols()
        assert PROTO_DEMO_NOOPS in res

    def test_first_protocol(self, client: Client):
        proto = 'PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i'
        assert client.get_protocol() == proto

    def test_activate_demo(self, client: Client):
        proto = PROTO_DEMO_NOOPS
        parameters = {}  # type: dict
        res = client.activate_protocol_json(
            proto, parameters, key='activator', fitness='1'
        )
        assert res.block_hash

    def test_level1(self, client: Client):
        assert client.get_level(params=PARAMS) == 1

    def test_protocol_genesis(self, client: Client):
        proto = PROTO_GENESIS
        assert client.get_protocol(params=PARAMS) == proto
