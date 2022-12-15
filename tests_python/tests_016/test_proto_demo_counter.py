import time
import pytest
from tools import constants
from tools.constants import PROTO_DEMO_COUNTER, PROTO_GENESIS
from client.client import Client

PARAMS = ['-p', PROTO_GENESIS]


@pytest.fixture(scope="class")
def client(sandbox):
    """One node with genesis."""
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    client = sandbox.client(0)
    yield client


@pytest.mark.incremental
class TestProtoDemo:
    """Activate protocol demo_counter, inject operations and bake block.

    This test relies on the fixture client which launches a single
    sandboxed node.
    """

    def test_proto_known(self, client: Client):
        res = client.list_protocols()
        assert PROTO_DEMO_COUNTER in res

    def test_proto_client_known(self, client: Client):
        res = client.list_understood_protocols()
        assert PROTO_DEMO_COUNTER[:12] in res

    def test_first_protocol(self, client: Client):
        proto = 'PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i'
        assert client.get_protocol() == proto

    def test_activate_proto(self, client: Client):
        parameters = {'init_a': 100, 'init_b': 100}
        res = client.activate_protocol_json(
            PROTO_DEMO_COUNTER, parameters, key='activator', fitness='1'
        )
        assert res.block_hash

    def test_level1(self, client: Client):
        assert client.get_level() == 1

    def test_protocol_genesis(self, client: Client):
        assert client.get_protocol() == PROTO_GENESIS

    def test_bake_command(self, client: Client):
        time.sleep(1)
        client.run(['bake', 'This is block 2'])

    def test_level2(self, client: Client):
        head = client.rpc('get', '/chains/main/blocks/head/')
        assert head['header']['level'] == 2

    def test_inject_operations(self, client: Client):
        client.run(['increment', 'a'])
        client.run(['increment', 'b'])
        client.run(['transfer', '10'])

    def test_mempool(self, client: Client):
        ops = client.get_mempool()
        assert len(ops['applied']) == 3

    def test_bake_command_2(self, client: Client):
        time.sleep(1)
        client.run(['bake', 'This is block 3'])

    def test_level3(self, client: Client):
        head = client.rpc('get', '/chains/main/blocks/head/')
        assert head['header']['level'] == 3

    def test_rpc_counter_a(self, client: Client):
        head = client.rpc('get', '/chains/main/blocks/head/counter/a')
        assert head == 91

    def test_rpc_counter_b(self, client: Client):
        head = client.rpc('get', '/chains/main/blocks/head/counter/b')
        assert head == 111

    def test_get_counter_commands(self, client: Client):
        message_a = client.run(['get', 'a'])
        assert message_a == "The counter value is 91\n"
        message_b = client.run(['get', 'b'])
        assert message_b == "The counter value is 111\n"
