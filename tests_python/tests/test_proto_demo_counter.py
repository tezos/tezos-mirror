import time
import pytest

PROTO = 'ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT'
PROTO_GENESIS = 'ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im'
PARAMS = ['-p', PROTO_GENESIS]


@pytest.fixture(scope="class")
def client(sandbox):
    """One node with genesis."""
    sandbox.add_node(0)
    client = sandbox.client(0)
    yield client


@pytest.mark.incremental
class TestProtoDemo:
    """Activate protocol demo_counter, inject operations and bake block.

    This test relies on the fixture client which launches a single
    sandboxed node.
    """

    def test_proto_known(self, client):
        res = client.list_protocols()
        assert PROTO in res

    def test_proto_client_known(self, client):
        res = client.list_understood_protocols()
        assert 'ProtoDemoCou' in res

    def test_first_protocol(self, client):
        proto = 'PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i'
        assert client.get_protocol() == proto

    def test_activate_proto(self, client):
        parameters = {'init_a': 100, 'init_b': 100}
        res = client.activate_protocol_json(PROTO, parameters, key='activator',
                                            fitness='1')
        assert res.block_hash

    def test_level1(self, client):
        assert client.get_level() == 1

    def test_protocol_genesis(self, client):
        assert client.get_protocol() == PROTO_GENESIS

    def test_bake_command(self, client):
        time.sleep(1)
        client.run(['bake', 'This is block 2'])

    def test_level2(self, client):
        head = client.rpc('get', '/chains/main/blocks/head/')
        assert head['header']['level'] == 2

    def test_inject_operations(self, client):
        client.run(['increment', 'a'])
        client.run(['increment', 'b'])
        client.run(['transfer', '10'])

    def test_mempool(self, client):
        ops = client.get_mempool()
        assert len(ops['applied']) == 3

    def test_bake_command_2(self, client):
        time.sleep(1)
        client.run(['bake', 'This is block 3'])

    def test_level3(self, client):
        head = client.rpc('get', '/chains/main/blocks/head/')
        assert head['header']['level'] == 3

    def test_rpc_counter_a(self, client):
        head = client.rpc('get', '/chains/main/blocks/head/counter/a')
        assert head == 91

    def test_rpc_counter_b(self, client):
        head = client.rpc('get', '/chains/main/blocks/head/counter/b')
        assert head == 111

    def test_get_counter_commands(self, client):
        message_a = client.run(['get', 'a'])
        assert message_a == "The counter value is 91\n"
        message_b = client.run(['get', 'b'])
        assert message_b == "The counter value is 111\n"
