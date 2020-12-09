import time
import pytest
from tools.constants import PROTO_DEMO_NOOPS, PROTO_GENESIS
from client.client import Client

PARAMS = ['-p', PROTO_GENESIS]


@pytest.fixture(scope="class")
def client(sandbox):
    """One node with genesis."""
    sandbox.add_node(0)
    client = sandbox.client(0)
    yield client


def forge_block_header_data(protocol_data):
    """
    Returns a binary encoding for a dict of the form
    `{'block_header_data: string}`, as expected by the protocol.

    This corresponds to the encoding given by
    `data_encoding.(obj1 (req "block_header_data" string))`. See
    `lib_data_encoding/data_encoding.mli` for the spec.
    """
    assert len(protocol_data) == 1 and 'block_header_data' in protocol_data
    string = protocol_data['block_header_data']
    tag = '0000'
    padded_hex_len = f'{len(string):#06x}'[2:]
    return tag + padded_hex_len + bytes(string, 'utf-8').hex()


@pytest.mark.incremental
class TestProtoDemo:
    """Activate protocol demo_noops, injection some operations and bake block.

    This test relies on the fixture client which launches a single
    sandboxed node.
    """

    def test_proto_known(self, client: Client):
        res = client.list_protocols()
        assert PROTO_DEMO_NOOPS in res

    def test_first_protocol(self, client: Client):
        proto = 'PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i'
        assert client.get_protocol() == proto

    def test_activate_proto(self, client: Client):
        parameters = {}  # type: dict
        res = client.activate_protocol_json(
            PROTO_DEMO_NOOPS, parameters, key='activator', fitness='1'
        )
        assert res.block_hash

    def test_level1(self, client: Client):
        assert client.get_level(params=PARAMS) == 1

    def test_protocol_genesis(self, client: Client):
        assert client.get_protocol(params=PARAMS) == PROTO_GENESIS

    def test_manual_bake(self, client: Client):
        time.sleep(1)
        message = "hello world"

        data = {
            "protocol_data": {
                "protocol": PROTO_DEMO_NOOPS,
                "block_header_data": message,
            },
            "operations": [],
        }
        block = client.rpc(
            'post',
            '/chains/main/blocks/head/helpers/preapply/block',
            data=data,
            params=PARAMS,
        )

        protocol_data = {'block_header_data': message}
        encoded = forge_block_header_data(protocol_data)

        shell_header = block['shell_header']
        shell_header['protocol_data'] = encoded
        encoded = client.rpc(
            'post',
            '/chains/main/blocks/head/helpers/forge_block_header',
            data=shell_header,
            params=PARAMS,
        )

        inject = {'data': encoded['block'], 'operations': []}
        client.rpc('post', '/injection/block', data=inject, params=PARAMS)

    def test_level2(self, client: Client):
        head = client.rpc('get', '/chains/main/blocks/head/', params=PARAMS)
        assert head['header']['level'] == 2
