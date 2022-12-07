import pytest
from daemons.node import Node
from launchers.sandbox import Sandbox
from tools import utils


@pytest.fixture(scope="class")
def node(sandbox: Sandbox):
    """Launches one node in sandbox mode (genesis)"""
    sandbox.add_node(0, params=['--cors-origin', '*'])
    yield sandbox.node(0)


class TestCors:
    def test_preflight(self, node: Node):
        origin = 'localhost'
        port = node.rpc_port
        headers = {
            'Origin': origin,
            'Access-Control-Request-Method': 'GET',
            'Access-Control-Request-Headers': 'Content-Type',
        }
        res = utils.rpc(
            origin,
            port,
            'options',
            '/chains/main/blocks/head/header/shell',
            headers=headers,
        )
        print(res.headers)
        assert res.headers["access-control-allow-origin"] == '*'
        assert res.headers["access-control-allow-methods"] == 'GET'
        assert res.headers["access-control-allow-headers"] == 'Content-Type'

    def test_request(self, node: Node):
        origin = 'localhost'
        port = node.rpc_port
        headers = {'Origin': origin, 'Content-Type': 'application/json'}
        res = utils.rpc(
            origin,
            port,
            'get',
            '/chains/main/blocks/head/header/shell',
            headers=headers,
        )
        print(res.headers)
        assert res.headers["access-control-allow-origin"] == '*'
