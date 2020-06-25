""" This file tests the proxy mode (tezos-client --mode proxy)
"""
import shutil
import time
import pytest
from client.client import Client
from tools import utils

from tools.constants import ALPHA

_BLOCK_ID = "head"
_CHAIN_ID = "main"
_GIVER = "bootstrap1"
_PROTO = ALPHA
_RPC_PATH = f'/chains/{_CHAIN_ID}/blocks/{_BLOCK_ID}'


def _create_proxy_for_node(sandbox, node) -> Client:
    """
    Args:
        sandbox (Sandbox): the sandbox
        node (Node): a node of the sandbox
    Returns:
        A proxy client that delegates to `node`. It's up to the caller
        to delete its base dir when done with it. Note that `client.py`'s
        `cleanup` method won't do it because the result is not managed
        by the sandbox.
    """
    return sandbox.instanciate_client(rpc_port=node.rpc_port, mode="proxy")


def _update_node_to_alpha(sandbox, node_idx):
    # Vanilla client (not mockup, not proxy)
    node_client = sandbox.client(node_idx)
    # Update node to protocol alpha
    # step 1
    utils.activate_protocol(node_client, _PROTO)
    time.sleep(2)
    # step 2
    node_client.bake("bootstrap1")
    metadata_json = node_client.rpc(
        verb="get", path="/chains/main/blocks/head/metadata"
    )
    assert metadata_json['protocol'] == _PROTO


@pytest.fixture
def proxy_client(sandbox):
    node_idx, client_idx = 0, 0
    sandbox.add_node(node_idx)
    node = sandbox.node(node_idx)

    _update_node_to_alpha(sandbox, node_idx)
    sandbox.rm_client(client_idx)  # We don't need the default client anymore

    result = sandbox.register_client(node_idx, node.rpc_port, mode="proxy")
    sandbox.init_client(result, node)

    return result  # Test depending on this fixture executes here


@pytest.mark.client
def test_chain_block_context_delegates(proxy_client):
    """ Test an RPC that is done locally """
    proxy_client.rpc(
        'get', f'/chains/{_CHAIN_ID}/blocks/{_BLOCK_ID}/context/delegates'
    )


@pytest.mark.client
def test_chain_blocks(proxy_client: Client):
    """ Test an RPC that is delegated to the node """
    proxy_client.rpc('get', f'/chains/{_CHAIN_ID}/blocks')


@pytest.mark.client
def test_network_self(proxy_client):
    """ Test an RPC that is delegated to the node """
    proxy_client.rpc('get', '/network/self')
