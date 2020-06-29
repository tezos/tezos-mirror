""" This file tests the proxy mode (tezos-client --mode proxy)
"""
import shutil
import time
from typing import Iterator, Tuple
import pytest
from client.client import Client
from tools import utils

from tools.constants import ALPHA, CARTHAGE

_OTHER_PROTO = CARTHAGE

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


@pytest.fixture
def vanilla_and_proxy_clients(sandbox) -> Iterator[Tuple[Client, Client]]:
    """
        Returns a vanilla client and a proxy client that talk to the same node
    """
    node_idx = 0
    sandbox.add_node(node_idx)
    node = sandbox.node(node_idx)

    _update_node_to_alpha(sandbox, node_idx)

    res_vanilla = sandbox.client(node_idx)  # Use the node default client
    res_proxy = _create_proxy_for_node(sandbox, node)

    try:
        yield (res_vanilla, res_proxy)
    finally:
        # When test returns, this teardown executes
        # No need to cleanup res_vanilla, it's managed by the sandbox
        shutil.rmtree(res_proxy.base_dir)


@pytest.mark.client
@pytest.mark.slow
@pytest.mark.parametrize('path',
                         [f'{_RPC_PATH}/context/constants',
                          f'{_RPC_PATH}/helpers/baking_rights',
                          f'{_RPC_PATH}/helpers/current_level',
                          f'{_RPC_PATH}/minimal_valid_time',
                          f'{_RPC_PATH}/context/constants',
                          f'{_RPC_PATH}/context/constants/errors',
                          f'{_RPC_PATH}/context/delegates',
                          f'{_RPC_PATH}/context/nonces/3',
                          f'{_RPC_PATH}/helpers/endorsing_rights',
                          f'{_RPC_PATH}/helpers/levels_in_current_cycle',
                          f'{_RPC_PATH}/votes/ballot_list',
                          f'{_RPC_PATH}/votes/ballots',
                          f'{_RPC_PATH}/votes/current_period_kind',
                          f'{_RPC_PATH}/votes/current_proposal',
                          f'{_RPC_PATH}/votes/current_quorum',
                          f'{_RPC_PATH}/votes/listings',
                          f'{_RPC_PATH}/votes/proposals'])
def test_compare(vanilla_and_proxy_clients: Tuple[Client, Client], path: str):
    """ Compare the default client and the proxy clients on
        a number of RPCs that are done locally by the proxy client
        to witness they yield the same results """
    vanilla, proxy = vanilla_and_proxy_clients

    jsons = []
    for client in [vanilla, proxy]:
        jsons.append(client.rpc(verb="get", path=path))

    assert jsons[0] == jsons[1],\
        f"""Executing {path} on the default client and the proxy client
lead different results. Default client returns:
{jsons[0]}
while proxy client returns:
{jsons[1]}"""


def test_wrong_proto(proxy_client):
    """Test that tezos-client --mode proxy --protocol P fails
    when the endpoint's protocol is NOT P"""
    # The chosen protocol must differ from the protocol
    # initialized by the proxy_client fixture
    cmd = ["--protocol", _OTHER_PROTO, "bake", "for", "bootstrap1"]
    (_, stderr, return_code) = proxy_client.run_generic(cmd, check=False)
    assert return_code != 0
    err_msg = (
        f"Protocol passed to the proxy ({_OTHER_PROTO})"
        + f" and protocol of the node ({_PROTO}) differ"
    )
    assert err_msg in stderr