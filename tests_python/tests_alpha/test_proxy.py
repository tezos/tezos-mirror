""" This file tests the proxy mode (tezos-client --mode proxy)
"""
import re
import time
from typing import List, Tuple

import pytest
from client.client import Client

from . import protocol

# Protocol-dependent stuff is these three constants:
_OTHER_PROTO = protocol.PREV_HASH
_PROTO = protocol.HASH
_PROTO_LOG_KEY = protocol.DAEMON

_PROXY_RPC_LOG = {"TEZOS_LOG": f"{_PROTO_LOG_KEY}.proxy_rpc->debug"}
_BLOCK_ID = "head"
_CHAIN_ID = "main"
_RPC_PATH = f'/chains/{_CHAIN_ID}/blocks/{_BLOCK_ID}'
_COMPARED_PATHS = [
    f'{_RPC_PATH}/context/constants',
    f'{_RPC_PATH}/helpers/baking_rights',
    f'{_RPC_PATH}/helpers/baking_rights?&all=true',
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
    f'{_RPC_PATH}/votes/proposals',
]


def _update_node_to_protocol(sandbox, node_idx):
    # Vanilla client (not mockup, not proxy)
    node_client = sandbox.client(node_idx)
    # step 1
    protocol.activate(node_client)
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

    _update_node_to_protocol(sandbox, node_idx)
    sandbox.rm_client(client_idx)  # We don't need the default client anymore

    result = sandbox.register_client(node_idx, node.rpc_port, mode="proxy")
    sandbox.init_client(result, node)

    return result  # Test depending on this fixture executes here


def _run_cmd_track_caches_creation(
    client: Client, cmd: List[str]
) -> List[Tuple[str, str]]:
    """
    Run the given command and inspects stdout to keep track of such lines:

    ... alpha.proxy_rpc: proxy cache created for chain main and block head

    This method returns the sequence of pairs (chain, blocks) for which
    such lines have been recognized
    """
    (_, stderr, _) = client.run_generic(cmd, env_change=_PROXY_RPC_LOG)
    if not stderr:
        return []

    pattern = (
        r"^.*proxy_rpc: proxy cache created for "
        + r"chain (\w+) and block (\w+)"
    )

    result = []
    for line in stderr.split("\n"):
        match = re.search(pattern, line)
        if match:
            grps = match.groups()
            result.append((grps[0], grps[1]))

    return result


@pytest.mark.slow
@pytest.mark.parametrize(
    'cmd', [["rpc", "get", path] for path in _COMPARED_PATHS]
)
def test_cache_at_most_once(proxy_client: Client, cmd: List[str]):
    """
    This test checks that the proxy client creates its cache for
    RPC answers at most once for a given (chain, block) pair.
    """
    pairs = _run_cmd_track_caches_creation(proxy_client, cmd)
    assert (
        pairs
    ), "Proxy cache should have been created when executing " + " ".join(cmd)

    def _gen_msg(chain_n_block: Tuple[str, str]) -> str:
        return (
            f"proxy RPC cache for chain {chain_n_block[0]}"
            + f" and block {chain_n_block[1]} created more than once"
        )

    created = set()
    for pair in pairs:
        if pair in created:
            assert False, _gen_msg(pair)
        created.add(pair)
