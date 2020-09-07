""" This file tests the proxy mode (tezos-client --mode proxy)
"""
import shutil
import time
from typing import Any, Iterator, List, Tuple
import re
import pytest
from client.client import Client
from client.client_output import extract_rpc_answer
from tools import utils

from tools.constants import ALPHA, CARTHAGE

_OTHER_PROTO = CARTHAGE

_PROXY_RPC_LOG = {"TEZOS_LOG": "alpha.proxy_rpc->debug"}
_BLOCK_ID = "head"
_CHAIN_ID = "main"
_GIVER = "bootstrap1"
_PROTO = ALPHA
_RPC_PATH = f'/chains/{_CHAIN_ID}/blocks/{_BLOCK_ID}'
_PROXY_RPC_CONTEXT_LOG = {"TEZOS_LOG": "proxy_rpc_ctxt->debug"}
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


def _rpc_get(client, path) -> Tuple[Any, bool, bool]:
    """
    Performs `rpc get path` on `client` and returns:

    1/ the json obtained
    2/ whether the RPC was done locally
    3/ whether the RPC was delegated to http
    """
    cmd = ["rpc", "get", path]
    (stdout, stderr, _) = client.run_generic(
        cmd, env_change=_PROXY_RPC_CONTEXT_LOG
    )

    pattern = r"proxy_rpc_ctxt: Done \w+ \w+ " + re.escape(path) + " locally"
    local = re.search(pattern, stderr)

    pattern = (
        r"proxy_rpc_ctxt: Delegating \w+ \w+ " + re.escape(path) + " to http"
    )
    http = re.search(pattern, stderr)

    return (extract_rpc_answer(stdout), local is not None, http is not None)


@pytest.mark.client
def test_chain_block_context_delegates(proxy_client):
    """ Test an RPC that is done locally """
    path = f'/chains/{_CHAIN_ID}/blocks/{_BLOCK_ID}/context/delegates'
    _, local, _ = _rpc_get(proxy_client, path)
    assert local


@pytest.mark.client
def test_chain_blocks(proxy_client: Client):
    """ Test an RPC that is delegated to the node """
    path = f'/chains/{_CHAIN_ID}/blocks'
    _, _, http = _rpc_get(proxy_client, path)
    assert http


@pytest.mark.client
def test_network_self(proxy_client):
    """ Test an RPC that is delegated to the node """
    _, _, http = _rpc_get(proxy_client, '/network/self')
    assert http


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
@pytest.mark.parametrize('path', _COMPARED_PATHS)
def test_compare(vanilla_and_proxy_clients: Tuple[Client, Client], path: str):
    """Compare the default client and the proxy clients on
    a number of RPCs that are done locally by the proxy client
    to witness they yield the same results"""
    vanilla, proxy = vanilla_and_proxy_clients

    jsons = []
    for client in [vanilla, proxy]:
        json_, local, http = _rpc_get(client, path)
        if client == vanilla:
            assert not local and not http
        if client == proxy:
            assert local
        jsons.append(json_)

    assert (
        jsons[0] == jsons[1]
    ), f"""Executing {path} on the default client and the proxy client
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


@pytest.mark.slow
@pytest.mark.parametrize(
    'path',
    [
        f'{_RPC_PATH}/helpers/baking_rights',
        f'{_RPC_PATH}/helpers/baking_rights?&all=true',
        f'{_RPC_PATH}/context/delegates',
        f'{_RPC_PATH}/context/nonces/3',
        f'{_RPC_PATH}/helpers/endorsing_rights',
        f'{_RPC_PATH}/votes/ballot_list',
        f'{_RPC_PATH}/votes/ballots',
        f'{_RPC_PATH}/votes/current_period_kind',
        f'{_RPC_PATH}/votes/current_proposal',
        f'{_RPC_PATH}/votes/current_quorum',
        f'{_RPC_PATH}/votes/listings',
        f'{_RPC_PATH}/votes/proposals',
    ],
)
def test_context_suffix_no_rpc(proxy_client: Client, path: str):
    """
    This test checks that the proxy client never does a useless RPC.

    I.e. it checks that if the proxy client requested
    `/chains/<main>/blocks/<head>/context/raw/bytes/some_path`
    it doesn't later request
    `/chains/<main>/blocks/<head>/context/raw/bytes/some_other_path`

    with `some_other_path` being a suffix of `some_path`. In this
    scenario, the proxy client should look for the tail of
    `some_other_path` that is a suffix of `some_path` and use this
    suffix to get the data within the tree received by the first request.

    For this, this test inspects the debug output produced by
    setting TEZOS_LOG to alphas.proxy_rpc->debug. This causes the client
    to print the RPCs done to get pieces of the context:

    alpha.proxy_rpc: P/v1/constants
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/v1/first_level
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/random_seed
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/roll_snapshot
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/last_roll/0

    where P is /chains/<main>/blocks/<head>/context/raw/bytes

    In the client, this behavior is implemented by putting the result
    of RPC calls in a trie.
    """
    cmd = ["rpc", "get", path]
    (_, stderr, _) = proxy_client.run_generic(cmd, env_change=_PROXY_RPC_LOG)

    prefix = "/chains/<main>/blocks/<head>/context/raw/bytes"
    search_re = f".proxy_rpc: {prefix}/(.*)"

    # The suffix of RPCs querying data, such as "v1/constants",
    # "v1/first_level", "cycle/0/random_seed", etc. (see method doc)
    context_queries = []
    for line in stderr.splitlines():
        match = re.search(search_re, line)
        if match:
            suffix = match.group(1)
            context_queries.append(suffix)

    assert len(context_queries) >= 2, "There should be at least two queries"

    def gen_msg(query_before, query):
        return (
            f"Query {query_before} should not be followed by query"
            + f" {query} because the latter is a suffix of the former."
            + " Hence the proxy should reuse the data of the first query."
        )

    i = len(context_queries) - 1
    while i >= 0:
        query = context_queries[i]
        j = i - 1
        while j >= 0:
            query_before = context_queries[j]
            assert not query_before.startswith(query), gen_msg(
                query_before, query
            )
            j -= 1
        i -= 1


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
