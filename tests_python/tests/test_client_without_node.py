"""Node-less tests for the client.

Some client tests do not require a node running, nor a
persistent mockup environment. These can be placed here.
"""
import pytest


@pytest.mark.client
class TestChainId:

    def test_chain_id_block_hash(self, simple_client):
        block_hash = 'BKyFui5WPY1n3e9aKF3qd2kGBKBtHu3rtm5miYFnUagJC1BdHTF'
        prms = ['compute', 'chain', 'id', 'from', 'block', 'hash', block_hash]
        assert simple_client.run(prms).strip() == 'NetXuwrXPL4VeX5'

    def test_chain_id_seed(self, simple_client):
        seed = 'choucroute'
        prms = ['compute', 'chain', 'id', 'from', 'seed', seed]
        assert simple_client.run(prms).strip() == 'NetXLGmPi3c5DXf'
