import pytest
from tools import constants


NUM_NODES = 5


@pytest.mark.multinode
@pytest.mark.incremental
class TestTrustedRing:
    """ This test sets up a network of public peers (running the default
        p2p protocol), with no initial bootstrap peers. It initializes a
        trusted ring relationship, and checks that points are advertised
        correctly to the whole network. """

    def test_init(self, sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, private=False, peers=[],
                             params=constants.NODE_PARAMS, config_client=False)

    def test_no_peers(self, sandbox):
        """ Initially, nobody knows other peers. """
        for client in sandbox.all_clients():
            res = client.p2p_stat()
            assert not res.peers

    def test_add_peers(self, sandbox):
        """ Set up a trusted ring topology. """
        base_p2p = sandbox.p2p
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            client.trust_peer(base_p2p + ((i + 1) % NUM_NODES))

    def test_check_stat(self, sandbox):
        """ All nodes, are public, everyone should be connected. But
            Only one neighbor should be trusted.  """
        base_p2p = sandbox.p2p
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            point_id = '127.0.0.1:' + str(base_p2p + i)
            peer_id = client.rpc('get', '/network/self')
            res = client.p2p_stat()
            assert peer_id not in res.peers
            assert point_id not in res.points
            num_trusted = 0
            num_connected = 0
            for point_id, point in res.points.items():
                num_trusted += point.is_trusted
                num_connected += point.is_connected
            assert len(res.peers) == NUM_NODES - 1
            assert len(res.points) == NUM_NODES - 1
            assert num_trusted == 1
            assert num_connected == NUM_NODES - 1
