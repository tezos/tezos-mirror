import time
import pytest
from tools import constants
from launchers.sandbox import Sandbox


NUM_NODES = 5
NUM_RETRIES = 20  # empirical values for testing a liveness property
POLLING_TIME = 10  # NUM_RETRY * POLLING_TIME = 200s, should be conservative


@pytest.mark.multinode
@pytest.mark.incremental
class TestTrustedRing:
    """This test sets up a network of public peers (running the default
    p2p protocol), with no initial bootstrap peers. It initializes a
    trusted ring relationship, and checks that points are advertised
    correctly to the whole network."""

    def test_init(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(
                i,
                private=False,
                peers=[],
                params=constants.NODE_PARAMS,
                config_client=False,
            )

    def test_no_peers(self, sandbox: Sandbox):
        """ Initially, nobody knows other peers. """
        for client in sandbox.all_clients():
            res = client.p2p_stat()
            assert not res.peers

    def test_add_peers(self, sandbox: Sandbox):
        """ Set up a trusted ring topology. """
        base_p2p = sandbox.p2p
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            client.trust_peer(base_p2p + ((i + 1) % NUM_NODES))

    def test_check_clique(self, sandbox: Sandbox):
        """Everyone should be connected to everyone else. This is a
        liveness property. Its realization depends on the timing of the
        p2p maintenance process. The check is repeated up to NUM_RETRIES
        times with a POLLING_TIME seconds wait."""
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            for _ in range(NUM_RETRIES):
                points = client.p2p_stat().points.values()
                num_connected = len(
                    [point for point in points if point.is_connected]
                )
                if num_connected == NUM_NODES - 1:
                    break
                time.sleep(POLLING_TIME)
            assert num_connected == NUM_NODES - 1

    def test_check_tables(self, sandbox: Sandbox):
        """Test various assumptions on the point/peer tables.
        Each peer has exactly one trusted neighbor. Tables don't
        contain their own peer/point id and contain exactly NUM_NODES - 1
        values.

        The previous test should guarantee that maintenance has been
        performed when this test is run."""
        base_p2p = sandbox.p2p
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            point_id = f'127.0.0.1:{base_p2p + i}'
            peer_id = client.rpc('get', '/network/self')
            res = client.p2p_stat()
            assert peer_id not in res.peers
            assert point_id not in res.points
            num_trusted = 0
            for point_id, point in res.points.items():
                num_trusted += point.is_trusted
            assert num_trusted == 1
            assert len(res.peers) == NUM_NODES - 1
            assert len(res.points) == NUM_NODES - 1

    def test_set_expected_peers(self, sandbox: Sandbox):
        """For all nodes, we add one expected peer_id
        for the successor node."""
        peers_id = {}
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            peers_id[i] = client.rpc('get', '/network/self')

        for i in range(NUM_NODES):
            client = sandbox.client(i)
            client.set_expected_peer_id(
                sandbox.p2p + ((i + 1) % NUM_NODES),
                peers_id[(i + 1) % NUM_NODES],
            )

    def test_expected_peers(self, sandbox: Sandbox):
        """For all nodes, we check that expected peer_id was
        set properly."""
        peers_id = {}
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            peers_id[i] = client.rpc('get', '/network/self')

        for i in range(NUM_NODES):
            client = sandbox.client(i)
            expected_id = client.get_expected_peer_id(
                sandbox.p2p + ((i + 1) % NUM_NODES)
            )
            assert expected_id == peers_id[(i + 1) % NUM_NODES]

    def test_wrong_expected_peer(self, sandbox: Sandbox):
        """We change the expected peer_id set previously to a wrong
        expected peer_id."""
        peers_id = {}
        for i in range(NUM_NODES):
            client = sandbox.client(i)
            peers_id[i] = client.rpc('get', '/network/self')

        for i in range(NUM_NODES):
            client = sandbox.client(i)
            client.set_expected_peer_id(
                sandbox.p2p + ((i + 2) % NUM_NODES), peers_id[i]
            )

    def test_check_stat_with_wrong_expected_peers(self, sandbox: Sandbox):
        """All nodes are public, everyone should be connected. But
        only one neighbor should be trusted."""
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
            # We lost two connections
            assert num_connected == NUM_NODES - 1 - 2
