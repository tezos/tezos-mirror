import time

import pytest
from launchers.sandbox import Sandbox
from . import protocol

LOG_LEVEL = {"validator.chain": "debug", "validator.peer": "debug"}


def params(threshold=0, latency=3):
    return [
        '--sync-latency',
        str(latency),
        '--synchronisation-threshold',
        str(threshold),
        '--connections',
        '100',
    ]


@pytest.mark.baker
@pytest.mark.incremental
class TestThresholdZero:
    """Threshold 0, peer always bootstrapped."""

    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        sandbox.add_baker(0, ['bootstrap5'], proto=protocol.DAEMON)

    def test_bootstrap(self, sandbox: Sandbox):
        client = sandbox.client(0)
        assert client.sync_state() == 'synced'


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestThresholdOne:
    """First peer has threshold zero, second peer has threshold one"""

    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        protocol.activate(sandbox.client(0))
        sandbox.add_baker(0, ['bootstrap5'], proto=protocol.DAEMON)

    def test_bootstrap(self, sandbox: Sandbox):
        client = sandbox.client(0)
        assert client.sync_state() == 'synced'

    def test_add_node(self, sandbox: Sandbox):
        sandbox.add_node(
            1, params=params(1), log_levels=LOG_LEVEL, config_client=False
        )
        sandbox.client(1).bootstrapped()


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestThresholdTwo:
    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(0), log_levels=LOG_LEVEL)
        protocol.activate(sandbox.client(0))
        sandbox.add_baker(0, ['bootstrap5'], proto=protocol.DAEMON)

    def test_add_nodes(self, sandbox: Sandbox):
        sandbox.add_node(
            1, params=params(2), log_levels=LOG_LEVEL, config_client=False
        )
        sandbox.add_node(
            2, params=params(2), log_levels=LOG_LEVEL, config_client=False
        )
        sandbox.add_node(
            3, params=params(1), log_levels=LOG_LEVEL, config_client=False
        )

    @pytest.mark.timeout(5)
    def test_node_3_bootstrapped(self, sandbox: Sandbox):
        sandbox.client(3).bootstrapped()

    @pytest.mark.timeout(5)
    def test_node_1_bootstrapped(self, sandbox: Sandbox):
        sandbox.client(1).bootstrapped()


@pytest.mark.slow
@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestStuck:
    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        protocol.activate(sandbox.client(0))
        sandbox.add_baker(0, ['bootstrap5'], proto=protocol.DAEMON)

    def test_kill_baker(self, sandbox: Sandbox):
        """Bake a few blocks and kill baker"""
        time.sleep(2)
        sandbox.rm_baker(0, proto=protocol.DAEMON)
        time.sleep(5)

    def test_progress(self, sandbox: Sandbox):
        assert sandbox.client(0).get_level() >= 2

    def test_add_node(self, sandbox: Sandbox):
        sandbox.add_node(
            1, params=params(2), config_client=False, log_levels=LOG_LEVEL
        )
        sandbox.add_node(
            2, params=params(2), config_client=False, log_levels=LOG_LEVEL
        )

    def test_all_nodes_boostrap(self, sandbox: Sandbox):
        """Eventually, 1 and 2 are bootstrapped with the chain stuck. """
        sandbox.client(1).bootstrapped()
        sandbox.client(2).bootstrapped()
        assert sandbox.client(1).sync_state() == 'stuck'
        assert sandbox.client(2).sync_state() == 'stuck'


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestSplitView:
    def test_setup_network(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        protocol.activate(sandbox.client(0))
        sandbox.add_node(
            1, params=params(), config_client=False, log_levels=LOG_LEVEL
        )
        sandbox.add_node(
            2,
            params=params(2, latency=15),
            config_client=False,
            log_levels=LOG_LEVEL,
        )
        sandbox.add_baker(0, ['bootstrap5'], proto=protocol.DAEMON)

    @pytest.mark.timeout(10)
    def test_all_nodes_boostrap(self, sandbox: Sandbox):
        assert sandbox.client(0).sync_state() == 'synced'
        assert sandbox.client(1).sync_state() == 'synced'
        sandbox.client(2).bootstrapped()

    def test_pause(self):
        time.sleep(2)

    def test_disconnect_node(self, sandbox: Sandbox):
        """node 1 is disconnected from baker"""
        sandbox.client(1).ban_peer(sandbox.node(0).p2p_port)
        sandbox.client(0).ban_peer(sandbox.node(1).p2p_port)

    def test_sync_status(self, sandbox: Sandbox):
        assert sandbox.client(0).sync_state() == 'synced'
        assert sandbox.client(1).sync_state() == 'synced'
        assert sandbox.client(2).sync_state() == 'synced'


NUM_NODES = 8
RUNNING_TIME = 10


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyNodesBootstrap:
    """Run many nodes, bake for a while, add a node and check it's bootstrapped
    when it should be."""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        parameters = dict(protocol.PARAMETERS)
        parameters["time_between_blocks"] = ["1", "0"]
        protocol.activate(sandbox.client(0))
        sandbox.add_baker(0, ['bootstrap1'], proto=protocol.DAEMON)
        sandbox.add_node(
            1, params=params(), log_levels=LOG_LEVEL, config_client=False
        )

    def test_bootstrap(self, sandbox: Sandbox):
        sandbox.client(0).bootstrapped()
        sandbox.client(1).bootstrapped()

    def test_add_nodes(self, sandbox: Sandbox):
        for i in range(2, NUM_NODES):
            sandbox.add_node(
                i, params=params(), config_client=False, log_levels=LOG_LEVEL
            )

    def test_let_baking(self):
        time.sleep(RUNNING_TIME)

    def test_progress(self, sandbox, session):
        cur_level = sandbox.client(NUM_NODES - 1).get_level()
        assert cur_level > RUNNING_TIME // 2  # check progress
        session['cur_level'] = cur_level

    @pytest.mark.timeout(50)
    def test_add_one_more_node(self, sandbox, session):
        new_node = NUM_NODES
        sandbox.add_node(
            new_node,
            params=params(NUM_NODES),
            config_client=False,
            log_levels=LOG_LEVEL,
        )
        time.sleep(1)
        sandbox.client(new_node).p2p_stat()
        sandbox.client(new_node).bootstrapped()
        cur_level = session['cur_level']
        assert sandbox.client(new_node).get_level() >= cur_level
