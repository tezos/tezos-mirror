import time
import pytest
from tools import constants, utils


LOG_LEVEL = {"validator.chain":  "debug", "validator.peer": "debug"}


def params(threshold=0):
    return ['--max-latency', '1', '--chain-stuck-delay', '10',
            '--sync-polling-period', '1', '--bootstrap-threshold',
            str(threshold), '--connections', '100']


@pytest.mark.baker
@pytest.mark.incremental
class TestThresholdZero:
    """Threshold 0, peer always bootstrapped."""

    def test_setup_network(self, sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)

    def test_bootstrap(self, sandbox):
        client = sandbox.client(0)
        assert client.sync_state() == 'sync'


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestThresholdOne:
    """First peer has threshold zero, second peer has threshold one"""

    def test_setup_network(self, sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)

    def test_bootstrap(self, sandbox):
        client = sandbox.client(0)
        assert client.sync_state() == 'sync'

    def test_add_node(self, sandbox):
        sandbox.add_node(1, params=params(1), log_levels=LOG_LEVEL,
                         config_client=False)
        sandbox.client(1).bootstrapped()


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestThresholdTwo:

    def test_setup_network(self, sandbox):
        sandbox.add_node(0, params=params(0), log_levels=LOG_LEVEL)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)

    def test_add_nodes(self, sandbox):
        sandbox.add_node(1, params=params(2), log_levels=LOG_LEVEL,
                         config_client=False)
        sandbox.add_node(2, params=params(2), log_levels=LOG_LEVEL,
                         config_client=False)
        sandbox.add_node(3, params=params(1), log_levels=LOG_LEVEL,
                         config_client=False)

    @pytest.mark.timeout(5)
    def test_node_3_bootstrapped(self, sandbox):
        sandbox.client(3).bootstrapped()

    @pytest.mark.timeout(5)
    def test_node_1_bootstrapped(self, sandbox):
        sandbox.client(1).bootstrapped()


@pytest.mark.slow
@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestStuck:

    def test_setup_network(self, sandbox):
        sandbox.add_node(0, params=params(),
                         log_levels=LOG_LEVEL)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)

    def test_kill_baker(self, sandbox):
        """Bake a few blocks and kill baker"""
        time.sleep(2)
        sandbox.rm_baker(0, proto=constants.ALPHA_DAEMON)
        time.sleep(3)

    def test_progress(self, sandbox):
        assert sandbox.client(0).get_level() >= 2

    def test_add_node(self, sandbox):
        sandbox.add_node(1, params=params(1), config_client=False,
                         log_levels=LOG_LEVEL)

    def test_unsync(self, sandbox):
        """Initially, 1 is unsync"""
        assert sandbox.client(1).sync_state() == 'unsync'

    @pytest.mark.timeout(15)
    def test_all_nodes_boostrap(self, sandbox):
        """Eventually, 1 is bootstrapped, considering chain is stuck. """
        sandbox.client(1).bootstrapped()
        assert sandbox.client(1).sync_state() == 'stuck'


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.incremental
class TestSplitView:
    def test_setup_network(self, sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        utils.activate_alpha(sandbox.client(0))
        sandbox.add_node(1, params=params(), config_client=False,
                         log_levels=LOG_LEVEL)
        sandbox.add_node(2, params=params(2), config_client=False,
                         log_levels=LOG_LEVEL)
        sandbox.add_baker(0, 'bootstrap5', proto=constants.ALPHA_DAEMON)

    @pytest.mark.timeout(10)
    def test_all_nodes_boostrap(self, sandbox):
        assert sandbox.client(0).sync_state() == 'sync'
        assert sandbox.client(1).sync_state() == 'sync'
        sandbox.client(2).bootstrapped()

    def test_pause(self):
        time.sleep(2)

    def test_disconnect_node(self, sandbox):
        """node 1 is disconnected from baker"""
        sandbox.client(1).ban_peer(sandbox.node(0).p2p_port)
        sandbox.client(0).ban_peer(sandbox.node(1).p2p_port)

    def test_sync_status(self, sandbox):
        assert sandbox.client(0).sync_state() == 'sync'
        assert sandbox.client(1).sync_state() == 'sync'
        assert sandbox.client(2).sync_state() == 'unsync'


NUM_NODES = 8
RUNNING_TIME = 10


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyNodesBootstrap:
    """Run many nodes, bake for a while, add a node and check it's bootstrapped
       when it should be. """

    def test_init(self, sandbox):
        sandbox.add_node(0, params=params(), log_levels=LOG_LEVEL)
        parameters = dict(constants.PARAMETERS)
        parameters["time_between_blocks"] = ["1", "0"]
        utils.activate_alpha(sandbox.client(0), parameters)
        sandbox.add_baker(0, 'bootstrap1', proto=constants.ALPHA_DAEMON)
        sandbox.add_node(1, params=params(), log_levels=LOG_LEVEL,
                         config_client=False)

    def test_bootstrap(self, sandbox):
        sandbox.client(0).bootstrapped()
        sandbox.client(1).bootstrapped()

    def test_add_nodes(self, sandbox):
        for i in range(2, NUM_NODES):
            sandbox.add_node(i, params=params(), config_client=False,
                             log_levels=LOG_LEVEL)

    def test_let_baking(self):
        time.sleep(RUNNING_TIME)

    def test_progress(self, sandbox, session):
        cur_level = sandbox.client(NUM_NODES-1).get_level()
        assert cur_level > RUNNING_TIME // 2  # check progress
        session['cur_level'] = cur_level

    @pytest.mark.timeout(50)
    def test_add_one_more_node(self, sandbox, session):
        new_node = NUM_NODES
        sandbox.add_node(new_node, params=params(NUM_NODES),
                         config_client=False,
                         log_levels=LOG_LEVEL)
        time.sleep(1)
        sandbox.client(new_node).p2p_stat()
        sandbox.client(new_node).bootstrapped()
        cur_level = session['cur_level']
        assert sandbox.client(new_node).get_level() >= cur_level
