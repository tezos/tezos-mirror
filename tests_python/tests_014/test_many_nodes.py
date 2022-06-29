import random
import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol

NUM_NODES = 5
NEW_NODES = 3
REPLACE = False
ERROR_PATTERN = r"Uncaught|registered"


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyNodesBootstrap:
    """Run many nodes, wait a while, run more nodes, check logs"""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        parameters = dict(protocol.PARAMETERS)
        # smaller threshold to make (almost sure) that 3/5 of the delegates
        # have enough endorsing power
        parameters['consensus_threshold'] = 5
        parameters['minimal_block_delay'] = '3'
        parameters['delay_increment_per_round'] = '1'
        protocol.activate(sandbox.client(0), parameters)
        for i in range(1, NEW_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
        for i in range(3):
            sandbox.add_baker(
                i,
                [f'bootstrap{i + 1}'],
                proto=protocol.DAEMON,
                run_params=['--liquidity-baking-toggle-vote', 'pass'],
            )

    def test_add_nodes(self, sandbox: Sandbox):
        for i in range(NEW_NODES, NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)

    def test_sleep_30s(self):
        time.sleep(30)

    def test_add_more_nodes(self, sandbox: Sandbox):
        new_node = NUM_NODES
        for i in range(NEW_NODES):
            if REPLACE:
                running_nodes = list(sandbox.nodes.keys())
                sandbox.rm_node(random.choice(running_nodes))
            sandbox.add_node(new_node + i, params=constants.NODE_PARAMS)

    def test_kill_baker(self, sandbox: Sandbox):
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
        for i in range(3):
            sandbox.rm_baker(i, proto=protocol.DAEMON)

    def test_synchronize(self, sandbox: Sandbox):
        utils.synchronize(sandbox.all_clients())

    def test_progress(self, sandbox: Sandbox):
        level = sandbox.client(0).get_level()
        assert level >= 5

    def test_check_logs(self, sandbox: Sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
