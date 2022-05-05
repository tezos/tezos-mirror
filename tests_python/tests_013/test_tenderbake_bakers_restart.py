import copy
import time
import pytest
from tools import constants
from launchers.sandbox import Sandbox
from . import protocol

NUM_NODES = 5  # because we assume 5 (bootstrap) accounts
NUM_TEST_CYCLES = 4
# CYCLE_DUR should be correlated with MINIMAL_BLOCK_DELAY and
# DELAY_INCREMENT_PER_ROUND below
CYCLE_DUR = 10
MINIMAL_BLOCK_DELAY = 4
DELAY_INCREMENT_PER_ROUND = 1


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
@pytest.mark.tenderbake
class TestProtoTenderbake:
    """Run a number of nodes and start the bakers incrementally, each one
    after one round duration more. After all bakers have been
    started, they should be able to reach a decision."""

    def test_init(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(
                i,
                params=constants.NODE_PARAMS,
                log_levels=constants.TENDERBAKE_NODE_LOG_LEVELS,
            )

        proto_params = dict(protocol.TENDERBAKE_PARAMETERS)
        parameters = copy.deepcopy(proto_params)
        parameters['minimal_block_delay'] = str(MINIMAL_BLOCK_DELAY)
        parameters['delay_increment_per_round'] = str(DELAY_INCREMENT_PER_ROUND)
        parameters['consensus_threshold'] = (
            2 * (parameters['consensus_threshold'] // 3) + 1
        )

        protocol.activate(sandbox.client(0), parameters=parameters)

        for i in range(NUM_NODES):
            sandbox.add_baker(
                i,
                [f'bootstrap{i + 1}'],
                proto=protocol.DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
                run_params=['--liquidity-baking-toggle-vote', 'pass'],
            )

    def test_restart(self, sandbox):
        # in the even cycles we run all the bakers
        # in the odd cycles one is dead
        # we iterate cyclically through the bakers to choose the dead one
        dead_baker = 0
        for cycle in range(NUM_TEST_CYCLES):
            if cycle % 2 == 1:
                sandbox.rm_baker(dead_baker, proto=protocol.DAEMON)
                # we let some time pass
                # (there will be no progress during this time)
                time.sleep(MINIMAL_BLOCK_DELAY)
            else:
                if cycle > 1:
                    sandbox.add_baker(
                        dead_baker,
                        [f'bootstrap{dead_baker + 1}'],
                        proto=protocol.DAEMON,
                        log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
                        run_params=['--liquidity-baking-toggle-vote', 'pass'],
                    )
                # the CYCLE_DUR is long enough for bakers to take a decision
                time.sleep(CYCLE_DUR)
                dead_baker = (dead_baker + 1) % NUM_NODES

    def test_level(self, sandbox):
        expected_min_level = 2 + NUM_TEST_CYCLES / 2
        for client in sandbox.all_clients():
            level = client.get_level()
            assert level >= expected_min_level
