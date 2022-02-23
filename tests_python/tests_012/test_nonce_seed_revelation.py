import time
import pytest
from tools import constants
from launchers.sandbox import Sandbox
from . import protocol


BLOCKS_PER_COMMITMENT = protocol.PARAMETERS['blocks_per_commitment']
BLOCKS_PER_CYCLE = protocol.PARAMETERS['blocks_per_cycle']
FIRST_PROTOCOL_BLOCK = 1
TIMEOUT = 60

MINIMAL_BLOCK_DELAY = 1
DELAY_INCREMENT_PER_ROUND = 1
TEST_DURATION = (
    FIRST_PROTOCOL_BLOCK + 2 * BLOCKS_PER_CYCLE
) * MINIMAL_BLOCK_DELAY
NUM_NODES = 5


@pytest.mark.incremental
@pytest.mark.slow
@pytest.mark.baker
class TestNonceSeedRevelation:
    """Test baker injection of nonce revelations.

    See http://tezos.gitlab.io/012_ithaca/proof_of_stake.html

    Runs a node and a baker. The baker bakes two full cycles.
    We collect nonce hashes from the first cycle. And check
    that they are revealed in the second cycle"""

    def test_init(self, sandbox: Sandbox):
        """Run a node and a baker.

        The node runs in archive mode to get metadata in `client.get_block()`.
        The protocol is activated in the past so the baker can submit blocks
        immediately without waiting for current time."""

        node_params = constants.NODE_PARAMS + ['--history-mode', 'archive']
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=node_params)

        # client setup
        parameters = protocol.get_parameters()
        parameters['minimal_block_delay'] = str(MINIMAL_BLOCK_DELAY)
        parameters['delay_increment_per_round'] = str(DELAY_INCREMENT_PER_ROUND)
        protocol.activate(sandbox.client(0), parameters=parameters)

        # baker setup
        # delegated_accounts = [f'bootstrap{i}' for i in range(1, 6)]
        for i in range(NUM_NODES):
            sandbox.add_baker(
                i,
                [f"bootstrap{i + 1}"],
                proto=protocol.DAEMON,
                log_levels=constants.TENDERBAKE_BAKER_LOG_LEVELS,
            )

    @pytest.mark.timeout(2 * TEST_DURATION)
    def test_wait_for_two_cycles(self, sandbox: Sandbox):
        """Poll the node until target level is reached """
        target = FIRST_PROTOCOL_BLOCK + 2 * BLOCKS_PER_CYCLE
        level = target - 1
        while level < target:
            time.sleep(
                4 * MINIMAL_BLOCK_DELAY
            )  # sleep first to avoid useless first query
            if sandbox.client(0).get_level() >= target:
                break
        # No need to bake more
        for i in range(NUM_NODES):
            sandbox.rm_baker(i, proto=protocol.DAEMON)

    def test_get_all_blocks(self, sandbox: Sandbox, session: dict):
        """Retrieve all blocks for two full cycles. """
        blocks = [
            sandbox.client(0).get_block(FIRST_PROTOCOL_BLOCK + i)
            for i in range(2 * BLOCKS_PER_CYCLE)
        ]
        session['blocks'] = blocks

    def test_cycle_alignment(self, session):
        """Test cycles start where they are supposed to start.

        Not really needed but helps clarifying cycles positions."""

        blocks = session['blocks']
        # blocks[0] is considered cycle = 0, cycle_position = 0 for the new
        # protocol, but because it is a protocol transition block, it
        # doesn't have the "cycle" and "cycle_position" metadata (unlike
        # the remaining blocks)
        initial_block_level = blocks[1]['metadata']['level_info']
        assert initial_block_level['cycle'] == 0
        assert initial_block_level['cycle_position'] == 1
        final_block_level = blocks[BLOCKS_PER_CYCLE]['metadata']['level_info']
        assert final_block_level['cycle'] == 1
        assert final_block_level['cycle_position'] == 0

    def test_collect_seed_nonce_hashes(self, session):
        """Collect nonce hashes in the block headers in the first cycle """
        seed_nonce_hashes = {}
        blocks = session['blocks']
        for i in range(BLOCKS_PER_CYCLE // BLOCKS_PER_COMMITMENT):
            level = (i + 1) * BLOCKS_PER_COMMITMENT - 1
            seed_nonce_hash = blocks[level]['header']['seed_nonce_hash']
            seed_nonce_hashes[level] = seed_nonce_hash
        session['seed_nonce_hashes'] = seed_nonce_hashes

    def test_check_revelations(self, session):
        """Collect reveal ops in second cycle and check they match
        the nonce hashes from first cycle."""
        blocks = session['blocks']
        seed_nonce_hashes = session['seed_nonce_hashes']
        ops = []
        # collect all operations
        for i in range(BLOCKS_PER_CYCLE, 2 * BLOCKS_PER_CYCLE):
            ops.extend(blocks[i]['operations'][2])
        reveal_ops = {}
        for operation in ops:
            content = operation['contents'][0]
            # there should be only revelations there
            assert content['kind'] == "seed_nonce_revelation"
            level = content['level'] - FIRST_PROTOCOL_BLOCK
            # Can't submit twice the same reveal op
            assert level not in reveal_ops
            # level should match a seed
            assert level in seed_nonce_hashes
            reveal_ops[level] = content['nonce']

        # check all nonce hashes have been revealed
        assert len(reveal_ops) == len(seed_nonce_hashes)
        # we could go a step further and check that revelations are correct
