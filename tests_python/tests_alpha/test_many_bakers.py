import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol

# TODO parameterize test

MINIMAL_BLOCK_DELAY = 15


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyBakers:
    """Run 5 bakers and num nodes, wait and check logs"""

    def test_init(self, sandbox: Sandbox):
        for i in range(10):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
        protocol.activate(sandbox.client(0))
        for i in range(5):
            sandbox.add_baker(
                i,
                [f'bootstrap{i + 1}'],
                proto=protocol.DAEMON,
                run_params=['--liquidity-baking-escape-vote', 'pass'],
            )

    def test_wait(self):
        # expects two level to be added to level start
        time.sleep(2 * MINIMAL_BLOCK_DELAY)

    def test_progress(self, sandbox: Sandbox):
        min_level = min(
            [client.get_level() for client in sandbox.all_clients()]
        )
        assert min_level >= 3

    @pytest.mark.xfail
    def test_check_logs(self, sandbox: Sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        error_pattern = r"canceled|crashed"
        assert utils.check_logs(sandbox.logs, error_pattern)
