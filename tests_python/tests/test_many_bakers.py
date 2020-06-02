import time

import pytest

from launchers.sandbox import Sandbox
from tools import constants, utils

# TODO parameterize test


@pytest.mark.baker
@pytest.mark.multinode
@pytest.mark.slow
@pytest.mark.incremental
class TestManyBakers:
    """Run 5 bakers and num nodes, wait and check logs"""

    def test_init(self, sandbox: Sandbox):
        for i in range(10):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
        utils.activate_alpha(sandbox.client(0))
        utils.synchronize(sandbox.all_clients())
        for i in range(1, 10):
            utils.remember_baker_contracts(sandbox.client(i))
        for i in range(5):
            sandbox.add_baker(i, f'baker{i + 1}',
                              proto=constants.ALPHA_DAEMON)

    def test_wait(self):
        time.sleep(5)

    def test_progress(self, sandbox: Sandbox):
        min_level = min([client.get_level()
                         for client in sandbox.all_clients()])
        assert min_level >= 3

    @pytest.mark.xfail
    def test_check_logs(self, sandbox: Sandbox):
        if not sandbox.log_dir:
            pytest.skip()
        assert sandbox.logs
        error_pattern = r"canceled|crashed"
        assert utils.check_logs(sandbox.logs, error_pattern)
