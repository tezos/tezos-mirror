import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol


BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
PARAMS = constants.NODE_PARAMS + ['--history-mode', 'full']
LEVEL_A = 10
LEVEL_B = 20
GROUP1 = [0]
GROUP2 = [3, 4]


@pytest.mark.multinode
@pytest.mark.incremental
@pytest.mark.snapshot
@pytest.mark.slow
class TestMultiNodeSnapshot:
    def test_init(self, sandbox: Sandbox):
        for i in GROUP1:
            sandbox.add_node(i, params=PARAMS)
        protocol.activate(sandbox.client(GROUP1[0]), activate_in_the_past=True)

    def test_bake_group1_level_a(self, sandbox: Sandbox):
        for _ in range(LEVEL_A - 1):
            sandbox.client(GROUP1[0]).bake('bootstrap1', BAKE_ARGS)
            sandbox.client(GROUP1[0]).endorse('bootstrap2')

    def test_group1_level_a(self, sandbox: Sandbox, session: dict):
        for i in GROUP1:
            assert utils.check_level(sandbox.client(i), LEVEL_A)
        session['head_hash'] = sandbox.client(GROUP1[0]).get_head()['hash']

    def test_terminate_group1(self, sandbox: Sandbox):
        for i in GROUP1:
            sandbox.node(i).terminate()
        time.sleep(1)

    def test_export_snapshot(self, sandbox: Sandbox, tmpdir, session: dict):
        node_export = sandbox.node(GROUP1[0])
        file = f'{tmpdir}/FILE.full'
        head_hash = session['head_hash']
        node_export.snapshot_export(file, params=['--block', head_hash])

        for i in GROUP2:
            sandbox.add_node(i, snapshot=file, params=PARAMS)

    def test_rerun_group1(self, sandbox: Sandbox):
        for i in GROUP1:
            sandbox.node(i).run()
            sandbox.client(i).check_node_listening()

    def test_level(self, sandbox: Sandbox):
        for i in GROUP1 + GROUP2:
            assert utils.check_level(sandbox.client(i), LEVEL_A)

    def test_bake_group2_level_b(self, sandbox: Sandbox):
        for _ in range(LEVEL_B - LEVEL_A):
            sandbox.client(GROUP2[0]).bake('bootstrap1', BAKE_ARGS)
            sandbox.client(GROUP2[0]).endorse('bootstrap2')

    def test_all_level_c(self, sandbox: Sandbox):
        for client in sandbox.all_clients():
            assert utils.check_level(client, LEVEL_B)
