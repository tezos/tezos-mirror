import time
from datetime import datetime, timedelta
import pytest
from tools import utils, constants

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
PARAMS = constants.NODE_PARAMS + ['--history-mode', 'full']
BATCH = 16


@pytest.mark.multinode
@pytest.mark.incremental
@pytest.mark.snapshot
@pytest.mark.slow
class TestMultiNodeStorageReconstruction:

    def test_init(self, sandbox):
        sandbox.add_node(0, params=PARAMS)
        # Allow fast `bake for` by activating the protocol in the past
        last_hour_date_time = datetime.utcnow() - timedelta(hours=1)
        timestamp = last_hour_date_time.strftime("%Y-%m-%dT%H:%M:%SZ")
        utils.activate_alpha(sandbox.client(0), timestamp=timestamp)

    def test_bake_node0_level_a(self, sandbox, session):
        for _ in range(BATCH - 1):
            sandbox.client(0).bake('bootstrap1', BAKE_ARGS)
            # time.sleep(2)
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        session['head_level'] = sandbox.client(0).get_head()['header']['level']

    def test_export_snapshot(self, sandbox, session):
        node_export = sandbox.node(0)
        # TODO: use a directory shared by the nodes instead of the node's dir
        file = f'{sandbox.node(0).node_dir}/FILE.full'
        session['snapshot_head'] = session['head_hash']
        session['snapshot_level'] = session['head_level']
        node_export.snapshot_export(
            file, params=['--block', session['snapshot_head']])

    # Test the `--reconstruct` flag of the `snapshot import` command
    def test_import_and_reconstruct(self, sandbox, session):
        n0_tmpdir = sandbox.node(0).node_dir
        file = f'{n0_tmpdir}/FILE.full'
        sandbox.add_node(1, snapshot=file, reconstruct=True)
        assert utils.check_level(sandbox.client(1), session['head_level'])

    # Test that all the reconstructed blocks can be
    # requested as non-pruned blocks
    def test_request_all_blocks_as_not_pruned(self, sandbox, session):
        for i in range(session['head_level']):
            assert utils.get_block_per_level(sandbox.client(1), i)

    # Test the reconstruct command on a full storage
    def test_reconstruct_command(self, sandbox, session):
        n0_tmpdir = sandbox.node(0).node_dir
        file = f'{n0_tmpdir}/FILE.full'
        sandbox.add_node(2, snapshot=file)
        assert utils.check_level(sandbox.client(2), session['head_level'])

    # Bake a few blocks
    def test_bake_node0_level_b(self, sandbox, session):
        for _ in range(BATCH):
            sandbox.client(0).bake('bootstrap1', BAKE_ARGS)
            # time.sleep(2)
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        session['head_level'] = sandbox.client(0).get_head()['header']['level']
        assert utils.check_level(sandbox.client(0), session['head_level'])
        assert utils.check_level(sandbox.client(1), session['head_level'])
        assert utils.check_level(sandbox.client(2), session['head_level'])

    def test_unavailable_blocks(self, sandbox, session):
        # We must fail while requesting those pruned blocks
        for i in range(1, session['snapshot_level']):
            with pytest.raises(Exception):
                utils.get_block_per_level(sandbox.client(2), i)

    def test_reconstruct_the_storage(self, sandbox):
        # Stop, reconstruct the storage and restart the node
        sandbox.node(2).terminate()
        time.sleep(2)
        sandbox.node(2).reconstruct()
        sandbox.node(2).run()
        assert sandbox.client(2).check_node_listening()

    def test_available_blocks(self, sandbox, session):
        # We should now success requesting those reconstructed blocks
        for i in range(session['head_level']):
            assert utils.get_block_per_level(sandbox.client(2), i)
