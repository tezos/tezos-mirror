import tempfile
import shutil
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol

PARAMS = constants.NODE_PARAMS

BATCH_1 = 48
BATCH_2 = 48  # not enough bakes to drag the savepoint after snapshot import
BATCH_3 = 32  # enough bakes to drag the savepoint
GROUP1 = [0, 1, 2]
GROUP2 = [0, 1, 2, 3, 4, 5]
GROUP_FULL = [1, 3]
GROUP_ROLLING = [2, 4, 5]
SNAPSHOT_DIR = tempfile.mkdtemp(prefix='tezos-snapshots.')
BLOCKS_PER_CYCLE = 8
PRESERVED_CYCLES = 2
RETAINED_CYCLES = 8


def clean(node):
    shutil.rmtree(node.node_dir)


# Restart node. Side effect: clean store caches
def restart(sandbox, node_id):
    sandbox.node(node_id).terminate_or_kill()
    sandbox.node(node_id).run()
    assert sandbox.client(node_id).check_node_listening()


@pytest.mark.multinode
@pytest.mark.incremental
@pytest.mark.snapshot
@pytest.mark.slow
class TestMultiNodeSnapshot:
    # Tests both the snapshot mechanism and the store's behaviour
    # TL;DR, how it works:
    # - bake few blocks using all history modes
    # - export all kinds of snapshots
    # - import all kinds of snapshots
    # - check consistency (the snapshot's window includes genesis)
    # - bake a few blocks
    # - check consistency (the checkpoints should not move yet)
    # - bake a few blocks
    # - check consistency (the checkpoints should have moved)
    # - export all kinds of snapshots
    # - import all kinds of snapshots
    # - check consistency (checkpoints should be still valid)
    # - bake a few blocks
    # - check consistency (the checkpoints should have moved)

    def test_init(self, sandbox: Sandbox):
        # Node 0: archive baker
        sandbox.add_node(0, params=PARAMS + ['--history-mode', 'archive'])
        # Node 1: full
        sandbox.add_node(1, params=PARAMS + ['--history-mode', 'full'])
        # Node 2: rolling
        sandbox.add_node(2, params=PARAMS + ['--history-mode', 'rolling'])
        protocol.activate(sandbox.client(GROUP1[0]), activate_in_the_past=True)

    def test_bake_batch_1(self, sandbox, session):
        for _ in range(BATCH_1):
            utils.bake(sandbox.client(0))
            sandbox.client(0).run(['endorse', "for", 'bootstrap2', '--force'])
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        session['head_level'] = sandbox.client(0).get_head()['header']['level']
        session['snapshot_level'] = session['head_level']

    def test_group1_batch_1(self, sandbox, session):
        for i in GROUP1:
            assert utils.check_level(sandbox.client(i), session['head_level'])

    ###########################################################################
    # Export all kinds of snapshots
    def test_archive_export_full_1(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node0_batch_1.full'
        export_level = session['snapshot_level']
        sandbox.node(0).snapshot_export(
            file, params=['--block', f'{export_level}']
        )

    def test_archive_export_rolling_1(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node0_batch_1.rolling'
        export_level = session['snapshot_level']
        sandbox.node(0).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    def test_full_export_full_1(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node1_batch_1.full'
        export_level = session['snapshot_level']
        sandbox.node(1).snapshot_export(
            file, params=['--block', f'{export_level}']
        )

    def test_full_export_rolling_1(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node1_batch_1.rolling'
        export_level = session['snapshot_level']
        sandbox.node(1).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    def test_rolling_export_rolling_1(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node2_batch_1.rolling'
        export_level = session['snapshot_level']
        sandbox.node(2).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    ###########################################################################
    # Import all kinds of snapshots
    # New node: 3
    def test_run_full_node_from_archive_1(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node0_batch_1.full'
        sandbox.add_node(3, snapshot=file, params=PARAMS)

    # New node: 4
    def test_run_rolling_node_from_archive_1(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node0_batch_1.rolling'
        sandbox.add_node(4, snapshot=file, params=PARAMS)

    # Reset node 1
    def test_reset_full_node_from_full_1(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node1_batch_1.full'
        sandbox.rm_node(1)
        sandbox.add_node(1, snapshot=file, params=PARAMS)

    # New node: 5
    def test_run_rolling_node_from_full_1(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node1_batch_1.rolling'
        sandbox.add_node(5, snapshot=file, params=PARAMS)

    # Reset node 2
    def test_reset_rolling_node_from_rolling_1(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node2_batch_1.rolling'
        sandbox.rm_node(2)
        sandbox.add_node(2, snapshot=file, params=PARAMS)

    ###########################################################################
    # Check consistency of imported snapshots
    # Do not factorize calls to ease debugging
    # For the full nodes
    def test_node_1_consistency_1(self, sandbox, session):
        node_id = 1
        restart(sandbox, node_id)
        expected_level = session['snapshot_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    def test_node_3_consistency_1(self, sandbox, session):
        node_id = 3
        restart(sandbox, node_id)
        expected_level = session['snapshot_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    # For the rolling nodes
    def test_node_2_consistency_1(self, sandbox, session):
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['snapshot_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_4_consistency_1(self, sandbox, session):
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['snapshot_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_5_consistency_1(self, sandbox, session):
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['snapshot_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    ###########################################################################
    # Bake a few blocks
    def test_bake_batch_2(self, sandbox, session):
        for _ in range(BATCH_2):
            utils.bake(sandbox.client(0))
            sandbox.client(0).run(['endorse', 'for', 'bootstrap2', '--force'])
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        session['head_level'] = sandbox.client(0).get_head()['header']['level']
        for i in GROUP2:
            assert utils.check_level(sandbox.client(i), session['head_level'])

    ###########################################################################
    # Check consistency of imported snapshots after > 5 baked cycles
    # The savepoints of full and rolling nodes **have not** been dragged yet

    # For the full nodes
    def test_node_1_consistency_2(self, sandbox, session):
        node_id = 1
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        savepoint_when_imported = session['snapshot_level']
        expected_savepoint = savepoint_when_imported
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    def test_node_3_consistency_2(self, sandbox, session):
        node_id = 3
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        savepoint_when_imported = session['snapshot_level']
        expected_savepoint = savepoint_when_imported
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    # For the rolling nodes
    # The caboose of rolling mode were no dragged yet as
    # (checkpoint - max_op_ttl(head)) < savepoint
    def test_node_2_consistency_2(self, sandbox, session):
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        savepoint_when_imported = session['snapshot_level']
        expected_savepoint = savepoint_when_imported
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_4_consistency_2(self, sandbox, session):
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        savepoint_when_imported = session['snapshot_level']
        expected_savepoint = savepoint_when_imported
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_5_consistency_2(self, sandbox, session):
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        savepoint_when_imported = session['snapshot_level']
        expected_savepoint = savepoint_when_imported
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    ###########################################################################
    # Bake a few blocks
    def test_bake_batch_3(self, sandbox, session):
        for _ in range(BATCH_3):
            utils.bake(sandbox.client(0))
            sandbox.client(0).run(['endorse', 'for', 'bootstrap2', '--force'])
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        session['head_level'] = sandbox.client(0).get_head()['header']['level']
        session['snapshot_level'] = session['head_level']
        for i in GROUP2:
            assert utils.check_level(sandbox.client(i), session['head_level'])

    ###########################################################################
    # Check consistency of imported snapshots after > 5 baked cycles
    # The savepoints of full and rolling nodes **have** been dragged yet

    # For the full nodes
    def test_node_1_consistency_3(self, sandbox, session):
        node_id = 1
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_savepoint = expected_checkpoint - (
            RETAINED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    def test_node_3_consistency_3(self, sandbox, session):
        node_id = 3
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_savepoint = expected_checkpoint - (
            RETAINED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    # For the rolling nodes
    def test_node_2_consistency_3(self, sandbox, session):
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - (
            RETAINED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_4_consistency_3(self, sandbox, session):
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - (
            RETAINED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_5_consistency_3(self, sandbox, session):
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['head_level']
        # last allowed fork level of the head
        expected_checkpoint = (
            expected_level - PRESERVED_CYCLES * BLOCKS_PER_CYCLE
        )
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - (
            RETAINED_CYCLES * BLOCKS_PER_CYCLE
        )
        expected_caboose = max(expected_checkpoint - max_op_ttl, 0)
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    ###########################################################################
    # Re-export all kinds of snapshots
    def test_archive_export_full_2(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node0_batch_3.full'
        export_level = session['snapshot_level']
        sandbox.node(0).snapshot_export(
            file, params=['--block', f'{export_level}']
        )

    def test_archive_export_rolling_2(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node0_batch_3.rolling'
        export_level = session['snapshot_level']
        sandbox.node(0).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    def test_full_export_full_2(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node1_batch_3.full'
        export_level = session['snapshot_level']
        sandbox.node(1).snapshot_export(
            file, params=['--block', f'{export_level}']
        )

    def test_full_export_rolling_2(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node1_batch_3.rolling'
        export_level = session['snapshot_level']
        sandbox.node(1).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    def test_rolling_export_rolling_2(self, sandbox, session):
        file = f'{SNAPSHOT_DIR}/node2_batch_3.rolling'
        export_level = session['snapshot_level']
        sandbox.node(2).snapshot_export(
            file, params=['--block', f'{export_level}', '--rolling']
        )

    ###########################################################################
    # Import all kinds of snapshots
    # Reset node: 3
    def test_run_full_node_from_archive_2(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node0_batch_3.full'
        sandbox.rm_node(3)
        sandbox.add_node(3, snapshot=file, params=PARAMS)

    # Reset node: 4
    def test_run_rolling_node_from_archive_2(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node0_batch_3.rolling'
        sandbox.rm_node(4)
        sandbox.add_node(4, snapshot=file, params=PARAMS)

    # Reset node 1
    def test_reset_full_node_from_full_2(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node1_batch_3.full'
        sandbox.rm_node(1)
        sandbox.add_node(1, snapshot=file, params=PARAMS)

    # Reset node: 5
    def test_run_rolling_node_from_full_2(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node1_batch_3.rolling'
        sandbox.rm_node(5)
        sandbox.add_node(5, snapshot=file, params=PARAMS)

    # Reset node 2
    def test_reset_rolling_node_from_rolling_2(self, sandbox):
        file = f'{SNAPSHOT_DIR}/node2_batch_3.rolling'
        sandbox.rm_node(2)
        sandbox.add_node(2, snapshot=file, params=PARAMS)

    ###########################################################################
    # Check consistency of imported snapshots with > 5 cycles

    # For the full nodes
    def test_node_1_consistency_4(self, sandbox, session):
        node_id = 1
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    def test_node_3_consistency_4(self, sandbox, session):
        node_id = 3
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        expected_savepoint = expected_checkpoint
        expected_caboose = 0
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.full_node_blocks_availability(
            node_id, sandbox, expected_savepoint, expected_level
        )

    # For the rolling nodes
    def test_node_2_consistency_4(self, sandbox, session):
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - max_op_ttl
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_4_consistency_4(self, sandbox, session):
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - max_op_ttl
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    def test_node_5_consistency_4(self, sandbox, session):
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        max_op_ttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - max_op_ttl
        utils.node_consistency_after_import(
            node_id,
            sandbox,
            expected_level,
            expected_checkpoint,
            expected_savepoint,
            expected_caboose,
        )
        utils.rolling_node_blocks_availability(
            node_id,
            sandbox,
            expected_savepoint,
            expected_caboose,
            expected_level,
        )

    ###########################################################################
    # Clean exported snapshots

    def test_clean_files(self):
        shutil.rmtree(SNAPSHOT_DIR)
