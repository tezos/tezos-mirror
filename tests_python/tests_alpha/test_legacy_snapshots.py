import shutil
import pytest
from tools import utils, paths, constants

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
PARAMS = constants.NODE_PARAMS
BATCH = 100

HOME = paths.TEZOS_HOME

GROUP = [1, 2, 3, 4, 5]

EXPECTED_LEVEL = 101


def check_expected_values(head):
    assert head['header']['level'] == EXPECTED_LEVEL


def restart(sandbox, node_id):
    sandbox.node(node_id).terminate_or_kill()
    sandbox.node(node_id).run()
    assert sandbox.client(node_id).check_node_listening()


def expect_wrong_version(sandbox, node):
    pattern = 'Found \'0.0.4\', expected \'0.0.5\''
    with utils.assert_run_failure(pattern):
        sandbox.init_node(node, snapshot=None, reconstruct=False)


def remove_lmdb(node):
    shutil.rmtree(node.node_dir + '/lmdb_store_to_remove')


def clean(node):
    shutil.rmtree(node.node_dir)


MAP = {
    "batch": BATCH,
    "home": paths.TEZOS_HOME,
    "snapshot": True,
}


@pytest.mark.incremental
@pytest.mark.snapshot
@pytest.mark.slow
@pytest.mark.parametrize('legacy_stores', [MAP], indirect=True)
class TestLegacy:

    # Generate legacy stores and export all kind of snapshots
    def test_generate_legacy_stores(self, legacy_stores, session):
        # Store snapshot paths in session
        for history_mode in ['archive', 'full']:
            path_full = legacy_stores[f'from_{history_mode}.full']
            session[f'from_{history_mode}.full'] = path_full
            path_rolling = legacy_stores[f'from_{history_mode}.rolling']
            session[f'from_{history_mode}.rolling'] = path_rolling
        # Store the rolling path
        tmp = legacy_stores['from_rolling.rolling']
        session['from_rolling.rolling'] = tmp
        session['head_level'] = EXPECTED_LEVEL

    ###########################################################################
    # Import all kinds of snapshots
    # New node: 3
    def test_run_full_node_from_archive_1(self, sandbox, legacy_stores):
        file = legacy_stores['from_archive.full']
        sandbox.add_node(3, snapshot=file, params=constants.NODE_PARAMS)

    # New node: 4
    def test_run_rolling_node_from_archive_1(self, sandbox, legacy_stores):
        file = legacy_stores['from_archive.rolling']
        sandbox.add_node(4, snapshot=file, params=constants.NODE_PARAMS)

    # New node 1
    def test_reset_full_node_from_full_1(self, sandbox, legacy_stores):
        file = legacy_stores['from_full.full']
        sandbox.add_node(1, snapshot=file, params=constants.NODE_PARAMS)

    # New node: 5
    def test_run_rolling_node_from_full_1(self, sandbox, legacy_stores):
        file = legacy_stores['from_full.rolling']
        sandbox.add_node(5, snapshot=file, params=constants.NODE_PARAMS)

    # New node 2
    def test_reset_rolling_node_from_rolling_1(self, sandbox, legacy_stores):
        file = legacy_stores['from_rolling.rolling']
        sandbox.add_node(2, snapshot=file, params=constants.NODE_PARAMS)

    ###########################################################################
    # Check consistency of imported snapshots with > 5 cycles

    # For the full nodes
    def test_node_1_consistency_1(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
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

    def test_node_3_consistency_1(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
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
    def test_node_2_consistency_1(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - maxopttl
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

    def test_node_4_consistency_1(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - maxopttl
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

    def test_node_5_consistency_1(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint
        expected_caboose = expected_checkpoint - maxopttl
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

    # Bake a few blocks to check if the Full and Rolling nodes catch up
    def test_bake_to_catch_up(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        for _ in range(BATCH):
            sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        session['head_level'] = sandbox.client(1).get_head()['header']['level']
        for i in GROUP:
            assert utils.check_level(sandbox.client(i), session['head_level'])

    ###########################################################################
    # Check consistency of imported snapshots with > 5 cycles

    # For the full nodes
    def test_node_1_consistency_2(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 1
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level - 2 * 8  # lafl(head)
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - maxopttl
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

    def test_node_3_consistency_2(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 3
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level - 2 * 8  # lafl(head)
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - maxopttl
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
    def test_node_2_consistency_2(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 2
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level - 2 * 8  # lafl(head)
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - maxopttl
        expected_caboose = expected_savepoint
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

    def test_node_4_consistency_2(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 4
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level - 2 * 8  # lafl(head)
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - maxopttl
        expected_caboose = expected_savepoint
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

    def test_node_5_consistency_2(self, sandbox, session, legacy_stores):
        # pylint: disable=unused-argument
        node_id = 5
        restart(sandbox, node_id)
        expected_level = session['head_level']
        expected_checkpoint = expected_level - 2 * 8  # lafl(head)
        head = sandbox.client(node_id).get_head()
        maxopttl = head['metadata']['max_operations_ttl']
        expected_savepoint = expected_checkpoint - maxopttl
        expected_caboose = expected_savepoint
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
