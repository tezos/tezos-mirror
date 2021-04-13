import pytest
from tools import utils, paths

# Defines the number of blocks to bake in the following test. This
# constant should be higher than max_opttl and should be a multiple of
# the cycle length (8 in sandboxed mode)
BATCH = 160

EXPECTED_LEVEL = BATCH + 1

# FIXME: How to get this value?
MAX_OPTTL = 120

# checkpoint = lafl(head)
EXPECTED_CHECKPOINT = EXPECTED_LEVEL - 16
# savepoint = checkpoint (legacy's Full limitations)
EXPECTED_SAVEPOINT = EXPECTED_LEVEL - 16
EXPECTED_CABOOSE = 0
# savepoint - maxopttl(cp)
EXPECTED_ROLLING_CABOOSE = EXPECTED_SAVEPOINT - MAX_OPTTL

EXPECTED_SERVICE_ERROR = 'Did not find service'
EXPECTED_COMMAND_ERROR = 'Command failed : Unable to find block'


def check_expected_values(head):
    assert head['header']['level'] == EXPECTED_LEVEL


def restart(sandbox, node_id):
    sandbox.node(node_id).run()
    assert sandbox.client(node_id).check_node_listening()


def expect_wrong_version(sandbox, node):
    pattern = "Found '0.0.4', expected '0.0.5'"
    with utils.assert_run_failure(pattern):
        sandbox.init_node(node, snapshot=None, reconstruct=False)


MAP = {
    "batch": BATCH,
    "home": paths.TEZOS_HOME,
    "snapshot": False,
}


@pytest.mark.incremental
@pytest.mark.snapshot
@pytest.mark.slow
@pytest.mark.parametrize("legacy_stores", [MAP], indirect=True)
class TestLegacy:

    # ARCHIVE
    def test_upgrade_archive(self, sandbox, nodes_legacy_store):
        node1 = nodes_legacy_store['archive']
        # We init the client
        client1 = sandbox.register_client(1, rpc_port=node1.rpc_port)
        expect_wrong_version(sandbox, node1)
        # We now run the storage upgarde
        sandbox.node(1).upgrade_storage()
        # After upgrading, we restart the node
        restart(sandbox, 1)
        sandbox.init_client(client1)

    # Checkpoints
    def test_archive_consistency_1(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        check_expected_values(sandbox.client(1).get_head())
        assert sandbox.client(1).get_savepoint() == 0
        assert sandbox.client(1).get_caboose() == 0

    # All blocks must be available
    def test_archive_consistency_2(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(EXPECTED_LEVEL):
            assert utils.get_block_at_level(sandbox.client(1), i)

    # FULL
    def test_upgrade_full(self, sandbox, nodes_legacy_store):
        node2 = nodes_legacy_store['full']
        # We init the client
        client2 = sandbox.register_client(2, rpc_port=node2.rpc_port)
        expect_wrong_version(sandbox, node2)
        # We now run the storage upgarde
        sandbox.node(2).upgrade_storage()
        # After upgrading, we restart the node
        restart(sandbox, 2)
        sandbox.init_client(client2)

    # Checkpoints
    def test_full_consistency_1(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        check_expected_values(sandbox.client(2).get_head())
        savepoint = sandbox.client(2).get_savepoint()
        assert savepoint == EXPECTED_SAVEPOINT
        caboose = sandbox.client(2).get_caboose()
        assert caboose == 0
        # the metadata of genesis are available
        assert utils.get_block_at_level(sandbox.client(2), 0)

    # All block in [1; CHECKPOINT] must non be available (only headers are)
    def test_full_consistency_2(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(1, EXPECTED_CHECKPOINT):
            with utils.assert_run_failure(EXPECTED_COMMAND_ERROR):
                utils.get_block_metadata_at_level(sandbox.client(2), i)

    # All block headers in [1; CHECKPOINT] must be available
    def test_full_consistency_3(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(1, EXPECTED_CHECKPOINT):
            utils.get_block_header_at_level(sandbox.client(2), i)

    # All blocks in [CHECKPOINT + 1; HEAD] must be available
    def test_full_consistency_4(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(EXPECTED_CHECKPOINT + 1, EXPECTED_LEVEL):
            assert utils.get_block_at_level(sandbox.client(2), i)

    # ROLLING
    def test_upgrade_rolling(self, sandbox, nodes_legacy_store):
        node3 = nodes_legacy_store['rolling']
        # We init the client
        client3 = sandbox.register_client(3, rpc_port=node3.rpc_port)
        expect_wrong_version(sandbox, node3)
        # We now run the storage upgarde
        sandbox.node(3).upgrade_storage()
        # After upgrading, we restart the node
        restart(sandbox, 3)
        sandbox.init_client(client3)

    # Checkpoints
    def test_rolling_consistency_1(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        check_expected_values(sandbox.client(3).get_head())
        savepoint = sandbox.client(3).get_savepoint()
        assert savepoint == EXPECTED_CHECKPOINT
        # In rolling, caboose = savepoint
        caboose = sandbox.client(3).get_caboose()
        assert caboose == EXPECTED_ROLLING_CABOOSE
        # the metadata of genesis are available
        utils.get_block_at_level(sandbox.client(3), 0)

    # All blocks in [1 ; ROLLING_CABOOSE] must not be known
    def test_rolling_consistency_2(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(1, EXPECTED_ROLLING_CABOOSE):
            with utils.assert_run_failure(EXPECTED_SERVICE_ERROR):
                utils.get_block_at_level(sandbox.client(3), i)

    # All blocks in [ROLLING_CABOOSE ; CHECKPOINT] must not be available
    # (only headers are)
    def test_rolling_consistency_3(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(EXPECTED_ROLLING_CABOOSE, EXPECTED_CHECKPOINT):
            with utils.assert_run_failure(EXPECTED_COMMAND_ERROR):
                utils.get_block_metadata_at_level(sandbox.client(3), i)

    # All block headers in [SAVEPOINT ; CHECKPOINT] must be available
    def test_rolling_consistency_4(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(EXPECTED_SAVEPOINT, EXPECTED_CHECKPOINT):
            utils.get_block_header_at_level(sandbox.client(3), i)

    # All blocks in [CHECKPOINT + 1; HEAD] must be available
    def test_rolling_consistency_5(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for i in range(EXPECTED_CHECKPOINT + 1, EXPECTED_LEVEL):
            assert utils.get_block_at_level(sandbox.client(3), i)

    # Bake a few blocks to check if the Full and Rolling nodes catch up
    def test_bake_to_catch_up(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        for _ in range(BATCH):
            utils.bake(sandbox.client(1))

    def test_archive_catch_up(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        head = sandbox.client(1).get_head()
        expected_head = EXPECTED_LEVEL + BATCH
        assert head['header']['level'] == expected_head
        checkpoint = (sandbox.client(1).get_checkpoint())['block']['level']
        assert checkpoint == (expected_head - 2 * 8)
        savepoint = sandbox.client(1).get_savepoint()
        caboose = sandbox.client(1).get_caboose()
        assert savepoint == caboose
        assert caboose == 0

    # We assume that "Full 0 mode" is now in "Full 5 mode"
    def test_full_catch_up(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        head = sandbox.client(2).get_head()
        expected_head = EXPECTED_LEVEL + BATCH
        assert head['header']['level'] == expected_head
        checkpoint = sandbox.client(2).get_checkpoint()['block']['level']
        assert checkpoint == (expected_head - 2 * 8)
        savepoint = sandbox.client(2).get_savepoint()
        assert savepoint == (checkpoint - MAX_OPTTL)
        caboose = sandbox.client(2).get_caboose()
        assert caboose == 0

    # We assume that "Rolling 0 mode" is now in "Rolling 5 mode"
    def test_rolling_catch_up(self, sandbox, nodes_legacy_store):
        # pylint: disable=unused-argument
        head = sandbox.client(3).get_head()
        expected_head = EXPECTED_LEVEL + BATCH
        assert head['header']['level'] == expected_head
        checkpoint = sandbox.client(3).get_checkpoint()['block']['level']
        assert checkpoint == (expected_head - 2 * 8)
        savepoint = sandbox.client(3).get_savepoint()
        assert savepoint == (checkpoint - MAX_OPTTL)
        caboose = sandbox.client(3).get_caboose()
        assert caboose == savepoint
