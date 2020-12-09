import time

import pytest

from launchers.sandbox import Sandbox
from tools import constants, utils
from tools.constants import PROTO_GENESIS
from . import protocol

MIGRATION_LEVEL = 3
BAKER = 'bootstrap1'
BAKER_PKH = constants.IDENTITIES[BAKER]['identity']
PREV_DEPOSIT = protocol.PREV_PARAMETERS["block_security_deposit"]
DEPOSIT = protocol.PARAMETERS["block_security_deposit"]

PREV_DEPOSIT_RECEIPTS = [
    {"kind": "contract", "contract": BAKER_PKH, "change": "-" + PREV_DEPOSIT},
    {
        "kind": "freezer",
        "category": "deposits",
        "delegate": BAKER_PKH,
        "cycle": 0,
        "change": PREV_DEPOSIT,
    },
]
# in protocol Alpha, the "origin" field is added
DEPOSIT_RECEIPTS = [
    {
        "kind": "contract",
        "contract": BAKER_PKH,
        "change": "-" + DEPOSIT,
        "origin": "block",
    },
    {
        "kind": "freezer",
        "category": "deposits",
        "delegate": BAKER_PKH,
        "cycle": 0,
        "change": DEPOSIT,
        "origin": "block",
    },
]


MIGRATION_RECEIPTS = [
    {
        "kind": "contract",
        "contract": 'tz1abmz7jiCV2GH2u81LRrGgAFFgvQgiDiaf',
        "change": "100000000",
        "origin": "migration",
    },
]


# configure user-activate-upgrade at MIGRATION_LEVEL to test migration
NODE_CONFIG = {
    'network': {
        'genesis': {
            'timestamp': '2018-06-30T16:07:32Z',
            'block': 'BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2',
            'protocol': PROTO_GENESIS,
        },
        'genesis_parameters': {
            'values': {'genesis_pubkey': constants.GENESIS_PK}
        },
        'chain_name': 'TEZOS',
        'sandboxed_chain_name': 'SANDBOXED_TEZOS',
        'user_activated_upgrades': [
            {'level': MIGRATION_LEVEL, 'replacement_protocol': protocol.HASH}
        ],
    }
}


@pytest.fixture(scope="class")
def client(sandbox):
    sandbox.add_node(0, node_config=NODE_CONFIG)
    protocol.activate(
        sandbox.client(0),
        proto=protocol.PREV_HASH,
        parameters=protocol.PREV_PARAMETERS,
        activate_in_the_past=True,
    )
    yield sandbox.client(0)


@pytest.mark.incremental
class TestMigration:
    """Test migration from PROTO_A (the previous protocol) to PROTO_B (the
    current protocol).

    After migration, test snapshots:
        - node0: activate PROTO_A, migrate to PROTO_B, bake, export
                 a snapshot in full and rolling modes, and terminate
        - node1: import full, bake
        - node2: import rolling, sync, bake
        - node3: reconstruct full, sync, bake
        - all 4 are synced
    """

    def test_init(self, client):
        # 1: genesis block
        client.get_head()
        client.rpc('get', '/config/network/user_activated_upgrades')

    def test_activate(self, client, sandbox):
        # 2: activated PROTO_A
        utils.bake(client, BAKER)
        assert client.get_protocol() == protocol.PREV_HASH
        assert sandbox.client(0).get_head()['header']['proto'] == 1
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == PREV_DEPOSIT_RECEIPTS
        # PROTO_A is using env. V1, metadata hashes should be present
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()

    def test_migration(self, client, sandbox):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        utils.bake(client, BAKER)
        metadata = client.get_metadata()
        assert metadata['next_protocol'] == protocol.HASH
        assert metadata['balance_updates'] == PREV_DEPOSIT_RECEIPTS
        # PROTO_B is using env. V1, metadata hashes should be present
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()
        assert sandbox.client(0).get_head()['header']['proto'] == 2

    def test_new_proto(self, client, sandbox):
        # 4: first block of PROTO_B
        utils.bake(client, BAKER)
        assert client.get_protocol() == protocol.HASH
        assert sandbox.client(0).get_head()['header']['proto'] == 2

        # check that migration balance update appears in receipts
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == (
            MIGRATION_RECEIPTS + DEPOSIT_RECEIPTS
        )
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()

    def test_new_proto_second(self, client):
        # 5: second block of PROTO_B
        utils.bake(client, BAKER)
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS

    def test_terminate_node0(self, client, sandbox: Sandbox, session: dict):
        # to export rolling snapshot, we need to be at level > 60
        # (see `max_operations_ttl`)
        level = client.get_head()['header']['level']
        for _ in range(60 - level + 1):
            utils.bake(client, BAKER)
        assert client.get_head()['header']['level'] == 61

        # terminate node0
        session['head_hash'] = sandbox.client(0).get_head()['hash']
        sandbox.node(0).terminate()
        time.sleep(1)

    def test_export_snapshots(self, sandbox, tmpdir, session: dict):
        node_export = sandbox.node(0)
        file_full = f'{tmpdir}/FILE.full'
        file_rolling = f'{tmpdir}/FILE.rolling'
        head_hash = session['head_hash']
        session['snapshot_full'] = file_full
        session['snapshot_rolling'] = file_rolling
        node_export.snapshot_export(file_full, params=['--block', head_hash])
        node_export.snapshot_export(
            file_rolling, params=['--block', head_hash, '--rolling']
        )

    def test_import_full_snapshot_node1(self, sandbox, session):
        sandbox.add_node(
            1, snapshot=session['snapshot_full'], node_config=NODE_CONFIG
        )
        client = sandbox.client(1)
        utils.bake(client, BAKER)

    def test_import_rolling_snapshot_node2(self, sandbox, session):
        sandbox.add_node(
            2, snapshot=session['snapshot_rolling'], node_config=NODE_CONFIG
        )
        client = sandbox.client(2)
        utils.synchronize([sandbox.client(1), client], max_diff=0)
        utils.bake(client, BAKER)

    def test_reconstruct_full_node3(self, sandbox, session):
        sandbox.add_node(
            3, snapshot=session['snapshot_full'], node_config=NODE_CONFIG
        )
        sandbox.node(3).terminate()
        time.sleep(3)
        sandbox.node(3).reconstruct()
        sandbox.node(3).run()
        client = sandbox.client(3)
        assert client.check_node_listening()
        utils.synchronize(
            [sandbox.client(1), sandbox.client(2), client], max_diff=0
        )
        utils.bake(client, BAKER)

    def test_rerun_node0(self, sandbox):
        sandbox.node(0).run()
        sandbox.client(0).check_node_listening()
        utils.synchronize(sandbox.all_clients(), max_diff=0)
