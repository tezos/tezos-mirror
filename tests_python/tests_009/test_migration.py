import datetime
import json
import os
import time
from typing import List

import pytest

from launchers.sandbox import Sandbox
from tools import constants, paths, utils
from tools.constants import PROTO_GENESIS

BAKE_ARGS = [
    '--minimal-fees',
    '0',
    '--minimal-nanotez-per-byte',
    '0',
    '--minimal-nanotez-per-gas-unit',
    '0',
    '--max-priority',
    '512',
    '--minimal-timestamp',
]
PROTO_A = constants.EDO
PROTO_A_DAEMON = constants.EDO_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"
PROTO_B = constants.ALPHA

PARAMETERS_FILE = (
    f'{paths.TEZOS_HOME}src/{PROTO_A_PATH}/parameters/' 'test-parameters.json'
)
assert os.path.isfile(PARAMETERS_FILE), (
    f'{PARAMETERS_FILE}'
    ' cannot be found; please first run'
    ' `make` in {paths.TEZOS_HOME}.'
)
with open(PARAMETERS_FILE) as f:
    PARAMETERS = dict(json.load(f))
MIGRATION_LEVEL = 3
BAKER = 'bootstrap1'

BAKER_PKH = constants.IDENTITIES[BAKER]['identity']
DEPOSIT_RECEIPTS = [
    {"kind": "contract", "contract": BAKER_PKH, "change": "-512000000"},
    {
        "kind": "freezer",
        "category": "deposits",
        "delegate": BAKER_PKH,
        "cycle": 0,
        "change": "512000000",
    },
]
# in Alpha protocol, the "origin" field is added
ALPHA_DEPOSIT_RECEIPTS = [
    {
        "kind": "contract",
        "contract": BAKER_PKH,
        "change": "-512000000",
        "origin": "block",
    },
    {
        "kind": "freezer",
        "category": "deposits",
        "delegate": BAKER_PKH,
        "cycle": 0,
        "change": "512000000",
        "origin": "block",
    },
]
MIGRATION_RECEIPTS: List[object] = [
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
            {'level': MIGRATION_LEVEL, 'replacement_protocol': PROTO_B}
        ],
    }
}


@pytest.fixture(scope="class")
def client(sandbox):
    sandbox.add_node(0, node_config=NODE_CONFIG)
    delay = datetime.timedelta(seconds=3600 * 24 * 365)
    sandbox.client(0).activate_protocol_json(PROTO_A, PARAMETERS, delay=delay)

    yield sandbox.client(0)


@pytest.mark.incremental
class TestMigration:
    """Test migration from PROTO_A to PROTO_B."""

    def test_init(self, client):
        # 1: genesis block
        client.get_head()
        client.rpc('get', '/config/network/user_activated_upgrades')

    def test_activate(self, client, sandbox):
        # 2: activated PROTO_A
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == PROTO_A
        assert sandbox.client(0).get_head()['header']['proto'] == 1
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS
        # PROTO_A is using env. V1, metadata hashes should be present
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()

    def test_migration(self, client, sandbox):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        assert metadata['next_protocol'] == PROTO_B
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS
        # PROTO_B is using env. V1, metadata hashes should be present
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()
        assert sandbox.client(0).get_head()['header']['proto'] == 2

    def test_new_proto(self, client, sandbox):
        # 4: first block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == PROTO_B
        assert sandbox.client(0).get_head()['header']['proto'] == 2

        # check that migration balance update appears in receipts
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == (
            MIGRATION_RECEIPTS + ALPHA_DEPOSIT_RECEIPTS
        )
        _ops_metadata_hash = client.get_operations_metadata_hash()
        _block_metadata_hash = client.get_block_metadata_hash()

    def test_new_proto_second(self, client):
        # 5: second block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == ALPHA_DEPOSIT_RECEIPTS

    def test_terminate_node0(self, client, sandbox: Sandbox, session: dict):
        # # to export rolling snapshot, we need to be at level > 60
        # (see `max_operations_ttl`)
        level = client.get_head()['header']['level']
        for _ in range(60 - level + 1):
            client.bake(BAKER, BAKE_ARGS)
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
        client.bake(BAKER, BAKE_ARGS)

    def test_import_rolling_snapshot_node2(self, sandbox, session):
        sandbox.add_node(
            2, snapshot=session['snapshot_rolling'], node_config=NODE_CONFIG
        )
        client = sandbox.client(2)
        utils.synchronize([sandbox.client(1), client], max_diff=0)
        client.bake(BAKER, BAKE_ARGS)

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
        client.bake(BAKER, BAKE_ARGS)

    def test_rerun_node0(self, sandbox):
        sandbox.node(0).run()
        sandbox.client(0).check_node_listening()
        utils.synchronize(sandbox.all_clients(), max_diff=0)
