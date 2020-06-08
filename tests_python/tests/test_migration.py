import json
import os
from typing import List

import pytest

from tools import constants, paths

BAKE_ARGS = ['--minimal-fees', '0', '--minimal-nanotez-per-byte', '0',
             '--minimal-nanotez-per-gas-unit', '0', '--max-priority', '512',
             '--minimal-timestamp']
PROTO_A = constants.CARTHAGE
PROTO_A_DAEMON = constants.CARTHAGE_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"
PROTO_B = constants.ALPHA

PARAMETERS_FILE = (f'{paths.TEZOS_HOME}src/{PROTO_A_PATH}/parameters/'
                   'test-parameters.json')
assert os.path.isfile(PARAMETERS_FILE), (f'{PARAMETERS_FILE}'
                                         ' cannot be found; please first run'
                                         ' `make` in {paths.TEZOS_HOME}.')
with open(PARAMETERS_FILE) as f:
    PARAMETERS = dict(json.load(f))
MIGRATION_LEVEL = 3
BAKER = 'bootstrap1'

BAKER_PKH = constants.IDENTITIES[BAKER]['identity']
DEPOSIT_RECEIPTS = [
    {
        "kind": "contract",
        "contract": BAKER_PKH,
        "change": "-512000000"},
    {
        "kind": "freezer", "category": "deposits",
        "delegate": BAKER_PKH, "cycle": 0,
        "change": "512000000"}]
MIGRATION_RECEIPTS: List[object] = [
    # invoice for bootstrap1 contract
    {
        "kind": "contract",
        "contract": constants.IDENTITIES['bootstrap1']['identity'],
        "change": str(662_607_015)}]


@pytest.fixture(scope="class")
def client(sandbox):
    # configure user-activate-upgrade at MIGRATION_LEVEL to test migration
    node_config = {
        'network': {
            'genesis': {
                'timestamp':
                '2018-06-30T16:07:32Z',
                'block':
                'BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2',
                'protocol':
                'ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im',
            },
            'genesis_parameters': {
                'values': {
                    'genesis_pubkey': constants.GENESIS_PK
                }
            },
            'chain_name': 'TEZOS',
            'sandboxed_chain_name': 'SANDBOXED_TEZOS',
            'user_activated_upgrades':
            [{'level': MIGRATION_LEVEL, 'replacement_protocol': PROTO_B}]}}

    sandbox.add_node(0, node_config=node_config)
    sandbox.client(0).activate_protocol_json(PROTO_A, PARAMETERS)

    yield sandbox.client(0)


@pytest.mark.incremental
class TestMigration:
    """Test migration from PROTO_A to PROTO_B.
    """

    def test_init(self, client):
        # 1: genesis block
        client.get_head()
        client.rpc('get', '/config/network/user_activated_upgrades')

    def test_activate(self, client):
        # 2: activated PROTO_A
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == PROTO_A
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS

    def test_migration(self, client):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        assert metadata['next_protocol'] == PROTO_B
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS

    def test_new_proto(self, client):
        # 4: first block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == PROTO_B

        # check that migration balance update appears in receipts
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == (DEPOSIT_RECEIPTS +
                                               MIGRATION_RECEIPTS)

    def test_new_proto_second(self, client):
        # 5: second block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == DEPOSIT_RECEIPTS
