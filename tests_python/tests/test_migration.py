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


def sort(list_of_dicts):
    # convert the dictionaries in the list to tuples
    return sorted(tuple(sorted(d.items())) for d in list_of_dicts)


DEPOSIT_RECEIPTS = sort([
    {
        "kind": "contract",
        "contract": BAKER_PKH,
        "change": "-512000000"},
    {
        "kind": "freezer", "category": "deposits",
        "delegate": BAKER_PKH, "cycle": 0,
        "change": "512000000"}])


def proto_b_invoice_receipts(client):
    bootstrap1_migrated_address = client.find_baker_with_consensus_key(
        'bootstrap1')
    # invoice for bootstrap1 contract
    return [{
        "kind": "contract",
        "contract": bootstrap1_migrated_address,
        "change": str(662_607_015)}]


def proto_b_deposit_receipts(baker_hash):
    return [
        {
            "kind": "contract",
            "contract": baker_hash,
            "change": "-512000000"},
        {
            "kind": "freezer", "category": "deposits",
            "baker": baker_hash, "cycle": 0,
            "change": "512000000"}]


# Get the receipts for expected moved balances from the original implicit
# baker contracts to SG1 baker contracts
def baker_migration_receipts(client, baker_hash):
    # the baker has the bootstrap balance minus 2 deposits
    bootstrap_balance = 4_000_000_000_000
    deposits = 2
    change = bootstrap_balance - deposits * 512_000_000
    receipts = [
        {
            "kind": "contract",
            "contract": BAKER_PKH,
            "change": str(-change)},
        {
            "kind": "contract",
            "contract": baker_hash,
            "change": str(change)}]

    for i in range(2, 6):
        proto_a_name = f'bootstrap{i}'
        pkh = constants.IDENTITIES[proto_a_name]['identity']
        baker = client.find_baker_with_consensus_key(proto_a_name)
        receipts += [
            {
                "kind": "contract",
                "contract": pkh,
                "change": str(-bootstrap_balance)},
            {
                "kind": "contract",
                "contract": baker,
                "change": str(bootstrap_balance)}]
    return receipts


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
        balance_updates = sort(metadata['balance_updates'])
        assert balance_updates == DEPOSIT_RECEIPTS
        # PROTO_A is using env. V0, metadata hashes should not be present
        ops_metadata_hash = client.get_operations_metadata_hash()
        assert ops_metadata_hash is None
        block_metadata_hash = client.get_block_metadata_hash()
        assert block_metadata_hash is None

    def test_migration(self, client):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        assert metadata['next_protocol'] == PROTO_B
        balance_updates = sort(metadata['balance_updates'])
        assert balance_updates == DEPOSIT_RECEIPTS
        # PROTO_B is using env. V1, metadata hashes should be present
        ops_metadata_hash = client.get_operations_metadata_hash()
        assert ops_metadata_hash is not None
        block_metadata_hash = client.get_block_metadata_hash()
        assert block_metadata_hash is not None

    def test_new_proto(self, client):
        # 4: first block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        assert client.get_protocol() == PROTO_B

        # check that migration balance update appears in receipts
        metadata = client.get_metadata()
        baker_hash = client.find_baker_with_consensus_key(BAKER)
        balance_updates = sort(metadata['balance_updates'])
        expected_receipts = sort(proto_b_deposit_receipts(baker_hash) +
                                 proto_b_invoice_receipts(client) +
                                 baker_migration_receipts(client, baker_hash))
        msg = f"expected:\n{expected_receipts}\n\ngot:\n{balance_updates}"
        assert expected_receipts == balance_updates, msg
        ops_metadata_hash = client.get_operations_metadata_hash()
        assert ops_metadata_hash is not None
        block_metadata_hash = client.get_block_metadata_hash()
        assert block_metadata_hash is not None

    def test_new_proto_second(self, client):
        # 5: second block of PROTO_B
        client.bake(BAKER, BAKE_ARGS)
        metadata = client.get_metadata()
        baker_hash = client.find_baker_with_consensus_key(BAKER)
        balance_updates = sort(metadata['balance_updates'])
        expected_receipts = sort(proto_b_deposit_receipts(baker_hash))
        msg = f"expected:\n{expected_receipts}\n\ngot:\n{balance_updates}"
        assert expected_receipts == balance_updates, msg
