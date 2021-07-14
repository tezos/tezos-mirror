from typing import Dict, List
import copy

import pytest

from launchers.sandbox import Sandbox
from tools import constants, paths, utils
from tools.constants import PROTO_GENESIS
from . import protocol

MIGRATION_LEVEL = 8
BAKER = 'bootstrap1'
BAKER_PKH = constants.IDENTITIES[BAKER]['identity']
PREV_DEPOSIT = protocol.PREV_PARAMETERS["block_security_deposit"]
DEPOSIT = protocol.PARAMETERS["block_security_deposit"]

PREV_DEPOSIT_RECEIPTS = [
    {
        "kind": "contract",
        "contract": BAKER_PKH,
        "change": "-" + PREV_DEPOSIT,
        "origin": "block",
    },
    {
        "kind": "freezer",
        "category": "deposits",
        "delegate": BAKER_PKH,
        "cycle": 0,
        "change": PREV_DEPOSIT,
        "origin": "block",
    },
]
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
        "cycle": 1,
        "change": DEPOSIT,
        "origin": "block",
    },
]
MIGRATION_RECEIPTS: List[Dict[str, str]] = []


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

# the same configuration without user-activate-upgrade
NODE_CONFIG_NOUPGRADE = copy.deepcopy(NODE_CONFIG)
NODE_CONFIG_NOUPGRADE['network'].pop('user_activated_upgrades')


@pytest.fixture(scope="class")
def sandbox_noupgrade(singleprocess):
    """Sandboxed network of nodes.

    Nodes, bakers and endorsers are added/removed dynamically."""
    with Sandbox(
        paths.TEZOS_HOME,
        constants.IDENTITIES,
        rpc=20730,
        p2p=21730,
        singleprocess=singleprocess,
    ) as sandbox:
        yield sandbox
        sandbox.are_daemons_alive()


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


@pytest.fixture(scope="class")
def client_noupgrade(sandbox_noupgrade):
    sandbox_noupgrade.add_node(0, node_config=NODE_CONFIG_NOUPGRADE)
    protocol.activate(
        sandbox_noupgrade.client(0),
        proto=protocol.PREV_HASH,
        parameters=protocol.PREV_PARAMETERS,
        activate_in_the_past=True,
    )
    yield sandbox_noupgrade.client(0)


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

    def test_init(self, client, client_noupgrade):
        # 1: genesis block
        client.get_head()
        client.rpc('get', '/config/network/user_activated_upgrades')
        client_noupgrade.get_head()
        client_noupgrade.rpc('get', '/config/network/user_activated_upgrades')

    def impl_activate(self, client, sandbox):
        # 2: activated PROTO_A
        utils.bake(client, BAKER)
        assert client.get_protocol() == protocol.PREV_HASH
        assert sandbox.client(0).get_head()['header']['proto'] == 1
        metadata = client.get_metadata()
        assert metadata['balance_updates'] == PREV_DEPOSIT_RECEIPTS

    def test_activate(
        self, client, sandbox, client_noupgrade, sandbox_noupgrade
    ):
        self.impl_activate(client, sandbox)
        self.impl_activate(client_noupgrade, sandbox_noupgrade)

    def test_flattening(self, client, client_noupgrade):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        for _i in range(MIGRATION_LEVEL - 2):
            utils.bake(client, BAKER)
            utils.bake(client_noupgrade, BAKER)

        # test path flattening migration
        def check_rpc_equal(path, compare_on=None):
            result_noupgrade = client_noupgrade.rpc('get', path)
            result = client.rpc('get', path)
            if compare_on is None:
                assert result_noupgrade == result
            else:
                assert compare_on(result_noupgrade) == compare_on(result)

        block_root = '/chains/main/blocks/head/'
        json_root = f'{block_root}/context/raw/json'
        contracts = client_noupgrade.rpc(
            'get', f'{block_root}/context/contracts'
        )
        assert contracts != []
        for contract in contracts:
            check_rpc_equal(f'{block_root}/context/contracts/{contract}')
        check_rpc_equal(f'{json_root}/rolls/index', sorted)
        check_rpc_equal(f'{json_root}/rolls/index/1234')
        check_rpc_equal(f'{json_root}/commitments')
        check_rpc_equal(f'{block_root}/votes/listings')
        check_rpc_equal(f'{block_root}/votes/ballots')
        check_rpc_equal(f'{block_root}/context/delegates')

        metadata = client.get_metadata()
        assert metadata['next_protocol'] == protocol.HASH
        assert metadata['balance_updates'] == PREV_DEPOSIT_RECEIPTS
