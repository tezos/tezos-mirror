import time

import pytest

from launchers.sandbox import Sandbox
from tools import constants, utils
from tools.constants import PROTO_GENESIS
from . import protocol

MIGRATION_LEVEL = 8
BAKER = 'bootstrap1'
BAKER_PKH = constants.IDENTITIES[BAKER]['identity']
BAKER_BALANCE = next(
    bal for [BAKER_PKH, bal] in protocol.PARAMETERS["bootstrap_accounts"]
)

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
    sandbox.add_node(0, params=constants.NODE_PARAMS, node_config=NODE_CONFIG)
    protocol.activate(
        sandbox.client(0),
        proto=protocol.PREV_HASH,
        parameters=protocol.PREV_PARAMETERS,
        activate_in_the_past=True,
    )
    yield sandbox.client(0)


all_bootstrap_accounts = [f"bootstrap{i}" for i in range(1, 6)]


def endorse_all(client, endorse="endorse"):
    cmd = [endorse, "for"] + all_bootstrap_accounts + ["--force"]
    client.run(cmd)


def manual_bake(client, baker):
    """Tenderbake baking using propose/preendorse/endorse

    Using the 3 lower level steps instead of `bake for` allows to control who
    bakes while (pre)endorsing with all known accounts. Such fine-grained
    control cannot be achieved through `bake for`.

    """
    client.propose([baker], ["--minimal-timestamp"])
    endorse_all(client, endorse="preendorse")
    endorse_all(client)


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

    def test_migration(self, client, sandbox):
        # 3: last block of PROTO_A, runs migration code (MIGRATION_LEVEL)
        for _i in range(MIGRATION_LEVEL - 2):
            utils.bake(client, BAKER)
        metadata = client.get_metadata()
        assert metadata['next_protocol'] == protocol.HASH
        assert sandbox.client(0).get_head()['header']['proto'] == 2

    def test_new_proto(self, client, sandbox):
        # 4: first block of PROTO_B
        manual_bake(client, BAKER)
        assert client.get_protocol() == protocol.HASH
        assert sandbox.client(0).get_head()['header']['proto'] == 2

    def test_new_proto_second(self, client):
        # 5: second block of PROTO_B
        manual_bake(client, BAKER)

    def test_terminate_node0(self, client, sandbox: Sandbox, session: dict):
        # to export rolling snapshot, we need to be at level > 60
        # (see `max_operations_ttl`)
        level = client.get_head()['header']['level']
        for _ in range(60 - level + 1):
            manual_bake(client, BAKER)
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
        client.multibake(args=["--minimal-timestamp"])

    def test_import_rolling_snapshot_node2(self, sandbox, session):
        sandbox.add_node(
            2,
            snapshot=session['snapshot_rolling'],
            params=constants.NODE_PARAMS,
            node_config=NODE_CONFIG,
        )
        client = sandbox.client(2)
        utils.synchronize([sandbox.client(1), client], max_diff=0)
        client.multibake(args=["--minimal-timestamp"])

    def test_reconstruct_full_node3(self, sandbox, session):
        sandbox.add_node(
            3,
            snapshot=session['snapshot_full'],
            node_config=NODE_CONFIG,
            params=constants.NODE_PARAMS,
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
        client.multibake(args=["--minimal-timestamp"])

    def test_rerun_node0(self, sandbox):
        sandbox.node(0).run()
        sandbox.client(0).check_node_listening()
        utils.synchronize(sandbox.all_clients(), max_diff=0)
