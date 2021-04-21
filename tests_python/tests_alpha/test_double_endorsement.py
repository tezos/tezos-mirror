import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
NUM_NODES = 3
PARAMS = constants.NODE_PARAMS
BLOCKS_PER_CYCLE = 2
PATH_FORGE_OPERATION = '/chains/main/blocks/head/helpers/forge/operations'


@pytest.mark.multinode
@pytest.mark.incremental
class TestDoubleEndorsement:
    """Constructs a double endorsement and builds evidence."""

    def test_init(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=PARAMS)
        protocol.activate(sandbox.client(0), activate_in_the_past=True)
        sandbox.client(0).bake('bootstrap1', BAKE_ARGS)

    def test_level(self, sandbox: Sandbox):
        level = 2
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_terminate_nodes_1_and_2(self, sandbox: Sandbox):
        sandbox.node(1).terminate()
        sandbox.node(2).terminate()

    def test_bake_node_0(self, sandbox: Sandbox):
        """Client 0 bakes block A at level 3, not communicated to 1 and 2
        Inject an endorsement to ensure a different hash"""
        sandbox.client(0).endorse('bootstrap1')
        sandbox.client(0).bake('bootstrap1', BAKE_ARGS)

    def test_endorse_node_0(self, sandbox: Sandbox, session: dict):
        """bootstrap1 builds an endorsement for block A"""
        client = sandbox.client(0)
        client.endorse('bootstrap1')
        mempool = client.get_mempool()
        endorsement = mempool['applied'][0]
        session['endorsement1'] = endorsement

    def test_terminate_node_0(self, sandbox: Sandbox):
        sandbox.node(0).terminate()

    def test_restart_node_2(self, sandbox: Sandbox):
        sandbox.node(2).run()
        assert sandbox.client(2).check_node_listening()

    def test_bake_node_2(self, sandbox: Sandbox):
        """Client 2 bakes block B at level 3, not communicated to 0 and 1"""
        sandbox.client(2).bake('bootstrap1', BAKE_ARGS)

    def test_endorse_node_2(self, sandbox: Sandbox, session: dict):
        """bootstrap1 builds an endorsement for block B"""
        client = sandbox.client(2)
        client.endorse('bootstrap1')
        mempool = client.get_mempool()
        endorsement = mempool['applied'][0]
        session['endorsement2'] = endorsement
        sandbox.client(2).endorse('bootstrap2')

    def test_restart_all(self, sandbox: Sandbox):
        sandbox.node(0).run()
        sandbox.node(1).run()
        sandbox.client(0).check_node_listening()
        sandbox.client(1).check_node_listening()

    def test_check_level(self, sandbox: Sandbox):
        """All nodes are at level 3, head is either block A or B"""
        level = 3
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_forge_accusation(self, sandbox: Sandbox, session: dict):
        """Forge and inject a double endorsement evidence operation"""
        client = sandbox.client(1)
        head_hash = client.get_head()['hash']

        # Extract the `Endorsement` ops and the slot out of the
        # `Endorsement_with_slot` ops
        endorsement1 = session['endorsement1']['contents'][0]['endorsement']
        endorsement2 = session['endorsement2']['contents'][0]['endorsement']
        slot = session['endorsement1']['contents'][0]['slot']

        operation = {
            'branch': head_hash,
            'contents': [
                {
                    'kind': 'double_endorsement_evidence',
                    'op1': endorsement1,
                    'op2': endorsement2,
                    'slot': slot,
                }
            ],
        }

        operation_hex_string = client.rpc(
            'post', PATH_FORGE_OPERATION, data=operation
        )
        assert isinstance(operation_hex_string, str)
        sender_sk_long = constants.IDENTITIES['bootstrap1']['secret']
        sender_sk = sender_sk_long[len('unencrypted:') :]
        signed_op = utils.sign_operation(operation_hex_string, sender_sk)
        op_hash = client.rpc('post', 'injection/operation', signed_op)
        assert isinstance(op_hash, str)
        session['operation'] = op_hash

    def test_operation_applied(self, sandbox: Sandbox, session: dict):
        """Check operation is in mempool"""
        client = sandbox.client(1)
        assert utils.check_mempool_contains_operations(
            client, [session['operation']]
        )
