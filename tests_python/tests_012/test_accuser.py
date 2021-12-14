import time

import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol


NUM_NODES = 2


@pytest.mark.multinode
@pytest.mark.incremental
class TestAccuser:
    """Constructs a double endorsement, and lets the accuser inject
    the evidence."""

    def test_init(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
        protocol.activate(sandbox.client(0), activate_in_the_past=True)
        # We inject 3 blocks so that the double-endorsement-evidence operation
        # is always branched from a known predecessor. (If the level of the
        # evidence is smaller than 5, then the operation is branched from the
        # head; however the node might change branch, and then the operation
        # becomes invalid, namely "branch refused".)
        for i in range(3):
            utils.bake(sandbox.client(0))

    def test_level(self, sandbox: Sandbox):
        level = 4
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_terminate_node_1(self, sandbox: Sandbox):
        sandbox.node(1).terminate()

    def test_bake_node_0(self, sandbox: Sandbox):
        """Client 0 bakes block A at level 5, not communicated to node 1.
        Inject an operation (transfer) to ensure a different hash"""
        sandbox.client(0).transfer(1, 'bootstrap4', 'bootstrap5')
        sandbox.client(0).propose(
            delegates=['bootstrap1'], args=['--minimal-timestamp']
        )

    def test_endorse_node_0(self, sandbox: Sandbox, session: dict):
        """bootstrap1 builds an endorsement for block A"""
        client = sandbox.client(0)
        client.run(["endorse", "for", 'bootstrap3', '--force'])
        mempool = client.get_mempool()
        endorsement = mempool['applied'][0]
        session['endorsement1'] = endorsement
        utils.bake(client, bake_for='bootstrap2')

    def test_terminate_node_0(self, sandbox: Sandbox):
        sandbox.node(0).terminate()

    def test_restart_node_1(self, sandbox: Sandbox):
        sandbox.node(1).run()
        assert sandbox.client(1).check_node_listening()

    def test_start_accuser(self, sandbox: Sandbox):
        sandbox.add_accuser(1, proto=protocol.DAEMON)
        # We make sure that there is enough time for the accuser to start
        # listing to the node's block stream, otherwise the accuser might miss
        # the next two blocks.
        time.sleep(2)

    def test_bake_node_1(self, sandbox: Sandbox):
        """Client 1 bakes block B at level 5, not communicated to node 0"""
        sandbox.client(1).propose(
            delegates=['bootstrap1'], args=['--minimal-timestamp']
        )

    def test_endorse_node_2(self, sandbox: Sandbox, session: dict):
        """bootstrap1 builds an endorsement for block B, which is included in
        a new block at level 6 by bootstrap2"""
        client = sandbox.client(1)
        client.run(["endorse", "for", 'bootstrap3', "--force"])
        mempool = client.get_mempool()
        endorsement = mempool['applied'][0]
        session['endorsement2'] = endorsement
        utils.bake(client, bake_for='bootstrap2')
        mempool = client.get_mempool()
        client.get_operations("4")

    def test_restart_node_0(self, sandbox: Sandbox):
        sandbox.node(0).run()
        sandbox.client(0).check_node_listening()

    def test_check_level(self, sandbox: Sandbox):
        """All nodes are at level 6, head is either block A or B"""
        level = 6
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_bake_block(self, sandbox: Sandbox):
        """Bake a block on node 0, which makes the chain on node 0 longer; in
        this way, we make sure that node 1 sees the block at level 6,
        which containts the conflicting endorsement.
        """
        utils.bake(sandbox.client(0))

    @pytest.mark.xfail(reason="Works locally - CI fails")
    def test_double_baking_evidence_generated(self, sandbox: Sandbox):
        """Check that a double baking evidence operation is in the
        mempool of node 1 or in the block at level 7, depending on
        whether the double endorsement operation has reached node 1
        before or after node 1 sees the block at level 7.
        """
        in_mempool = False
        in_block = False

        mempool = sandbox.client(1).get_mempool()
        applied = mempool['applied']
        evidence_kind = "double_baking_evidence"

        if len(applied) > 0 and len(applied[0]['contents']) > 0:
            in_mempool = applied[0]['contents'][0]['kind'] == evidence_kind

        if not in_mempool:
            ops = sandbox.client(1).get_operations()
            if len(ops[2]) > 0 and len(ops[2][0]['contents']) > 0:
                in_block = ops[2][0]['contents'][0]['kind'] == evidence_kind

        assert in_mempool or in_block
