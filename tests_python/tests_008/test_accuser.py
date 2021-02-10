import time

import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol


BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
NUM_NODES = 3


@pytest.mark.multinode
@pytest.mark.incremental
class TestAccuser:
    """Constructs a double endorsement, and lets the accuser inject
    the evidence."""

    def test_init(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
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
        client.bake('bootstrap1', BAKE_ARGS)

    def test_terminate_node_0(self, sandbox: Sandbox):
        sandbox.node(0).terminate()

    def test_restart_node_2(self, sandbox: Sandbox):
        sandbox.node(2).run()
        assert sandbox.client(2).check_node_listening()

    def test_start_accuser(self, sandbox: Sandbox):
        sandbox.add_accuser(2, proto=protocol.DAEMON)

    def test_bake_node_2(self, sandbox: Sandbox):
        """Client 2 bakes block B at level 3, not communicated to 0 and 1"""
        sandbox.client(2).bake('bootstrap1', BAKE_ARGS)

    def test_endorse_node_2(self, sandbox: Sandbox, session: dict):
        """bootstrap1 builds an endorsement for block B, which is included in
        a new block at level 4 by bootstrap2"""
        client = sandbox.client(2)
        client.endorse('bootstrap1')
        mempool = client.get_mempool()
        endorsement = mempool['applied'][0]
        session['endorsement2'] = endorsement
        client.bake('bootstrap2', BAKE_ARGS)

    def test_restart_all(self, sandbox: Sandbox):
        sandbox.node(0).run()
        sandbox.node(1).run()
        sandbox.client(0).check_node_listening()
        sandbox.client(1).check_node_listening()

    def test_check_level(self, sandbox: Sandbox):
        """All nodes are at level 4, head is either block A or B"""
        level = 4
        for client in sandbox.all_clients():
            assert utils.check_level(client, level)

    def test_bake_block(self, sandbox: Sandbox):
        """bake a block on node 0, which makes the chain on node 0 longer; in
        this way, we make sure that node 2 sees the block at level 4,
        which containts the conflicting endorsement
        """
        sandbox.client(0).bake('bootstrap1', BAKE_ARGS)

    def test_double_endorsement_evidence_generated(self, sandbox: Sandbox):
        """Check double endorsement evidence operation is in the mempool of
        node 2 or in the block at level 5, depending on whether the
        double endorsement operation has reached node 2 before or
        after node 2 sees the block at level 5.
        """
        time.sleep(1)
        in_mempool = False
        in_block = False
        mempool = sandbox.client(2).get_mempool()
        if (
            len(mempool['applied']) > 0
            and len(mempool['applied'][0]['contents']) > 0
        ):
            in_mempool = (
                mempool['applied'][0]['contents'][0]['kind']
                == "double_endorsement_evidence"
            )
        if not in_mempool:
            ops = sandbox.client(2).get_operations()
            if len(ops[2]) > 0 and len(ops[2][0]['contents']) > 0:
                in_block = (
                    ops[2][0]['contents'][0]['kind']
                    == "double_endorsement_evidence"
                )
        assert in_mempool or in_block
