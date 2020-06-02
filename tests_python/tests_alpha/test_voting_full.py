import json
import os
import time

import pytest

from client import client_output
from launchers.sandbox import Sandbox
from client.client import Client
from tools import utils, constants
from . import protocol

BLOCKS_PER_VOTING_PERIOD = 8
OFFSET = int(BLOCKS_PER_VOTING_PERIOD / 2)
POLLING_TIME = 5
NUM_NODES = 3
BAKER = "bootstrap1"
BAKE_ARGS = ['--minimal-timestamp']
ERROR_PATTERN = r"Uncaught|registered|error"

PROTO_A = protocol.PREV_HASH
PROTO_A_DAEMON = protocol.PREV_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"
PROTO_B = protocol.HASH
PROTO_B_DAEMON = protocol.DAEMON

def client_get_current_period_kind(client) -> dict:
    res = client.get_current_period()
    return res['voting_period']['kind']


def bake_n_blocks(client: Client, baker: str, n_blocks: int):
    for _ in range(n_blocks):
        client.bake(baker, BAKE_ARGS)


def bake_until_next_voting_period(client: Client, baker: str, offset: int = 0):
    period_info = client.get_current_period()
    remaining_blocks = period_info["remaining"]
    # if offset is the constant OFFSET, it will take us to
    # the middle of the next voting period
    bake_n_blocks(client, baker, 1 + remaining_blocks + offset)


@pytest.mark.timeout(10)
def wait_until_level(clients, level):
    for client in clients:
        while client.get_level() < level:
            time.sleep(1)


def assert_all_clients_in_period(clients, period):
    for client in clients:
        assert client_get_current_period_kind(client) == period


@pytest.mark.vote
@pytest.mark.slow
@pytest.mark.baker
@pytest.mark.incremental
class TestVotingFull:
    """This tests the migration from PROTO_A to PROTO_B using the voting
    procedure.  PROTO_A and PROTO_B are the previous and
    respectively the current protocol as given by the 'protocol'
    module.

    This test advances through all the periods of the voting procedure
    until the last one (adoption), by manually baking the right number
    of blocks. Once the adoption period is reached, a baker takes over
    to bake the remaining blocks of the period. From there the baker
    for the next protocol, which was started at the beginning of the
    test, takes over. (Bakers are used to make the test more
    realistic. However, to be sure that proposals and ballots are
    injected at the right moment, manual baking is used instead.)

    This test differs in the following aspects from test_voting.py:
    - it uses more nodes, not just one
    - it goes through all voting periods, not just the first two
    - it uses bakers
    - it uses already registered protocols, instead of injecting a
      new dummy protocol
    """

    def test_add_tmp_bootstrap_node(self, sandbox: Sandbox):
        """ launch tmp nodes just to bootstrap all other ones """
        sandbox.add_node(10, params=node_params(0))
        sandbox.add_node(11, params=node_params(0))

    def test_activate_proto_a(self, sandbox: Sandbox):
        sandbox.client(10).activate_protocol_json(PROTO_A, PARAMETERS)

    def test_add_tmp_bootstrap_baker(self, sandbox: Sandbox):
        """Launch a temporary baker so that 10 and 11 keep broadcasting
        heads to the future joining nodes and help them bootstrap"""
        # note we use 'bootstrap1' for all baking, this avoids the issue
        # of a delegate becoming inactive. For instance, if we want
        # to bake with 'bootstrap2' later in the test, it may have became
        # inactive
        sandbox.add_baker(10, 'bootstrap1', proto=PROTO_A_DAEMON)

    def test_add_initial_nodes(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)
        parameters = dict(protocol.PREV_PARAMETERS)
        parameters["blocks_per_voting_period"] = BLOCKS_PER_VOTING_PERIOD
        utils.activate_protocol(
            sandbox.client(0), PROTO_A, parameters, activate_in_the_past=True
        )

    def test_add_baker(self, sandbox: Sandbox):
        sandbox.add_baker(0, BAKER, proto=PROTO_B_DAEMON)

    def test_client_knows_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        protos = client.list_protocols()
        assert PROTO_B in protos

    def test_proposal_period(self, sandbox: Sandbox):
        assert_all_clients_in_period(sandbox.all_clients(), 'proposal')

    def test_submit_proto_b_proposal(self, sandbox, session):
        client = sandbox.client(0)
        cmd = ['submit', 'proposals', 'for', 'bootstrap1', PROTO_B]
        proposals = client_output.SubmitProposalsResult(client.run(cmd))
        session['prop_hash'] = proposals.operation_hash

    def test_submit_proto_b_proposal(self, sandbox):
        client = sandbox.client(0)
        client.wait_for_inclusion(session['prop_hash'])
        session['proposal_period'] = client.get_period_index()

    def test_check_proto_b_proposed(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        wait_until_level(clients, sandbox.client(0).get_level())
        for client in clients:
            proposals = client.get_proposals()
            assert PROTO_B in [proto for (proto, _) in proposals]

    @pytest.mark.timeout(60)
    def test_wait_for_voting_period(self, sandbox: Sandbox, session):
        client = sandbox.client(0)
        while client.get_period_index() < session['proposal_period'] + 1:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'testing_vote'

    def test_delegates_vote_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        # submit ballot for all bakers with listings
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    @pytest.mark.timeout(60)
    def test_wait_for_cooldown(self, sandbox: Sandbox, session):
        for client in sandbox.all_clients():
            while client.get_period_index() < session['proposal_period'] + 2:
                time.sleep(POLLING_TIME)
            assert client_get_current_period_kind(client) == 'testing'

    @pytest.mark.timeout(60)
    def test_wait_for_promotion_period(self, sandbox: Sandbox, session):
        client = sandbox.client(0)
        while client.get_period_index() < session['proposal_period'] + 3:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'promotion_vote'

    def test_vote_in_promotion_phase(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    @pytest.mark.timeout(90)
    def test_wait_for_adoption(self, sandbox: Sandbox, session):
        client = sandbox.client(1)
        while client.get_period_index() < session['proposal_period'] + 4:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'adoption'

    @pytest.mark.timeout(90)
    def test_wait_for_proposal(self, sandbox: Sandbox, session):
        client = sandbox.client(1)
        while client.get_period_index() < session['proposal_period'] + 5:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'proposal'

    @pytest.mark.timeout(60)
    def test_all_nodes_run_proto_b(self, sandbox: Sandbox):
        # we let a PROTO_A baker bake the last blocks of PROTO_A
        sandbox.add_baker(0, BAKER, proto=PROTO_A_DAEMON)
        clients = sandbox.all_clients()
        all_have_proto = False
        while not all_have_proto:
            all_have_proto = all(
                client.get_protocol() == PROTO_B for client in clients
            )
            time.sleep(POLLING_TIME)

    def test_new_chain_progress(self, sandbox: Sandbox):
        client = sandbox.client(0)
        level_before = client.get_level()
        assert utils.check_level_greater_than(client, level_before + 1)

    @pytest.mark.xfail
    def test_check_logs(self, sandbox: Sandbox):
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
