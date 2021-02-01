"""
This tests the migration from PROTO_A to PROTO_B.
"""

import datetime
import json
import os
import time

import pytest

from launchers.sandbox import Sandbox
from tools import paths, utils
from . import protocol

ERROR_PATTERN = r"Uncaught|registered|error"
BLOCKS_PER_VOTING_PERIOD = 24
POLLING_TIME = 5

PROTO_A = protocol.PREV_HASH
PROTO_A_DAEMON = protocol.PREV_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"
PROTO_B = protocol.HASH
PROTO_B_DAEMON = protocol.DAEMON

PARAMETERS_FILE = (
    f'{paths.TEZOS_HOME}src/{PROTO_A_PATH}/parameters/' 'test-parameters.json'
)
assert os.path.isfile(PARAMETERS_FILE), (
    f'{PARAMETERS_FILE}'
    ' cannot be found; please first run'
    ' `make` in TEZOS_HOME.'
)
with open(PARAMETERS_FILE) as f:
    PARAMETERS = dict(json.load(f))
    PARAMETERS["blocks_per_voting_period"] = BLOCKS_PER_VOTING_PERIOD


def node_params(threshold=0):
    return [
        '--sync-latency',
        '2',
        '--synchronisation-threshold',
        str(threshold),
        '--connections',
        '100',
        '--enable-testchain',
    ]


def client_get_current_period_kind(client) -> dict:
    res = client.get_current_period()
    return res['voting_period']['kind']


@pytest.mark.vote
@pytest.mark.slow
@pytest.mark.baker
@pytest.mark.testchain
@pytest.mark.incremental
class TestVotingFull:
    def test_add_tmp_bootstrap_node(self, sandbox: Sandbox):
        """ launch tmp nodes just to bootstrap all other ones """
        sandbox.add_node(10, params=node_params(0))
        sandbox.add_node(11, params=node_params(0))

    def test_activate_proto_a(self, sandbox: Sandbox):
        delay = datetime.timedelta(seconds=0)
        sandbox.client(10).activate_protocol_json(
            PROTO_A, PARAMETERS, delay=delay
        )

    def test_add_tmp_bootstrap_baker(self, sandbox: Sandbox):
        """Launch a temporary baker so that 10 and 11 keep broadcasting
        heads to the future joining nodes and help them bootstrap"""
        # note we use 'bootstrap1' for all baking, this avoids the issue
        # of a delegate becoming inactive. For instance, if we want
        # to bake with 'bootstrap2' later in the test, it may have became
        # inactive
        sandbox.add_baker(10, 'bootstrap1', proto=PROTO_A_DAEMON)

    def test_add_initial_nodes(self, sandbox: Sandbox):
        """We launch nodes with non-null synchronisation-threshold.
        This is to test the bootstrap heuristics with the testchain."""
        sandbox.add_node(0, params=node_params(2))
        sandbox.add_node(1, params=node_params(2))
        sandbox.add_node(2, params=node_params(2))
        sandbox.add_node(3, params=node_params(2))

    @pytest.mark.timeout(20)
    def test_bootstrap(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        for client in clients:
            print(client.sync_state())
            client.bootstrapped()

    def test_remove_tmp_bootstrap_nodes(self, sandbox: Sandbox):
        """These temp noddes are no longer needed"""
        sandbox.rm_baker(10, proto=PROTO_A_DAEMON)
        sandbox.rm_node(10)
        sandbox.rm_node(11)

    def test_add_bakers(self, sandbox: Sandbox):
        sandbox.add_baker(0, 'bootstrap1', proto=PROTO_A_DAEMON)
        sandbox.add_baker(0, 'bootstrap1', proto=PROTO_B_DAEMON)

    def test_client_knows_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        protos = client.list_protocols()
        assert PROTO_B in protos

    def test_proposal_period(self, sandbox: Sandbox):
        client = sandbox.client(0)
        assert client_get_current_period_kind(client) == 'proposal'

    def test_submit_proto_b_proposal(self, sandbox, session):
        client = sandbox.client(0)
        proposals = client.submit_proposals('bootstrap1', [PROTO_B])
        session['prop_hash'] = proposals.operation_hash

    def test_wait_for_operation_inclusion(self, sandbox, session):
        client = sandbox.client(0)
        time.sleep(3 * POLLING_TIME)
        client.wait_for_inclusion(session['prop_hash'])

    @pytest.mark.timeout(60)
    def test_check_proto_b_proposed(self, sandbox: Sandbox):
        client = sandbox.client(0)
        proposals = client.get_proposals()
        while proposals == []:
            time.sleep(POLLING_TIME)
        assert PROTO_B in [proto for (proto, _) in proposals]

    @pytest.mark.timeout(60)
    def test_wait_for_voting_period(self, sandbox: Sandbox):
        client = sandbox.client(0)
        while client.get_level() <= BLOCKS_PER_VOTING_PERIOD + 1:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'testing_vote'

    def test_delegates_vote_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        # submit ballot for all bakers with listings
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    @pytest.mark.timeout(60)
    def test_wait_for_cooldown(self, sandbox: Sandbox):
        for client in sandbox.all_clients():
            while client.get_level() <= 2 * BLOCKS_PER_VOTING_PERIOD + 1:
                time.sleep(POLLING_TIME)
            assert client_get_current_period_kind(client) == 'testing'

    @pytest.mark.timeout(60)
    def test_wait_for_promotion_period(self, sandbox: Sandbox):
        client = sandbox.client(0)
        while client.get_level() <= 3 * BLOCKS_PER_VOTING_PERIOD + 1:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'promotion_vote'

    def test_vote_in_promotion_phase(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    @pytest.mark.timeout(90)
    def test_wait_for_adoption(self, sandbox: Sandbox):
        client = sandbox.client(1)
        while client.get_level() <= 4 * BLOCKS_PER_VOTING_PERIOD + 1:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'adoption'

    @pytest.mark.timeout(90)
    def test_wait_for_proposal(self, sandbox: Sandbox):
        client = sandbox.client(1)
        while client.get_level() <= 5 * BLOCKS_PER_VOTING_PERIOD + 1:
            time.sleep(POLLING_TIME)
        assert client_get_current_period_kind(client) == 'proposal'

    @pytest.mark.timeout(60)
    def test_all_nodes_run_proto_b(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        all_have_proto = False
        while not all_have_proto:
            all_have_proto = all(
                client.get_protocol() == PROTO_B for client in clients
            )
            (time).sleep(POLLING_TIME)

    def test_new_chain_progress(self, sandbox: Sandbox):
        client = sandbox.client(0)
        level_before = client.get_level()
        assert utils.check_level_greater_than(client, level_before + 1)

    @pytest.mark.xfail
    def test_check_logs(self, sandbox: Sandbox):
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
