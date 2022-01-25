import time

import subprocess
import pytest

from launchers.sandbox import Sandbox
from client.client import Client
from tools import utils, constants
from . import protocol

# TODO: <https://gitlab.com/tezos/tezos/-/issues/2519>
# When PROTO_A >= J (proto 013) then replace this constant by
# CYCLES_PER_VOTING_PERIOD = 2
BLOCKS_PER_VOTING_PERIOD = 8
OFFSET = int(BLOCKS_PER_VOTING_PERIOD / 2)
POLLING_TIME = 5
BAKING_RATE = 1
NUM_NODES = 5
BAKER = "bootstrap1"
ERROR_PATTERN = r"Uncaught|registered|error"

PROTO_A = protocol.PREV_HASH
PROTO_A_DAEMON = protocol.PREV_DAEMON
PROTO_A_PATH = f"proto_{PROTO_A_DAEMON.replace('-','_')}"
PROTO_B = protocol.HASH
PROTO_B_DAEMON = protocol.DAEMON


def client_get_current_period_kind(client) -> dict:
    res = client.get_current_period()
    return res['voting_period']['kind']


def tenderbake(client: Client):
    """Call to 'bake for' that uses the multi-account command for Tenderbake.

    In particular, this allows to never get a 'Delegates do not have enough
    voting power' error in sandboxed mode since we bake for all known accounts
    (aka all bootstrap accounts) by default in method multibake.

    """
    client.multibake(args=['--minimal-timestamp'])


def bake_n_blocks(client: Client, baker: str, n_blocks: int):
    for _ in range(n_blocks):
        utils.bake(client, bake_for=baker)


def bake_until_next_voting_period(client: Client, baker: str, offset: int = 0):
    period_info = client.get_current_period()
    remaining_blocks = period_info["remaining"]
    # if offset is the constant OFFSET, it will take us to
    # the middle of the next voting period
    bake_n_blocks(client, baker, 1 + remaining_blocks + offset)


@pytest.mark.timeout(60)
def wait_until_level(clients, level):
    print(f"Waiting until {level}")
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

    def test_add_initial_nodes(self, sandbox: Sandbox):
        for i in range(NUM_NODES):
            sandbox.add_node(i, params=constants.NODE_PARAMS)

    def test_activate_proto_a(self, sandbox: Sandbox):
        parameters = protocol.get_parameters(protocol.Protocol.PREV)
        # TODO: <https://gitlab.com/tezos/tezos/-/issues/2519>
        # When PROTO_A >= J (proto 013) then replace this line with
        # parameters["cycles_per_voting_period"] = CYCLES_PER_VOTING_PERIOD
        parameters["blocks_per_voting_period"] = BLOCKS_PER_VOTING_PERIOD
        utils.activate_protocol(
            sandbox.client(0),
            PROTO_A,
            parameters=parameters,
            activate_in_the_past=True,
        )

    # def test_add_bakers(self, sandbox: Sandbox):
    #     """Add a baker per node"""
    #     sandbox.add_baker(
    #         1, [f"bootstrap{i}" for i in range(1, 6)], proto=PROTO_B_DAEMON,
    #         run_params=['--liquidity-baking-escape-vote', 'pass'],
    #     )

    def test_client_knows_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        protos = client.list_protocols()
        assert PROTO_B in protos

    def test_proposal_period(self, sandbox: Sandbox):
        assert_all_clients_in_period(sandbox.all_clients(), 'proposal')

    def test_submit_proto_b_proposal(self, sandbox):
        client = sandbox.client(0)
        proposals = client.submit_proposals(BAKER, [PROTO_B])
        # bake a block for the submit proposal to be included
        bake_n_blocks(client, BAKER, 1)
        client.wait_for_inclusion(proposals.operation_hash, check_previous=1)

    def test_check_proto_b_proposed(self, sandbox: Sandbox):
        clients = sandbox.all_clients()
        wait_until_level(clients, sandbox.client(0).get_level())
        for client in clients:
            proposals = client.get_proposals()
            assert PROTO_B in [proto for (proto, _) in proposals]

    def test_wait_for_exploration_period(self, sandbox: Sandbox):
        client = sandbox.client(0)
        bake_until_next_voting_period(client, BAKER, OFFSET)
        clients = sandbox.all_clients()
        wait_until_level(clients, client.get_level())
        assert_all_clients_in_period(clients, 'exploration')

    def test_delegates_vote_proto_b(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        # submit ballot for all bakers with listings
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    def test_wait_for_cooldown(self, sandbox: Sandbox):
        client = sandbox.client(0)
        bake_until_next_voting_period(client, BAKER, OFFSET)
        clients = sandbox.all_clients()
        wait_until_level(clients, client.get_level())
        assert_all_clients_in_period(clients, 'cooldown')

    def test_wait_for_promotion_period(self, sandbox: Sandbox):
        client = sandbox.client(0)
        bake_until_next_voting_period(client, BAKER, OFFSET)
        clients = sandbox.all_clients()
        wait_until_level(clients, client.get_level())
        assert_all_clients_in_period(clients, 'promotion')

    def test_vote_in_promotion_phase(self, sandbox: Sandbox):
        client = sandbox.client(0)
        listings = client.get_listings()
        for listing in listings:
            client.submit_ballot(listing["pkh"], PROTO_B, 'yay')

    def test_wait_for_adoption(self, sandbox: Sandbox):
        client = sandbox.client(0)
        bake_until_next_voting_period(client, BAKER)
        clients = sandbox.all_clients()
        wait_until_level(clients, client.get_level())
        assert_all_clients_in_period(clients, 'adoption')

    @pytest.mark.timeout(600)
    def test_all_nodes_run_proto_b(self, sandbox: Sandbox):
        # we let a PROTO_A baker bake the last blocks of PROTO_A
        # sandbox.add_baker(
        #     0, [f"bootstrap{i}" for i in range(1, 6)], proto=PROTO_A_DAEMON,
        #     run_params=['--liquidity-baking-escape-vote', 'pass'],
        # )
        # for i in range(1,NUM_NODES):
        #     sandbox.add_baker(
        #         i, [f"bootstrap{i}"], proto=PROTO_B_DAEMON,
        #         run_params=['--liquidity-baking-escape-vote', 'pass'],
        #     )
        clients = sandbox.all_clients()
        client = clients[0]
        utils.bake(client, bake_for="bootstrap2")
        all_have_proto_b = False
        while not all_have_proto_b:
            try:
                utils.bake(client, bake_for="bootstrap2")
            except subprocess.CalledProcessError:
                # A fatal error is raised when we do not have enough endorsing
                # power.
                # This is typical of a simple bake for call in Tenderbake
                # Therefore this means we actually have migrated to Tenderbake
                # Let's use a baking call that is sure to pass
                tenderbake(client)
            # either succeeds out of the loop or fails due to the timeout header
            client_protocols = [client.get_protocol() for c in clients]
            all_have_proto_b = all(p == PROTO_B for p in client_protocols)
            time.sleep(POLLING_TIME)

    def test_new_chain_progress(self, sandbox: Sandbox):
        # sandbox.rm_baker(0, proto=PROTO_A_DAEMON)
        client = sandbox.client(0)
        level_before = client.get_level(chain='main')
        tenderbake(client)
        print(f"level before {level_before}")
        assert utils.check_level_greater_than(client, level_before + 1)

    def test_submit_again(self, sandbox: Sandbox):
        # check that the voting procedure can still be initiated
        # after the migration
        client = sandbox.client(0)
        proposals = client.submit_proposals(BAKER, [PROTO_B])
        tenderbake(client)
        client.wait_for_inclusion(proposals.operation_hash, check_previous=1)

    @pytest.mark.xfail
    def test_check_logs(self, sandbox: Sandbox):
        assert utils.check_logs(sandbox.logs, ERROR_PATTERN)
