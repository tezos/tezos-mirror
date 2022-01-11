import copy
import shutil
import pytest
from client.client import Client
from tools import constants, paths, utils
from . import protocol

pytestmark = pytest.mark.skipif(
    utils.check_static_binary(constants.COMPILER),
    reason="cannot inject with statically compiled binaries",
)


@pytest.fixture(scope="class")
def client(sandbox):
    """One node, 4 blocks per voting period."""
    proto_params = dict(protocol.PARAMETERS)
    parameters = copy.deepcopy(proto_params)
    parameters["blocks_per_cycle"] = 4
    parameters["blocks_per_voting_period"] = 4
    parameters['consensus_threshold'] = 0
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    protocol.activate(sandbox.client(0), parameters, activate_in_the_past=True)
    yield sandbox.client(0)


@pytest.mark.vote
@pytest.mark.incremental
class TestManualBaking:
    """Test voting protocol with manual baking, 4 blocks per voting period."""

    def test_current_period(self, client: Client):
        period_info = client.get_current_period()
        level = client.get_current_level()
        assert level["level_position"] == 0
        assert period_info["voting_period"]["index"] == 0
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 0
        assert period_info["position"] == 0
        assert period_info["remaining"] == 3

    def test_succ_period(self, client: Client):
        period_info = client.get_succ_period()
        assert period_info["voting_period"]["index"] == 0
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 0
        assert period_info["position"] == 1
        assert period_info["remaining"] == 2

    def test_level_info_offset(self, client: Client):
        level = client.get_current_level(offset=1)
        assert level["level_position"] == 1
        level = client.get_current_level(offset=4)
        assert level["level_position"] == 4
        level = client.get_current_level(offset=10)
        assert level["level_position"] == 10

    def test_bake_two_blocks(self, client: Client):
        utils.bake(client)
        utils.bake(client)
        period_info = client.get_current_period()
        level = client.get_current_level()
        assert level["level_position"] == 2
        assert period_info["voting_period"]["index"] == 0
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 0
        assert period_info["position"] == 2
        assert period_info["remaining"] == 1

    def test_last_block_of_proposal_period(self, client: Client):
        # last block of voting period 0
        utils.bake(client)
        period_info = client.get_current_period()
        assert period_info["voting_period"]["index"] == 0
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 0
        assert period_info["position"] == 3
        assert period_info["remaining"] == 0

    def test_listing_is_not_empty(self, client: Client):
        assert client.get_listings() != []

    def test_inject_proto1(self, client: Client, tmpdir):
        proto_fp = (
            f'{paths.TEZOS_HOME}/src/bin_client/test/proto_test_injection'
        )

        for i in range(1, 4):
            proto = f'{tmpdir}/proto{i}'
            shutil.copytree(proto_fp, proto)
            main = f'{proto}/main.ml'
            print(main)
            with open(main, "a") as file:
                file.write(f'(* {i} *)')
            client.inject_protocol(proto)

    # this is maybe useless because the protocol already knows more than 4
    # protocol
    def test_known_protocol(self, client: Client, session: dict):
        protos = client.list_protocols()
        assert len(protos) >= 4
        session['protos'] = protos[:4]

    def test_proposals_is_empty(self, client: Client):
        assert client.get_proposals() == []

    def test_show_voting_period2(self, client: Client):
        client.show_voting_period()

    def test_bake_first_block_of_proposal_period(self, client: Client):
        # using the client it's not possible to add voting operation on the
        # first block of a voting period. This is to be fixed in a future
        # protocol
        utils.bake(client)
        period_info = client.get_current_period()
        assert period_info["voting_period"]["index"] == 1
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 4
        assert period_info["position"] == 0
        assert period_info["remaining"] == 3

    def test_submit_proposals(self, client: Client, session: dict):
        protos = session['protos']
        client.submit_proposals('bootstrap1', [protos[0]])
        client.submit_proposals('bootstrap2', [protos[0], protos[1]])
        client.submit_proposals('bootstrap3', [protos[1]])
        client.submit_proposals('bootstrap4', [protos[2]])

    def test_bake_one_block(self, client: Client):
        utils.bake(client)
        period_info = client.get_current_period()
        assert period_info["voting_period"]["index"] == 1
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 4
        assert period_info["position"] == 1
        assert period_info["remaining"] == 2

    def test_proposals_is_not_empty(self, client: Client):
        assert client.get_proposals() != []

    def test_bake_until_prev_last_block_of_voting_period(self, client: Client):
        utils.bake(client)
        period_info = client.get_current_period()
        assert period_info["position"] == 2
        assert period_info["remaining"] == 1

    def test_break_proposal_tie(self, client: Client, session: dict):
        protos = session['protos']
        client.submit_proposals('bootstrap4', [protos[1]])

    def test_bake_last_block_of_proposal_period(self, client: Client):
        utils.bake(client)
        period_info = client.get_current_period()
        metadata = client.get_metadata()
        level = client.get_current_level()
        level_info = metadata["level_info"]
        meta_period_info = metadata["voting_period_info"]
        expected_commitment = level["expected_commitment"]
        assert level["level"] == level_info["level"]
        assert level["level_position"] == level_info["level_position"]
        assert level["cycle"] == level_info["cycle"]
        assert level["cycle_position"] == level_info["cycle_position"]
        assert expected_commitment == level_info["expected_commitment"]
        assert level["level_position"] == 7
        assert period_info["voting_period"]["index"] == 1
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 4
        assert period_info["position"] == 3
        assert period_info["remaining"] == 0
        assert meta_period_info == period_info

    def test_listing_is_not_empty2(self, client: Client):
        assert client.get_listings() != []

    def test_current_proposal(self, client: Client, session: dict):
        expected = session['protos'][1]
        assert expected == client.get_current_proposal()

    def test_bake_first_block_of_cooldown_vote_period(self, client: Client):
        # using the client it's not possible to add voting operation on the
        # first block of a voting period. This is to be fixed in a future
        # protocol
        utils.bake(client)
        period_info = client.get_current_period()
        assert period_info["voting_period"]["index"] == 2
        assert period_info["voting_period"]["kind"] == "exploration"
        assert period_info["voting_period"]["start_position"] == 8
        assert period_info["position"] == 0
        assert period_info["remaining"] == 3

    def test_submit_ballot(self, client: Client, session: dict):
        # next block is going to be of 'exploration' kind
        proto = session['protos'][1]
        for i in range(1, 4):
            client.submit_ballot(f'bootstrap{i}', proto, 'yay')

    def test_bake_until_prev_last_block_of_voting_period2(self, client: Client):
        utils.bake(client)
        utils.bake(client)
        period_info = client.get_current_period()
        level = client.get_current_level()
        assert level["level_position"] == 10
        assert period_info["voting_period"]["index"] == 2
        assert period_info["voting_period"]["kind"] == "exploration"
        assert period_info["voting_period"]["start_position"] == 8
        assert period_info["position"] == 2
        assert period_info["remaining"] == 1

    def test_submit_failing_ballot(self, client: Client, session: dict):
        proto = session['protos'][1]
        client.submit_ballot(f'bootstrap{4}', proto, 'nay')

    def test_level_info_offset2(self, client: Client):
        level = client.get_current_level(block='head~1')
        assert level["level_position"] == 9
        level = client.get_current_level(block='head~4')
        assert level["level_position"] == 6
        level = client.get_current_level(block='head~10')
        assert level["level_position"] == 0

    def test_bake_first_block_of_new_proposal_period(self, client: Client):
        utils.bake(client)
        # Because of the current hack in proposal here we make sure we get the
        # correct value
        level = client.get_current_level()
        period_info = client.get_current_period()
        metadata = client.get_metadata()
        level_info = metadata["level_info"]
        meta_period_info = metadata["voting_period_info"]
        expected_commitment = level["expected_commitment"]
        assert level["level"] == level_info["level"]
        assert level["level_position"] == level_info["level_position"]
        assert level["cycle"] == level_info["cycle"]
        assert level["cycle_position"] == level_info["cycle_position"]
        assert expected_commitment == level_info["expected_commitment"]
        assert level["level_position"] == 11
        assert period_info["voting_period"]["index"] == 2
        assert period_info["voting_period"]["kind"] == "exploration"
        assert period_info["voting_period"]["start_position"] == 8
        assert period_info["position"] == 3
        assert period_info["remaining"] == 0
        assert meta_period_info == period_info
        utils.bake(client)
        period_info = client.get_current_period()
        level = client.get_current_level()
        assert level["level_position"] == 12
        assert period_info["voting_period"]["index"] == 3
        assert period_info["voting_period"]["kind"] == "proposal"
        assert period_info["voting_period"]["start_position"] == 12
        assert period_info["position"] == 0
        assert period_info["remaining"] == 3
        assert client.get_listings() != '[]'
        # strange behavior here, RPC returns 'null' on stderr
        assert client.get_current_proposal() is None
        assert client.get_ballot_list() == []
