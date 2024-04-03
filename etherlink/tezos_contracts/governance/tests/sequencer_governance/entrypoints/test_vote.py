from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import NAY_VOTE, PASS_VOTE, PROMOTION_PERIOD, PROPOSAL_PERIOD, YEA_VOTE
from tests.helpers.errors import (
    INCORRECT_VOTE_VALUE, NO_VOTING_POWER, NOT_PROMOTION_PERIOD, PROMOTION_ALREADY_VOTED, 
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER, pack_sequencer_payload

class CommitteeGovernanceNewProposalTestCase(BaseTestCase):
    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_sequencer_governance()

        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).vote(YEA_VOTE).with_amount(1).send()

    def test_should_fail_if_sender_has_no_voting_power(self) -> None:
        no_baker = self.bootstrap_no_baker()
        governance = self.deploy_sequencer_governance()

        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).vote(YEA_VOTE).send()

    def test_should_fail_if_current_period_is_not_promotion(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_sequencer_governance()

        with self.raisesMichelsonError(NOT_PROMOTION_PERIOD):
            governance.using(baker).vote(YEA_VOTE).send()

    def test_should_fail_if_vote_parameter_is_incorrect(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 3
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 3,
            'proposal_quorum': 20 # 1 baker out of 5 will vote
        })
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }# Period index: 0. Block: 2 of 3
        governance.using(baker1).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()
        
        # Period index: 0. Block: 3 of 3
        self.bake_block()
        # Period index: 1. Block: 1 of 3
        self.bake_block()

        # Period index: 1. Block: 2 of 3
        with self.raisesMichelsonError(INCORRECT_VOTE_VALUE):
            governance.using(baker1).vote("yep").send()
        with self.raisesMichelsonError(INCORRECT_VOTE_VALUE):
            governance.using(baker1).vote("no").send()
        with self.raisesMichelsonError(INCORRECT_VOTE_VALUE):
            governance.using(baker1).vote("pas").send()
        with self.raisesMichelsonError(INCORRECT_VOTE_VALUE):
            governance.using(baker1).vote("NAY").send()

        governance.using(baker1).vote("yea").send()
        governance.using(baker2).vote("nay").send()
        governance.using(baker3).vote("pass").send()
        self.bake_block()

    def test_should_fail_if_proposal_already_voted(self) -> None:
        baker = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 3
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 3,
            'proposal_quorum': 20 # 1 baker out of 5 will vote
        })
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 3
        governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()
        
        # Period index: 0. Block: 3 of 3
        self.bake_block()
        # Period index: 1. Block: 1 of 3
        self.bake_block()

        # Period index: 1. Block: 2 of 3
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_block()

        with self.raisesMichelsonError(PROMOTION_ALREADY_VOTED):
            governance.using(baker).vote(YEA_VOTE).send()

    def test_should_vote_on_proposal_with_correct_parameters(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        baker4 = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'proposal_quorum': 20 # 1 baker out of 5 will vote
        })

        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 5,
            'finished_voting': None
        }
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 5
        governance.using(baker1).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()
        # Period index: 1. Block: 1 of 5
        self.bake_blocks(4)

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 5,
            'finished_voting': None
        }

        # Period index: 1. Block: 2 of 5
        governance.using(baker1).vote(YEA_VOTE).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == 0
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 4,
            'finished_voting': None
        }


        # Period index: 1. Block: 3 of 5
        governance.using(baker2).vote(NAY_VOTE).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 3,
            'finished_voting': None
        }

        # Period index: 1. Block: 4 of 5
        governance.using(baker3).vote(PASS_VOTE).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 2,
            'finished_voting': None
        }

        # Period index: 1. Block: 5 of 5
        governance.using(baker4).vote(YEA_VOTE).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER  * 2
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 1,
            'finished_voting': None
        }