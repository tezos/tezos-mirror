from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import PROMOTION_PERIOD, PROPOSAL_PERIOD, YEA_VOTE
from tests.helpers.errors import (
    INCORRECT_POOL_ADDRESS_LENGTH, INCORRECT_SEQUENCER_PK_LENGTH, NO_VOTING_POWER, NOT_PROPOSAL_PERIOD, PROPOSAL_ALREADY_CREATED, PROPOSER_NOT_IN_COMMITTEE, 
    UPVOTING_LIMIT_EXCEEDED, TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER, pack_sequencer_payload, pkh

class CommitteeGovernanceNewProposalTestCase(BaseTestCase):
    def test_should_fail_if_proposal_payload_has_incorrect_size(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={'delegation_contract': delegation.address})

        with self.raisesMichelsonError(INCORRECT_SEQUENCER_PK_LENGTH):
            governance.using(baker).new_proposal('edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1XFF', 'B7A97043983f24991398E5a82f63F4C58a417185').send()
        with self.raisesMichelsonError(INCORRECT_POOL_ADDRESS_LENGTH):
            governance.using(baker).new_proposal('edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X', 'B7A97043983f24991398E5a82f63F4C58a41718543').send()
        governance.using(baker).new_proposal('edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X', 'B7A97043983f24991398E5a82f63F4C58a417185').send()
        self.bake_block()

    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_sequencer_governance()

        sequencer_pk = 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X'
        pool_address = 'B7A97043983f24991398E5a82f63F4C58a417185'
        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).new_proposal(sequencer_pk, pool_address).with_amount(1).send()

    def test_should_fail_if_sender_has_no_voting_power(self) -> None:
        no_baker = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={'delegation_contract': delegation.address})

        sequencer_pk = 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X'
        pool_address = 'B7A97043983f24991398E5a82f63F4C58a417185'
        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).new_proposal(sequencer_pk, pool_address).send()

    def test_should_not_fail_if_payload_same_as_last_winner(self) -> None:
        baker = self.bootstrap_baker()
        
        delegation = self.deploy_delegated_governance()

        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1

        # Period index: 0. Block: 1 of 2
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 bakers out of 5 voted
            'promotion_quorum': 20, # 1 bakers out of 5 voted
            'promotion_supermajority': 50, # 1 bakers out of 5 voted
            'delegation_contract': delegation.address
        })

        # Period index: 0. Block: 2 of 2
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_blocks(2)

        # Period index: 1. Block: 1 of 2
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(2)

        # Period index: 3. Block: 1 of 2
        governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

    def test_should_fail_if_current_period_is_not_proposal(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 2
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 bakers out of 5 voted
            'delegation_contract': delegation.address
        })

        # Period index: 0. Block: 2 of 2
        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }

        governance.using(baker).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        self.bake_block()
        # Period index: 1. Block: 1 of 2
        state = governance.get_voting_state()
        assert state['period_index'] == 1
        assert state['period_type'] == PROMOTION_PERIOD

        # Period index: 1. Block: 2 of 2
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a4171ff'
        }
        with self.raisesMichelsonError(NOT_PROPOSAL_PERIOD):
            governance.using(baker).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()

    def test_should_fail_if_new_proposal_limit_is_exceeded(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })
        
        # Period index: 0. Block: 2 of 5
        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(baker).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 3 of 5
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }
        governance.using(baker).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        payload3 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417187'
        }
        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(baker).new_proposal(payload3['sequencer_pk'], payload3['pool_address']).send()

    def test_should_fail_if_new_proposal_already_created(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 5,
            'delegation_contract': delegation.address
        })
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 1 of 5
        governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()
        self.bake_block()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_CREATED):
            governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()

    def test_should_create_new_proposal_with_correct_parameters(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 5,
            'finished_voting': None
        }
        
        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 5
        governance.using(baker1).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload1)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 4,
            'finished_voting': None
        }

        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }
        # Period index: 0. Block: 3 of 5
        governance.using(baker2).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == None
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 3,
            'finished_voting': None
        }

    def test_should_should_be_able_to_propose_as_delegate(self) -> None:
        delegator = self.bootstrap_baker()
        proposer_delegate= self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_delegate(pkh(proposer_delegate), pkh(delegator), governance.address)

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }

        governance.using(proposer_delegate).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert governance.get_voting_state()['remaining_blocks'] == 3
    
    def test_delegate_should_propose_only_for_non_proposing_bakers(self) -> None:
        proposer_delegate = self.bootstrap_no_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'period_length': 5,
            'upvoting_limit': 3,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(delegator1).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)

        # Delegate should still be able to propose on behalf of delegator2
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }
        governance.using(proposer_delegate).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload2)
    
    def test_delegate_can_propose_multiple_payloads_for_different_bakers(self) -> None:
        delegate = self.bootstrap_no_baker()
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(baker1).set_delegate(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(baker2).set_delegate(pkh(delegate), True, whitelist).send()
        self.bake_block()

        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }

        governance.using(delegate).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload1)

        governance.using(delegate).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()