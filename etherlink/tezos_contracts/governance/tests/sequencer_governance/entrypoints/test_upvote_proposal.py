from tests.base import BaseTestCase
from tests.helpers.utility import pkh
from tests.helpers.contracts.governance_base import PROMOTION_PERIOD, PROPOSAL_PERIOD
from tests.helpers.errors import (
    NO_VOTING_POWER, NOT_PROPOSAL_PERIOD, PROPOSAL_ALREADY_UPVOTED, UPVOTING_LIMIT_EXCEEDED, 
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER, pack_sequencer_payload

class CommitteeGovernanceUpvoteProposalTestCase(BaseTestCase):
    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_sequencer_governance()
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).with_amount(1).send()

    def test_should_fail_if_sender_has_no_voting_power(self) -> None:
        no_baker = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'delegation_contract': delegation.address
        })

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()

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
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 2
        governance.using(baker).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        self.bake_block()
        # Period index: 1. Block: 1 of 2
        state = governance.get_voting_state()
        assert state['period_index'] == 1
        assert state['period_type'] == PROMOTION_PERIOD

        # Period index: 1. Block: 2 of 2
        with self.raisesMichelsonError(NOT_PROPOSAL_PERIOD):
            governance.using(baker).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()

    def test_should_fail_if_upvoting_limit_is_exceeded(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()

        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 7
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 7,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })
        
        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 7
        governance.using(baker1).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 3 of 7
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }
        governance.using(baker1).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 4 of 7
        payload3 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417188'
        }
        governance.using(baker2).new_proposal(payload3['sequencer_pk'], payload3['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 5 of 7
        governance.using(baker3).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 6 of 7
        governance.using(baker3).upvote_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(baker3).upvote_proposal(payload3['sequencer_pk'], payload3['pool_address']).send()


    def test_should_fail_if_proposal_already_upvoted_by_proposer(self) -> None:
        baker = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        delegation = self.deploy_delegated_governance()
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

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(baker).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()

    def test_should_fail_if_proposal_already_upvoted_by_another_baker(self) -> None:
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
        
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 1 of 5
        governance.using(baker1).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        # Period index: 0. Block: 2 of 5
        governance.using(baker2).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(baker2).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()

    def test_should_upvote_proposal_with_correct_parameters(self) -> None:
        # Reset sandbox because the voting power of each bakers changes during previous tests
        self.setUpClass()

        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()

        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 6
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 6,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 6,
            'finished_voting': None
        }
        
        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        # Period index: 0. Block: 2 of 6
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
            'remaining_blocks': 5,
            'finished_voting': None
        }

        # Period index: 0. Block: 3 of 6
        governance.using(baker2).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload1)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
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
        # Period index: 0. Block: 4 of 6
        governance.using(baker1).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload1)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 3,
            'finished_voting': None
        }

        # Period index: 0. Block: 5 of 6
        governance.using(baker2).upvote_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        # Period index: 0. Block: 6 of 6
        governance.using(baker3).upvote_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload2)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 3
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 1,
            'finished_voting': None
        }

    def test_should_upvote_proposal_as_delegate(self) -> None:
        proposer = self.bootstrap_baker()
        delegator = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        # Period index: 0. Block: 2 of 5
        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator), governance.address)

        # Period index: 0. Block: 3 of 5
        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(proposer).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        # Period index: 0. Block: 4 of 5
        governance.using(delegate).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 2,
            'finished_voting': None
        }
    
    def test_should_upvote_proposal_as_delegate_for_multiple_bakers(self) -> None:
        # Reset sandbox because the voting power of each bakers changes during previous tests
        self.setUpClass()
        
        proposer = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 3,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator1), governance.address)

        delegation.using(delegator2).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator2), governance.address)

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(proposer).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        governance.using(delegate).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 3
        assert governance.get_voting_state()['remaining_blocks'] == 1
    
    def test_should_fail_to_upvote_as_delegate_if_baker_already_upvoted(self) -> None:
        proposer = self.bootstrap_baker()
        delegator = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator), governance.address)

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(proposer).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        governance.using(delegator).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(delegate).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert governance.get_voting_state()['remaining_blocks'] == 2

    
    def test_delegate_upvote_should_fail_with_limit_exceeded_if_already_upvoted_and_limit_hit(self) -> None:
        proposer = self.bootstrap_baker()
        delegator = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }

        governance.using(proposer).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()
        governance.using(proposer).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        governance.using(delegator).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()
        governance.using(delegator).upvote_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()

        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(delegate).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()


    def test_delegate_upvote_should_fail_with_already_upvoted(self) -> None:
        proposer = self.bootstrap_baker()
        proposer2 = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'period_length': 13,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        payload1 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        payload2 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417186'
        }
        payload3 = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417188'
        }

        governance.using(proposer).new_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        governance.using(proposer).new_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        governance.using(proposer2).new_proposal(payload3['sequencer_pk'], payload3['pool_address']).send()
        self.bake_block()

        governance.using(delegator1).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()
        self.bake_block()

        governance.using(delegator2).upvote_proposal(payload2['sequencer_pk'], payload2['pool_address']).send()
        self.bake_block()
        governance.using(delegator2).upvote_proposal(payload3['sequencer_pk'], payload3['pool_address']).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(delegate).upvote_proposal(payload1['sequencer_pk'], payload1['pool_address']).send()

    def test_delegate_upvote_should_only_apply_for_non_upvoted_baker(self) -> None:
        proposer = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_sequencer_governance(custom_config={
            'period_length': 6,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        payload = {
            'sequencer_pk': 'edpkurcgafZ2URyB6zsm5d1YqmLt9r1Lk89J81N6KpyMaUzXWEsv1X',
            'pool_address': 'B7A97043983f24991398E5a82f63F4C58a417185'
        }
        governance.using(proposer).new_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        governance.using(delegator1).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        governance.using(delegate).upvote_proposal(payload['sequencer_pk'], payload['pool_address']).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == pack_sequencer_payload(payload)
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 3