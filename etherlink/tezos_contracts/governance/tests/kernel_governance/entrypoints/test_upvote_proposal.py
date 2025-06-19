from tests.base import BaseTestCase
from tests.helpers.utility import pkh
from tests.helpers.contracts.governance_base import PROMOTION_PERIOD, PROPOSAL_PERIOD
from tests.helpers.errors import (
    NO_VOTING_POWER, NOT_PROPOSAL_PERIOD, PROPOSAL_ALREADY_UPVOTED, 
    UPVOTING_LIMIT_EXCEEDED, TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER

class KernelGovernanceUpvoteProposalTestCase(BaseTestCase):
    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_kernel_governance()

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).upvote_proposal(kernel_root_hash).with_amount(1).send()

    def test_should_fail_if_sender_has_no_voting_power(self) -> None:
        no_baker = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'delegation_contract': delegation.address
        })

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).upvote_proposal(kernel_root_hash).send()

    def test_should_fail_if_current_period_is_not_proposal(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 2
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 bakers out of 5 voted
            'delegation_contract': delegation.address
        })
        
        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        # Period index: 0. Block: 2 of 2
        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_block()

        self.bake_block()
        # Period index: 1. Block: 1 of 2
        state = governance.get_voting_state()
        assert state['period_index'] == 1
        assert state['period_type'] == PROMOTION_PERIOD

        # Period index: 1. Block: 2 of 2
        with self.raisesMichelsonError(NOT_PROPOSAL_PERIOD):
            governance.using(baker).upvote_proposal(kernel_root_hash).send()


    def test_should_fail_if_upvoting_limit_is_exceeded(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 7
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 7,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })
        
        kernel_root_hash1 = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        # Period index: 0. Block: 2 of 7
        governance.using(baker1).new_proposal(kernel_root_hash1).send()
        self.bake_block()
        # Period index: 0. Block: 3 of 7
        kernel_root_hash2 = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        governance.using(baker1).new_proposal(kernel_root_hash2).send()
        self.bake_block()
        # Period index: 0. Block: 4 of 7
        kernel_root_hash3 = bytes.fromhex('030303030303030303030303030303030303030303030303030303030303030303')
        governance.using(baker2).new_proposal(kernel_root_hash3).send()
        self.bake_block()
        # Period index: 0. Block: 5 of 7
        governance.using(baker3).upvote_proposal(kernel_root_hash1).send()
        self.bake_block()
        # Period index: 0. Block: 6 of 7
        governance.using(baker3).upvote_proposal(kernel_root_hash2).send()
        self.bake_block()

        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(baker3).upvote_proposal(kernel_root_hash3).send()


    def test_should_fail_if_proposal_already_upvoted_by_proposer(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })
        
        kernel_root_hash = '010101010101010101010101010101010101010101010101010101010101010101'
        # Period index: 0. Block: 1 of 5
        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(baker).upvote_proposal(kernel_root_hash).send()

    def test_should_fail_if_proposal_already_upvoted_by_another_baker(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })
        
        kernel_root_hash = '010101010101010101010101010101010101010101010101010101010101010101'
        # Period index: 0. Block: 1 of 5
        governance.using(baker1).new_proposal(kernel_root_hash).send()
        self.bake_block()

        # Period index: 0. Block: 2 of 5
        governance.using(baker2).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(baker2).upvote_proposal(kernel_root_hash).send()

    def test_should_upvote_proposal_with_correct_parameters(self) -> None:
        # Resetting sandbox
        self.setUpClass()
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
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
        
        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        # Period index: 0. Block: 2 of 5
        governance.using(baker1).new_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 4,
            'finished_voting': None
        }

        # Period index: 0. Block: 3 of 5
        governance.using(baker2).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 3,
            'finished_voting': None
        }

        kernel_root_hash2 = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        # Period index: 0. Block: 4 of 5
        governance.using(baker1).new_proposal(kernel_root_hash2).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 2,
            'finished_voting': None
        }

        # Period index: 0. Block: 5 of 5
        governance.using(baker2).upvote_proposal(kernel_root_hash2).send()
        governance.using(baker3).upvote_proposal(kernel_root_hash2).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash2
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
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator), governance.address)

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(proposer).new_proposal(kernel_root_hash).send()
        self.bake_block()

        governance.using(delegate).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert governance.get_voting_state()['remaining_blocks'] == 2
    
    def test_should_upvote_proposal_as_delegate_for_multiple_bakers(self) -> None:
        proposer = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
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

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(proposer).new_proposal(kernel_root_hash).send()
        self.bake_block()

        governance.using(delegate).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 3
        assert governance.get_voting_state()['remaining_blocks'] == 1
    
    def test_should_fail_to_upvote_as_delegate_if_baker_already_upvoted(self) -> None:
        proposer = self.bootstrap_baker()
        delegator = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(pkh(delegate), pkh(delegator), governance.address)

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(proposer).new_proposal(kernel_root_hash).send()
        self.bake_block()

        governance.using(delegator).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(delegate).upvote_proposal(kernel_root_hash).send()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 2
        assert governance.get_voting_state()['remaining_blocks'] == 2

    
    def test_delegate_upvote_should_fail_with_limit_exceeded_if_already_upvoted_and_limit_hit(self) -> None:
        proposer = self.bootstrap_baker()
        delegator = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        kernel_a = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        kernel_b = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')

        governance.using(proposer).new_proposal(kernel_a).send()
        self.bake_block()
        governance.using(proposer).new_proposal(kernel_b).send()
        self.bake_block()

        governance.using(delegator).upvote_proposal(kernel_a).send()
        self.bake_block()
        governance.using(delegator).upvote_proposal(kernel_b).send()
        self.bake_block()

        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(delegate).upvote_proposal(kernel_a).send()


    def test_delegate_upvote_should_fail_with_already_upvoted(self) -> None:
        proposer = self.bootstrap_baker()
        proposer2 = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'period_length': 13,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        kernel_a = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        kernel_b = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        kernel_c = bytes.fromhex('030303030303030303030303030303030303030303030303030303030303030303')

        governance.using(proposer).new_proposal(kernel_a).send()
        self.bake_block()

        governance.using(proposer).new_proposal(kernel_b).send()
        self.bake_block()
        governance.using(proposer2).new_proposal(kernel_c).send()
        self.bake_block()

        governance.using(delegator1).upvote_proposal(kernel_a).send()
        self.bake_block()

        governance.using(delegator2).upvote_proposal(kernel_b).send()
        self.bake_block()
        governance.using(delegator2).upvote_proposal(kernel_c).send()
        self.bake_block()

        with self.raisesMichelsonError(PROPOSAL_ALREADY_UPVOTED):
            governance.using(delegate).upvote_proposal(kernel_a).send()

    def test_delegate_upvote_should_only_apply_for_non_upvoted_baker(self) -> None:
        proposer = self.bootstrap_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'period_length': 6,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address,
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_voting_key(pkh(delegate), True, whitelist).send()
        self.bake_block()

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(proposer).new_proposal(kernel_root_hash).send()
        self.bake_block()

        governance.using(delegator1).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        governance.using(delegate).upvote_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER * 3


