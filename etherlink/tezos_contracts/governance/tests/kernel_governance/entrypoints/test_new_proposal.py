from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import PROMOTION_PERIOD, PROPOSAL_PERIOD, YEA_VOTE
from tests.helpers.errors import (
    INCORRECT_KERNEL_ROOT_HASH_LENGTH, NO_VOTING_POWER, NOT_PROPOSAL_PERIOD, 
    PROPOSAL_ALREADY_CREATED, PROPOSER_NOT_IN_COMMITTEE, UPVOTING_LIMIT_EXCEEDED, 
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER, pkh

class KernelGovernanceNewProposalTestCase(BaseTestCase):
    def test_should_fail_if_kernel_root_hash_has_incorrect_size(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={'delegation_contract': delegation.address})

        with self.raisesMichelsonError(INCORRECT_KERNEL_ROOT_HASH_LENGTH):
            governance.using(baker).new_proposal(bytes.fromhex('009279df4982e47cf101e2525b605fa06cd3ccc0f67d1c792a6a3ea56af9606a')).send()
        with self.raisesMichelsonError(INCORRECT_KERNEL_ROOT_HASH_LENGTH):
            governance.using(baker).new_proposal(bytes.fromhex('009279df4982e47cf101e2525b605fa06cd3ccc0f67d1c792a6a3ea56af9606abcde')).send()
        governance.using(baker).new_proposal(bytes.fromhex('009279df4982e47cf101e2525b605fa06cd3ccc0f67d1c792a6a3ea56af9606abc')).send()
        self.bake_block()

    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_kernel_governance()

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).new_proposal(kernel_root_hash).with_amount(1).send()

    def test_should_fail_if_sender_has_no_voting_power(self) -> None:
        no_baker = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={'delegation_contract': delegation.address})

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).new_proposal(kernel_root_hash).send()

    def test_should_not_fail_if_payload_same_as_last_winner(self) -> None:
        baker = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()

        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        
        # Period index: 0. Block: 1 of 2
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 bakers out of 5 voted
            'promotion_quorum': 20, # 1 bakers out of 5 voted
            'promotion_supermajority': 50, # 1 bakers out of 5 voted
            'delegation_contract': delegation.address
        })

        # Period index: 0. Block: 2 of 2
        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_blocks(2)

        # Period index: 1. Block: 1 of 2
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(2)

        # Period index: 3. Block: 1 of 2
        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_block()

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
        
        # Period index: 0. Block: 2 of 2
        governance.using(baker).new_proposal('010101010101010101010101010101010101010101010101010101010101010101').send()
        self.bake_block()

        self.bake_block()
        # Period index: 1. Block: 1 of 2
        state = governance.get_voting_state()
        assert state['period_index'] == 1
        assert state['period_type'] == PROMOTION_PERIOD

        # Period index: 1. Block: 2 of 2
        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        with self.raisesMichelsonError(NOT_PROPOSAL_PERIOD):
            governance.using(baker).new_proposal(kernel_root_hash).send()

    def test_should_fail_if_new_proposal_limit_is_exceeded(self) -> None:
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
        
        # Period index: 0. Block: 2 of 5
        governance.using(baker).new_proposal('010101010101010101010101010101010101010101010101010101010101010101').send()
        self.bake_block()
        # Period index: 0. Block: 3 of 5
        governance.using(baker).new_proposal('020202020202020202020202020202020202020202020202020202020202020202').send()
        self.bake_block()

        with self.raisesMichelsonError(UPVOTING_LIMIT_EXCEEDED):
            governance.using(baker).new_proposal('030303030303030303030303030303030303030303030303030303030303030303').send()

    def test_should_fail_if_new_proposal_already_created(self) -> None:
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

        with self.raisesMichelsonError(PROPOSAL_ALREADY_CREATED):
            governance.using(baker).new_proposal(kernel_root_hash).send()

    def test_should_create_new_proposal_with_correct_parameters(self) -> None:
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

        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 5,
            'finished_voting': None
        }
        
        kernel_root_hash1 = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        # Period index: 0. Block: 2 of 5
        governance.using(baker1).new_proposal(kernel_root_hash1).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash1
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 0,
            'remaining_blocks': 4,
            'finished_voting': None
        }

        kernel_root_hash2 = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        # Period index: 0. Block: 3 of 5
        governance.using(baker2).new_proposal(kernel_root_hash2).send()
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
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()

        assert delegation.is_delegate(pkh(proposer_delegate), pkh(delegator), governance.address)

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')

        governance.using(proposer_delegate).new_proposal(kernel_root_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert governance.get_voting_state()['remaining_blocks'] == 3
    
    def test_delegate_should_propose_only_for_non_proposing_bakers(self) -> None:
        proposer_delegate = self.bootstrap_no_baker()
        delegator1 = self.bootstrap_baker()
        delegator2 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'period_length': 5,
            'upvoting_limit': 3,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(delegator1).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(delegator2).set_delegate(pkh(proposer_delegate), True, whitelist).send()
        self.bake_block()

        kernel_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(delegator1).new_proposal(kernel_hash).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_hash

        # Delegate should still be able to propose on behalf of delegator2
        kernel_hash2 = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        governance.using(proposer_delegate).new_proposal(kernel_hash2).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_hash2
    
    def test_delegate_can_propose_multiple_payloads_for_different_bakers(self) -> None:
        delegate = self.bootstrap_no_baker()
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()

        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={
            'upvoting_limit': 2,
            'delegation_contract': delegation.address
        })

        whitelist = {governance.address}
        delegation.using(baker1).set_delegate(pkh(delegate), True, whitelist).send()
        self.bake_block()
        delegation.using(baker2).set_delegate(pkh(delegate), True, whitelist).send()
        self.bake_block()

        kernel1 = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        kernel2 = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')

        governance.using(delegate).new_proposal(kernel1).send()
        self.bake_block()

        storage = governance.contract.storage()
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel1

        governance.using(delegate).new_proposal(kernel2).send()
        self.bake_block()


