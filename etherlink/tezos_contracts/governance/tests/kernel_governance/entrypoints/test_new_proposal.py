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
        governance = self.deploy_kernel_governance()

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
        governance = self.deploy_kernel_governance()

        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).new_proposal(kernel_root_hash).send()

    def test_should_not_fail_if_payload_same_as_last_winner(self) -> None:
        baker = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 2
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 bakers out of 5 voted
            'promotion_quorum': 20, # 1 bakers out of 5 voted
            'promotion_supermajority': 50, # 1 bakers out of 5 voted
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
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 2
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20 # 1 bakers out of 5 voted
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
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2
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
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2
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
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        # Period index: 0. Block: 1 of 5
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 5,
            'upvoting_limit': 2
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