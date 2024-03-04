from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import NAY_VOTE, PASS_VOTE, PROMOTION_PERIOD, PROPOSAL_PERIOD, YEA_VOTE
from tests.helpers.contracts.kernel_governance import KernelGovernance
from tests.helpers.utility import DEFAULT_TOTAL_VOTING_POWER, DEFAULT_VOTING_POWER
from pytezos.client import PyTezosClient

class KernelGovernancePromotionPeriodTestCase(BaseTestCase):
    def prepare_promotion_period(self, custom_config=None):
        proposer = self.bootstrap_baker()
        # deploying will take 1 block
        governance_started_at_level = self.get_current_level() + 1
        config = {
            'started_at_level': governance_started_at_level,
            'period_length': 3,
            'proposal_quorum': 10 # 1 bakers out of 5 voted
        }
        if custom_config is not None:
            config.update(custom_config)

        # Period index: 0. Block: 1 of 3
        governance = self.deploy_kernel_governance(custom_config=config)
        assert self.get_current_level() == governance_started_at_level

        # Period index: 0. Block: 2 of 3
        kernel_root_hash = bytes.fromhex('010101010101010101010101010101010101010101010101010101010101010101')
        governance.using(proposer).new_proposal(kernel_root_hash).send()
        self.bake_block()

        self.bake_blocks(2)

        # Period index: 1. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 3,
            'finished_voting': None
        }

        return {
            'governance': governance,
            'proposer': proposer,
            'kernel_root_hash': kernel_root_hash
        }

    def test_should_reset_to_proposal_period_if_promotion_period_is_skipped(self) -> None:
        test = self.prepare_promotion_period()
        governance: KernelGovernance = test['governance']
        kernel_root_hash = test['kernel_root_hash']

        # Wait to skip the whole promotion period
        self.bake_blocks(3)
        # Period index: 2. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }

        # Wait to skip one more period
        self.bake_blocks(3)
        # Period index: 3. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 0
        assert storage['voting_context']['period']['proposal']['winner_candidate'] == kernel_root_hash
        assert storage['voting_context']['period']['proposal']['max_upvotes_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['proposal']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 3,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }

    def test_should_reset_to_proposal_period_if_promotion_quorum_is_not_reached(self) -> None:
        test = self.prepare_promotion_period({
            'promotion_quorum': 50, # 2 bakers out of 5 will vote (40%)
            'promotion_supermajority': 10, # 1 baker will vote yea, 1 baker will vote nay (50%)
        })
        governance: KernelGovernance = test['governance']
        proposer: PyTezosClient = test['proposer']
        kernel_root_hash = test['kernel_root_hash']
        baker1 = self.bootstrap_baker()

        # Period index: 1. Block: 2 of 3
        governance.using(proposer).vote(YEA_VOTE).send()
        governance.using(baker1).vote(NAY_VOTE).send()
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
            'remaining_blocks': 2,
            'finished_voting': None
        }

        # Wait to skip one more period
        self.bake_blocks(2)
        # Period index: 2. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }

    def test_should_reset_to_proposal_period_if_promotion_supermajority_is_not_reached(self) -> None:
        test = self.prepare_promotion_period({
            'promotion_quorum': 50, # 3 bakers out of 5 will vote (60%)
            'promotion_supermajority': 51, # 1 baker will vote yea, 1 baker will vote nay (50%)
        })
        governance: KernelGovernance = test['governance']
        proposer: PyTezosClient = test['proposer']
        kernel_root_hash = test['kernel_root_hash']
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()

        # Period index: 1. Block: 2 of 3
        governance.using(proposer).vote(YEA_VOTE).send()
        governance.using(baker1).vote(NAY_VOTE).send()
        governance.using(baker2).vote(PASS_VOTE).send()
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

        # Wait to skip one more period
        self.bake_blocks(2)
        # Period index: 2. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }

    def test_should_reset_to_proposal_period_if_promotion_phase_has_only_pass_votes(self) -> None:
        test = self.prepare_promotion_period({
            'promotion_quorum': 50, # 1 bakers out of 5 will vote (20%)
            'promotion_supermajority': 51, # 1 baker will vote pass)
        })
        governance: KernelGovernance = test['governance']
        kernel_root_hash = test['kernel_root_hash']
        baker2 = self.bootstrap_baker()

        # Period index: 1. Block: 2 of 3
        governance.using(baker2).vote(PASS_VOTE).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 2,
            'finished_voting': None
        }

        # Wait to skip one more period
        self.bake_blocks(2)
        # Period index: 2. Block: 1 of 3

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }

    def test_should_reset_to_proposal_period_if_promotion_phase_has_only_pass_votes_which_passes_only_promotion_quorum(self) -> None:
        test = self.prepare_promotion_period({
            'promotion_quorum': 20, # 1 bakers out of 5 will vote (20%)
            'promotion_supermajority': 51, # 1 baker will vote pass
        })
        governance: KernelGovernance = test['governance']
        kernel_root_hash = test['kernel_root_hash']
        baker2 = self.bootstrap_baker()

        # Period index: 1. Block: 2 of 3
        governance.using(baker2).vote(PASS_VOTE).send()
        self.bake_block()
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROMOTION_PERIOD,
            'period_index': 1,
            'remaining_blocks': 2,
            'finished_voting': None
        }

        # Wait to skip one more period
        self.bake_blocks(2)
        # Period index: 2. Block: 1 of 3

        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == 0 
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': None
            }
        }


    def test_should_reset_to_proposal_period_with_a_new_winner_and_event_if_promotion_period_passed_successfully(self) -> None:
        test = self.prepare_promotion_period({
            'promotion_quorum': 50, # 3 bakers out of 5 will vote (60%)  
            'promotion_supermajority': 40, # 1 baker will vote yea, 1 baker will vote nay (50%)
        })
        governance: KernelGovernance = test['governance']
        proposer: PyTezosClient = test['proposer']
        kernel_root_hash = test['kernel_root_hash']
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()

        # Period index: 1. Block: 2 of 3
        governance.using(proposer).vote(YEA_VOTE).send()
        governance.using(baker1).vote(NAY_VOTE).send()
        governance.using(baker2).vote(PASS_VOTE).send()
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

        # Wait to skip one more period
        self.bake_blocks(2)
        # Period index: 2. Block: 1 of 3
        storage = governance.contract.storage()
        assert storage['voting_context']['period_index'] == 1
        assert storage['voting_context']['period']['promotion']['yea_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['nay_voting_power'] == DEFAULT_VOTING_POWER
        assert storage['voting_context']['period']['promotion']['pass_voting_power'] == DEFAULT_VOTING_POWER 
        assert storage['voting_context']['period']['promotion']['total_voting_power'] == DEFAULT_TOTAL_VOTING_POWER 
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 2,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': kernel_root_hash
            }
        }

        # Check that event preserves in future periods
        self.bake_blocks(6)
        assert governance.get_voting_state() == {
            'period_type': PROPOSAL_PERIOD,
            'period_index': 4,
            'remaining_blocks': 3,
            'finished_voting': {
                'finished_at_period_index': 2, 
                'finished_at_period_type': PROMOTION_PERIOD,
                'winner_proposal_payload': kernel_root_hash
            }
        }
