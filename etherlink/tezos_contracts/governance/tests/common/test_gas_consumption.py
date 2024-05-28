import secrets
from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import YEA_VOTE
from tests.helpers.operation_result_recorder import OperationResultRecorder
from tests.helpers.utility import find_op_by_hash, get_tests_dir, pkh
from pytezos.operation.result import OperationResult
from os.path import join

class KernelGovernanceGasConsumptionTestCase(BaseTestCase):
    recorder = OperationResultRecorder()

    @classmethod
    def tearDownClass(self) -> None:
        self.recorder.write_to_file(join(get_tests_dir(), 'gas_consumption.json'))
        super().tearDownClass()

    def test_new_proposal_same_baker(self) -> None:
        baker = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 500,
            'upvoting_limit': 500,
        })

        for i in range(4):
            random_bytes = secrets.token_bytes(33)
            opg = governance.using(baker).new_proposal(random_bytes).send()
            self.bake_block()
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'new_proposal_same_baker_nth_{i + 1}', op)

    def test_new_proposal_different_baker(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        baker4 = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 500,
            'upvoting_limit': 500,
        })

        for i, baker in enumerate([baker1, baker2, baker3, baker4]):
            random_bytes = secrets.token_bytes(33)
            opg = governance.using(baker).new_proposal(random_bytes).send()
            self.bake_block()
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'new_proposal_different_baker_nth_{i + 1}', op)

    def test_new_proposal_with_event(self) -> None:
        baker1 = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20, # 1 baker out of 5 will vote,
            'promotion_quorum': 20, # 1 bakers out of 5 will vote (20%)  
            'promotion_supermajority': 50, # 1 baker will vote yea
        })

        random_bytes = secrets.token_bytes(33)
        governance.using(baker1).new_proposal(random_bytes).send()
        self.bake_blocks(2)
        
        governance.using(baker1).vote(YEA_VOTE).send()
        self.bake_blocks(2)
        
        opg = governance.using(baker1).new_proposal(secrets.token_bytes(33)).send()
        self.bake_block()
        op = find_op_by_hash(self.manager, opg)
        self.recorder.add_element(f'test_new_proposal_with_event', op)
        
    def test_upvote_proposal(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        baker4 = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 500,
        })

        random_bytes = secrets.token_bytes(33)
        governance.using(baker1).new_proposal(random_bytes).send()
        self.bake_block()
        
        for i, baker in enumerate([baker2, baker3, baker4]):
            opg = governance.using(baker).upvote_proposal(random_bytes).send()
            self.bake_block()
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'upvote_nth_{i + 1}', op)

    def test_vote_proposal(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        baker4 = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 10,
            'proposal_quorum': 20 # 1 baker out of 5 will vote
        })

        random_bytes = secrets.token_bytes(33)
        governance.using(baker1).new_proposal(random_bytes).send()
        self.bake_blocks(11)
        
        for i, baker in enumerate([baker1, baker2, baker3, baker4]):
            opg = governance.using(baker).vote(YEA_VOTE).send()
            self.bake_block()
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'vote_nth_{i + 1}', op)

    def test_new_proposal_with_a_lot_of_proposals_on_previous_voting_period(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        def run_test(prev_voting_proposal_count): 
            governance_started_at_level = self.get_current_level() + 1
            governance = self.deploy_kernel_governance(custom_config={
                'started_at_level': governance_started_at_level,
                'period_length': prev_voting_proposal_count + 2,
                'upvoting_limit': 500,
                'proposal_quorum': 20, # 1 baker out of 5 will vote,
                'promotion_quorum': 20, # 1 bakers out of 5 will vote (20%)  
                'promotion_supermajority': 50, # 1 baker will vote yea
            })

            random_bytes = secrets.token_bytes(33)
            governance.using(baker1).new_proposal(random_bytes).send()
            self.bake_block()
            governance.using(baker2).upvote_proposal(random_bytes).send()
            self.bake_block()
            for i in range(prev_voting_proposal_count):
                governance.using(baker1).new_proposal(secrets.token_bytes(33)).send()
                self.bake_block()
            
            governance.using(baker1).vote(YEA_VOTE).send()
            self.bake_blocks(prev_voting_proposal_count + 2)
            
            opg = governance.using(baker1).new_proposal(secrets.token_bytes(33)).send()
            self.bake_block()
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'new_proposal_with_{prev_voting_proposal_count}_proposals_on_previous_voting_period', op)

        for i, prev_voting_proposal_count in enumerate([10, 50, 100]):
            run_test(prev_voting_proposal_count)

    def test_trigger_upgrade(self) -> None:
        baker = self.bootstrap_baker()
        rollup_mock1 = self.deploy_rollup_mock()
        rollup_mock2 = self.deploy_rollup_mock()
        rollup_mock3 = self.deploy_rollup_mock()
        rollup_mock4 = self.deploy_rollup_mock()
        rollup_mock5 = self.deploy_rollup_mock()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 6,
            'proposal_quorum': 20,
            'promotion_quorum': 20,
            'promotion_supermajority': 20,
        })

        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_blocks(7)
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(7)
        
        for i, mock in enumerate([rollup_mock1, rollup_mock2, rollup_mock3, rollup_mock4, rollup_mock5]):
            opg = governance.using(baker).trigger_kernel_upgrade(mock.contract.address).send()
            self.bake_blocks(10)
            op = find_op_by_hash(self.manager, opg)
            self.recorder.add_element(f'trigger_upgrade_nth_{i + 1}', op)
        
    # def test_new_proposal_stress(self) -> None:
    #     baker = self.bootstrap_baker()
    #     governance_started_at_level = self.get_current_level() + 1
    #     governance = self.deploy_kernel_governance(custom_config={
    #         'started_at_level': governance_started_at_level,
    #         'period_length': 5000,
    #         'upvoting_limit': 5000,
    #     })

    #     for i in range(5000):
    #         random_bytes = secrets.token_bytes(33)
    #         opg = governance.using(baker).new_proposal(random_bytes).send()
    #         self.bake_block()
    #         op = find_op_by_hash(self.manager, opg)
    #         consumed_gas = OperationResult.consumed_gas(op)
    #         print('new proposal: ', i, 'consumed_gas', consumed_gas)