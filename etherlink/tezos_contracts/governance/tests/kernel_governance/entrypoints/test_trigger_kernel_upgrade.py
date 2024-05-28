from pytezos.client import PyTezosClient
from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import YEA_VOTE
from tests.helpers.contracts.kernel_governance import KernelGovernance
from tests.helpers.errors import (
    LAST_WINNER_NOT_FOUND,
    UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED,
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_ADDRESS
import re

class KernelGovernanceTriggerKernelUpgradeTestCase(BaseTestCase):
    def prepare_last_winner(self, payload):
        baker = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_kernel_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20,
            'promotion_quorum': 20,
            'promotion_supermajority': 20,
        })

        # Period index: 0. Block: 2 of 2
        governance.using(baker).new_proposal(payload).send()
        self.bake_blocks(2)

        # Period index: 1. Block: 2 of 2
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(2)

        return {
            'governance': governance,
            'baker': baker,
        }

    def test_should_fail_if_tez_in_transaction(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_kernel_governance()

        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).trigger_kernel_upgrade(DEFAULT_ADDRESS).with_amount(1).send()

    def test_should_fail_if_there_is_no_last_winner_payload(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_kernel_governance()

        with self.raisesMichelsonError(LAST_WINNER_NOT_FOUND):
            governance.using(baker).trigger_kernel_upgrade(DEFAULT_ADDRESS).send()

    def test_should_allow_no_baker_to_trigger_upgrade(self) -> None:
        no_baker = self.bootstrap_no_baker()
        rollup_mock = self.deploy_rollup_mock()
        governance = self.deploy_kernel_governance()

        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        test = self.prepare_last_winner(kernel_root_hash)
        governance : KernelGovernance = test['governance']

        governance.using(no_baker).trigger_kernel_upgrade(rollup_mock.contract.address).send()
        self.bake_block()

    def test_should_fail_to_send_last_winner_second_time_to_the_same_address_but_reset_for_new_winner(self) -> None:
        rollup_mock1 = self.deploy_rollup_mock()
        rollup_mock2 = self.deploy_rollup_mock()
        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        test = self.prepare_last_winner(kernel_root_hash)
        governance : KernelGovernance = test['governance']
        baker : PyTezosClient = test['baker']

        governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        self.bake_blocks(10)

        governance.using(baker).trigger_kernel_upgrade(rollup_mock2.contract.address).send()
        self.bake_blocks(10)

        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock2.contract.address).send()

        governance.using(baker).new_proposal(kernel_root_hash).send()
        self.bake_blocks(2)
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(2)

        governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        self.bake_blocks(10)
        governance.using(baker).trigger_kernel_upgrade(rollup_mock2.contract.address).send()
        self.bake_blocks(10)

        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock2.contract.address).send()

        # check that trigger history preserves
        self.bake_blocks(21)
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_kernel_upgrade(rollup_mock2.contract.address).send()


    def test_should_send_last_winner_payload_to_rollup(self) -> None:
        rollup_mock = self.deploy_rollup_mock()
        kernel_root_hash = bytes.fromhex('020202020202020202020202020202020202020202020202020202020202020202')
        test = self.prepare_last_winner(kernel_root_hash)
        governance : KernelGovernance = test['governance']
        baker : PyTezosClient = test['baker']

        payload_pattern = rf'^EBA1{kernel_root_hash.hex()}88[\da-f]{{16}}$'
        assert not re.match(payload_pattern, rollup_mock.contract.storage().hex(), re.IGNORECASE)

        governance.using(baker).trigger_kernel_upgrade(rollup_mock.contract.address).send()
        self.bake_block()
        assert re.match(payload_pattern, rollup_mock.contract.storage().hex(), re.IGNORECASE)