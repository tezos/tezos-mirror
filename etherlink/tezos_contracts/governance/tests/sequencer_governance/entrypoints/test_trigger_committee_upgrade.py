from pytezos.client import PyTezosClient
from tests.base import BaseTestCase
from tests.helpers.contracts.governance_base import YEA_VOTE
from tests.helpers.contracts.sequencer_governance import SequencerGovernance
from tests.helpers.errors import (
    LAST_WINNER_NOT_FOUND,
    UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED,
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import DEFAULT_ADDRESS
import re

class CommitteeGovernanceTriggerCommitteeUpgradeTestCase(BaseTestCase):
    def prepare_last_winner(self, sequencer_pk, pool_address):
        baker = self.bootstrap_baker()
        governance_started_at_level = self.get_current_level() + 1
        governance = self.deploy_sequencer_governance(custom_config={
            'started_at_level': governance_started_at_level,
            'period_length': 2,
            'proposal_quorum': 20,
            'promotion_quorum': 20,
            'promotion_supermajority': 20,
        })

        # Period index: 0. Block: 2 of 2
        governance.using(baker).new_proposal(sequencer_pk, pool_address).send()
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
        governance = self.deploy_sequencer_governance()

        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            governance.using(baker).trigger_committee_upgrade(DEFAULT_ADDRESS).with_amount(1).send()

    def test_should_fail_if_there_is_no_last_winner_payload(self) -> None:
        baker = self.bootstrap_baker()
        governance = self.deploy_sequencer_governance()

        with self.raisesMichelsonError(LAST_WINNER_NOT_FOUND):
            governance.using(baker).trigger_committee_upgrade(DEFAULT_ADDRESS).send()

    def test_should_allow_no_baker_to_trigger_upgrade(self) -> None:
        no_baker = self.bootstrap_no_baker()
        rollup_mock = self.deploy_rollup_mock()
        sequencer_pk = 'edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav'
        pool_address = '71c7656ec7ab88b098defb751b7401b5f6d8976f'
        test = self.prepare_last_winner(sequencer_pk, pool_address)
        governance : SequencerGovernance = test['governance']

        governance.using(no_baker).trigger_committee_upgrade(rollup_mock.contract.address).send()
        self.bake_block()

    def test_should_fail_to_send_last_winner_second_time_to_the_same_address_but_reset_for_new_winner(self) -> None:
        rollup_mock1 = self.deploy_rollup_mock()
        rollup_mock2 = self.deploy_rollup_mock()
        sequencer_pk = 'edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav'
        pool_address = '71c7656ec7ab88b098defb751b7401b5f6d8976f'
        test = self.prepare_last_winner(sequencer_pk, pool_address)
        governance : SequencerGovernance = test['governance']
        baker : PyTezosClient = test['baker']

        governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        self.bake_blocks(10)

        governance.using(baker).trigger_committee_upgrade(rollup_mock2.contract.address).send()
        self.bake_blocks(10)

        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock2.contract.address).send()

        governance.using(baker).new_proposal(sequencer_pk, pool_address).send()
        self.bake_blocks(2)
        governance.using(baker).vote(YEA_VOTE).send()
        self.bake_blocks(2)

        governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        self.bake_blocks(10)
        governance.using(baker).trigger_committee_upgrade(rollup_mock2.contract.address).send()
        self.bake_blocks(10)

        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock2.contract.address).send()

        # check that trigger history preserves
        self.bake_blocks(21)
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock1.contract.address).send()
        with self.raisesMichelsonError(UPGRADE_FOR_ADDRESS_ALREADY_TRIGGERED):
            governance.using(baker).trigger_committee_upgrade(rollup_mock2.contract.address).send()


    def test_should_send_last_winner_payload_to_rollup(self) -> None:
        rollup_mock = self.deploy_rollup_mock()
        sequencer_pk = 'edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav'
        pool_address = '71c7656ec7ab88b098defb751b7401b5f6d8976f'
        test = self.prepare_last_winner(sequencer_pk, pool_address)
        governance : SequencerGovernance = test['governance']
        baker : PyTezosClient = test['baker']

        payload_pattern = rf'^F855B6{sequencer_pk.encode().hex()}94{pool_address}88[\da-f]{{16}}$'
        assert not re.match(payload_pattern, rollup_mock.contract.storage().hex(), re.IGNORECASE)

        governance.using(baker).trigger_committee_upgrade(rollup_mock.contract.address).send()
        self.bake_block()
        assert re.match(payload_pattern, rollup_mock.contract.storage().hex(), re.IGNORECASE)