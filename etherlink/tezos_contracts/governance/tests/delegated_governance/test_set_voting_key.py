from tests.base import BaseTestCase
from tests.helpers.errors import (
    TEZ_IN_TRANSACTION_DISALLOWED
)
from tests.helpers.utility import pkh

class DelegatedGovernanceSetDelegateTestCase(BaseTestCase):
    def test_should_propose_voting_key(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
        delegates = delegation.list_voters(delegate_pkh, None)
        assert not (baker_pkh in delegates)

        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
        delegates = delegation.list_voters(delegate_pkh, None)
        assert baker_pkh in delegates

    def test_should_propose_voting_key_with_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        whitelist = {governance.address}

        delegation.using(baker).propose_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)
        delegates = delegation.list_voters(delegate_pkh, governance.address)
        assert not (baker_pkh in delegates)

        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)
        
        delegates = delegation.list_voters(delegate_pkh, governance.address)
        assert baker_pkh in delegates
        # The two assertions below should this should fail because a whitelist is set
        delegates = delegation.list_voters(delegate_pkh, delegation.address)
        assert not (baker_pkh in delegates)
        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)
    
    def test_should_delegate_to_same_address_for_two_bakers_with_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        whitelist = {governance.address}

        delegation.using(baker).propose_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        delegation.using(baker2).propose_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker2_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)
        assert delegation.is_voting_key_of(delegate_pkh, baker2_pkh, governance.address)
        delegates = delegation.list_voters(delegate_pkh, governance.address)
        assert baker_pkh, baker2_pkh in delegates

    def test_should_delegate_with_blacklist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        blacklist = {governance.address}

        delegation.using(baker).propose_voting_key(delegate_pkh, False, blacklist).send()
        self.bake_block()

        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)
        delegates = delegation.list_voters(delegate_pkh, governance.address)
        assert baker_pkh not in delegates

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
        delegates = delegation.list_voters(delegate_pkh, None)
        assert baker_pkh in delegates

    def test_should_remove_delegate(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)

        delegation.using(baker).propose_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)

    def test_should_update_delegate_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        kernel = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})
        sequencer = self.deploy_sequencer_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        whitelist1 = {kernel.address}
        whitelist2 = {sequencer.address}

        delegation.using(baker).propose_voting_key(delegate_pkh, True, whitelist1).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, kernel.address)
        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, whitelist2).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, kernel.address)
        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

    def test_delegate_for_all_contracts(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(pkh(delegate), True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
    
    def test_should_cleanup_delegate_map_when_empty(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)

        delegation.using(baker).propose_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
        assert delegation.list_voters(delegate_pkh, None) == []

    def test_multiple_delegations(self) -> None:
        baker = self.bootstrap_baker()
        delegate1 = self.bootstrap_no_baker()
        delegate2 = self.bootstrap_no_baker()
        delegate3 = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate1_pkh = pkh(delegate1)
        delegate2_pkh = pkh(delegate2)
        delegate3_pkh = pkh(delegate3)

        delegation.using(baker).propose_voting_key(delegate1_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate1).claim_voting_rights(baker_pkh).send()
        self.bake_block()
        delegation.using(baker).propose_voting_key(delegate2_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate2).claim_voting_rights(baker_pkh).send()
        self.bake_block()
        delegation.using(baker).propose_voting_key(delegate3_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate3).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate1_pkh, baker_pkh, None)
        assert delegation.is_voting_key_of(delegate2_pkh, baker_pkh, None)
        assert delegation.is_voting_key_of(delegate3_pkh, baker_pkh, None)

        delegates = delegation.list_voters(delegate1_pkh, None)
        assert baker_pkh in delegates
        delegates = delegation.list_voters(delegate2_pkh, None)
        assert baker_pkh in delegates
        delegates = delegation.list_voters(delegate3_pkh, None)
        assert baker_pkh in delegates

    def test_update_blacklist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        kernel = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})
        sequencer = self.deploy_sequencer_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        blacklist1 = {kernel.address}
        blacklist2 = {sequencer.address}

        delegation.using(baker).propose_voting_key(delegate_pkh, False, blacklist1).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, kernel.address)
        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

        delegation.using(baker).propose_voting_key(delegate_pkh, False, blacklist2).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, kernel.address)
        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

    def test_mixed_whitelist_blacklist(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        whitelist = {governance.address}
        blacklist = {governance.address}

        delegation.using(baker1).propose_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker1_pkh).send()
        self.bake_block()

        delegation.using(baker2).propose_voting_key(delegate_pkh, False, blacklist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker1_pkh, governance.address)
        assert not delegation.is_voting_key_of(delegate_pkh, baker2_pkh, governance.address)

    def test_empty_lists(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()
        governance = self.deploy_kernel_governance(custom_config={"delegation_contract": delegation.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        empty_whitelist = set()
        delegation.using(baker).propose_voting_key(delegate_pkh, True, empty_whitelist).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)

        empty_blacklist = set()
        delegation.using(baker).propose_voting_key(delegate_pkh, False, empty_blacklist).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, governance.address)

    def test_cross_delegation(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        baker3_pkh = pkh(baker3)

        delegation.using(baker1).propose_voting_key(baker2_pkh, True, None).send()
        self.bake_block()
        delegation.using(baker2).claim_voting_rights(baker1_pkh).send()
        self.bake_block()

        delegation.using(baker2).propose_voting_key(baker3_pkh, True, None).send()
        self.bake_block()
        delegation.using(baker3).claim_voting_rights(baker2_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(baker2_pkh, baker1_pkh, None)
        assert delegation.is_voting_key_of(baker3_pkh, baker2_pkh, None)
        assert not delegation.is_voting_key_of(baker3_pkh, baker1_pkh, None)

    def test_storage_cleanup_comprehensive(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        delegation.using(baker1).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker1_pkh).send()
        self.bake_block()
        delegation.using(baker2).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker2_pkh).send()
        self.bake_block()

        assert delegation.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert delegation.is_voting_key_of(delegate_pkh, baker2_pkh, None)

        delegation.using(baker1).propose_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert delegation.is_voting_key_of(delegate_pkh, baker2_pkh, None)

        delegation.using(baker2).propose_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert not delegation.is_voting_key_of(delegate_pkh, baker2_pkh, None)
        assert delegation.list_voters(delegate_pkh, None) == []

    def test_not_claiming_should_not_set_voting_key(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)
        delegates = delegation.list_voters(delegate_pkh, None)
        assert not (baker_pkh in delegates)

    def test_once_claimed_no_need_to_claim_again_to_update_same_key(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()
        delegation.using(baker).propose_voting_key(delegate_pkh, False, {delegation.address}).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)

    def test_need_to_claim_again_after_completely_removing_voting_key(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()
        
        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)
        
        delegation.using(baker).propose_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)

        delegation.using(baker).propose_voting_key(delegate_pkh, True, None).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)

        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()
        
        assert delegation.is_voting_key_of(delegate_pkh, baker_pkh, delegation.address)

    def test_cant_claim_unproposed_voting_key(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        delegation = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        delegation.using(delegate).claim_voting_rights(baker_pkh).send()
        self.bake_block()

        assert not delegation.is_voting_key_of(delegate_pkh, baker_pkh, None)

    def test_should_fail_if_tez_in_transaction_propose_voting_key(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()

        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            delegation.using(baker).claim_voting_rights(pkh(baker)).with_amount(1).send()

    def test_should_fail_if_tez_in_transaction_claim_voting_rights(self) -> None:
        baker = self.bootstrap_baker()
        delegation = self.deploy_delegated_governance()

        with self.raisesMichelsonError(TEZ_IN_TRANSACTION_DISALLOWED):
            delegation.using(baker).propose_voting_key(pkh(baker), True, None).with_amount(1).send()