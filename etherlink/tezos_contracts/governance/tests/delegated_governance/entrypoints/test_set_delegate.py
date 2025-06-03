from tests.base import BaseTestCase
from tests.helpers.errors import (
    NO_VOTING_POWER
)
from tests.helpers.utility import pkh

class DelegatedGovernanceSetDelegateTestCase(BaseTestCase):
    def test_should_fail_if_no_power_voting(self) -> None:
        no_baker = self.bootstrap_no_baker()
        delegate_address = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()

        delegate_pkh = pkh(delegate_address)

        with self.raisesMichelsonError(NO_VOTING_POWER):
            governance.using(no_baker).set_voting_key(delegate_pkh, True, None).send()

    def test_should_delegate_with_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        whitelist = {etherlink.address}

        governance.using(baker).set_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        delegates = governance.list_voters(delegate_pkh, etherlink.address)
        assert baker_pkh in delegates
    
    def test_should_delegate_to_same_address_for_two_bakers_with_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        whitelist = {etherlink.address}

        governance.using(baker).set_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()

        governance.using(baker2).set_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        assert governance.is_voting_key_of(delegate_pkh, baker2_pkh, etherlink.address)
        delegates = governance.list_voters(delegate_pkh, etherlink.address)
        assert baker_pkh, baker2_pkh in delegates

    def test_should_delegate_with_blacklist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        blacklist = {etherlink.address}

        governance.using(baker).set_voting_key(delegate_pkh, False, blacklist).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        delegates = governance.list_voters(delegate_pkh, etherlink.address)
        assert baker_pkh not in delegates

    def test_should_remove_delegate(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        governance.using(baker).set_voting_key(delegate_pkh, True, None).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, None)

        governance.using(baker).set_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, None)

    def test_should_update_delegate_whitelist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})
        sequencer = self.deploy_sequencer_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        whitelist1 = {etherlink.address}
        whitelist2 = {sequencer.address}

        governance.using(baker).set_voting_key(delegate_pkh, True, whitelist1).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

        governance.using(baker).set_voting_key(delegate_pkh, True, whitelist2).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

    def test_delegate_for_all_contracts(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        governance.using(baker).set_voting_key(pkh(delegate), True, None).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, None)
    
    def test_should_cleanup_delegate_map_when_empty(self) -> None:
       baker = self.bootstrap_baker()
       delegate = self.bootstrap_no_baker()
       governance = self.deploy_delegated_governance()

       baker_pkh = pkh(baker)
       delegate_pkh = pkh(delegate)

       governance.using(baker).set_voting_key(delegate_pkh, True, None).send()
       self.bake_block()

       assert governance.is_voting_key_of(delegate_pkh, baker_pkh, None)

       governance.using(baker).set_voting_key(delegate_pkh, False, None).send()
       self.bake_block()

       assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, None)
       assert governance.list_voters(delegate_pkh, None) == []

    def test_multiple_delegations(self) -> None:
        baker = self.bootstrap_baker()
        delegate1 = self.bootstrap_no_baker()
        delegate2 = self.bootstrap_no_baker()
        delegate3 = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()

        baker_pkh = pkh(baker)
        delegate1_pkh = pkh(delegate1)
        delegate2_pkh = pkh(delegate2)
        delegate3_pkh = pkh(delegate3)

        governance.using(baker).set_voting_key(delegate1_pkh, True, None).send()
        self.bake_block()
        governance.using(baker).set_voting_key(delegate2_pkh, True, None).send()
        self.bake_block()
        governance.using(baker).set_voting_key(delegate3_pkh, True, None).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate1_pkh, baker_pkh, None)
        assert governance.is_voting_key_of(delegate2_pkh, baker_pkh, None)
        assert governance.is_voting_key_of(delegate3_pkh, baker_pkh, None)

        delegates = governance.list_voters(delegate1_pkh, None)
        assert baker_pkh in delegates
        delegates = governance.list_voters(delegate2_pkh, None)
        assert baker_pkh in delegates
        delegates = governance.list_voters(delegate3_pkh, None)
        assert baker_pkh in delegates

    def test_update_blacklist(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})
        sequencer = self.deploy_sequencer_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        blacklist1 = {etherlink.address}
        blacklist2 = {sequencer.address}

        governance.using(baker).set_voting_key(delegate_pkh, False, blacklist1).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

        governance.using(baker).set_voting_key(delegate_pkh, False, blacklist2).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)
        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, sequencer.address)

    def test_mixed_whitelist_blacklist(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        whitelist = {etherlink.address}
        blacklist = {etherlink.address}

        governance.using(baker1).set_voting_key(delegate_pkh, True, whitelist).send()
        self.bake_block()

        governance.using(baker2).set_voting_key(delegate_pkh, False, blacklist).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker1_pkh, etherlink.address)
        assert not governance.is_voting_key_of(delegate_pkh, baker2_pkh, etherlink.address)

    def test_empty_lists(self) -> None:
        baker = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()
        etherlink = self.deploy_kernel_governance(custom_config={"delegation_contract": governance.address})

        baker_pkh = pkh(baker)
        delegate_pkh = pkh(delegate)

        empty_whitelist = set()
        governance.using(baker).set_voting_key(delegate_pkh, True, empty_whitelist).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)

        empty_blacklist = set()
        governance.using(baker).set_voting_key(delegate_pkh, False, empty_blacklist).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker_pkh, etherlink.address)

    def test_cross_delegation(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        baker3 = self.bootstrap_baker()
        governance = self.deploy_delegated_governance()

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        baker3_pkh = pkh(baker3)

        governance.using(baker1).set_voting_key(baker2_pkh, True, None).send()
        self.bake_block()

        governance.using(baker2).set_voting_key(baker3_pkh, True, None).send()
        self.bake_block()

        assert governance.is_voting_key_of(baker2_pkh, baker1_pkh, None)
        assert governance.is_voting_key_of(baker3_pkh, baker2_pkh, None)
        assert not governance.is_voting_key_of(baker3_pkh, baker1_pkh, None)

    def test_storage_cleanup_comprehensive(self) -> None:
        baker1 = self.bootstrap_baker()
        baker2 = self.bootstrap_baker()
        delegate = self.bootstrap_no_baker()
        governance = self.deploy_delegated_governance()

        baker1_pkh = pkh(baker1)
        baker2_pkh = pkh(baker2)
        delegate_pkh = pkh(delegate)

        governance.using(baker1).set_voting_key(delegate_pkh, True, None).send()
        self.bake_block()
        governance.using(baker2).set_voting_key(delegate_pkh, True, None).send()
        self.bake_block()

        assert governance.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert governance.is_voting_key_of(delegate_pkh, baker2_pkh, None)

        governance.using(baker1).set_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert governance.is_voting_key_of(delegate_pkh, baker2_pkh, None)

        governance.using(baker2).set_voting_key(delegate_pkh, False, None).send()
        self.bake_block()

        assert not governance.is_voting_key_of(delegate_pkh, baker1_pkh, None)
        assert not governance.is_voting_key_of(delegate_pkh, baker2_pkh, None)
        assert governance.list_voters(delegate_pkh, None) == []