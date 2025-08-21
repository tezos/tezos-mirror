from pytezos.client import PyTezosClient
from tests.helpers.contracts.contract import ContractHelper
from tests.helpers.utility import (
    get_build_dir,
    originate_from_file,
)
from pytezos.operation.group import OperationGroup
from pytezos.contract.call import ContractCall
from os.path import join
from typing import Optional


class DelegatedGovernance(ContractHelper):
    @classmethod
    def originate(self, client: PyTezosClient) -> OperationGroup:
        """Deploys Delegated Governance"""

        storage = {
            "voting_delegations": {},
            "proposals": {}
        }
        filename = join(get_build_dir(), 'delegated_governance.tz')

        print(f'file :  {filename}')
        return originate_from_file(filename, client, storage)
    
    def propose_voting_key(self, delegate : str, is_delegate : bool, opt_addresses: Optional[set[str]] = None) -> ContractCall:
        return self.contract.propose_voting_key(delegate, is_delegate, opt_addresses)
    
    def claim_voting_rights(self, baking_key : str) -> ContractCall:
        return self.contract.claim_voting_rights(baking_key)
    
    def is_voting_key_of(self, delegate : str, voter : str, contract : Optional[set[str]]):
        return self.contract.is_voting_key_of(delegate, voter, contract).run_view()
    
    def list_voters(self, delegate : str, contract : Optional[set[str]]):
        return self.contract.list_voters(delegate, contract).run_view()

