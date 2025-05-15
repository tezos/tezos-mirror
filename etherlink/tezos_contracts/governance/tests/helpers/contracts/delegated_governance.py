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

        storage = {}
        filename = join(get_build_dir(), 'delegated_governance.tz')

        print(f'file :  {filename}')
        return originate_from_file(filename, client, storage)
    
    def set_delegate(self, delegate : str, is_delegate : bool, opt_addresses: Optional[set[str]] = None) -> ContractCall:
        """Adds or removes the caller's address to the set associated with the address in the big_map"""

        return self.contract.default(delegate, is_delegate, opt_addresses)
    
    def is_delegate(self, delegate : str, voter : str, contract : Optional[set[str]]):
        return self.contract.is_delegate(delegate, voter, contract).run_view()
    
    def list_delegates(self, delegate : str, contract : Optional[set[str]]):
        return self.contract.list_delegates(delegate, contract).run_view()