from pytezos.client import PyTezosClient
from tests.helpers.contracts.governance_base import GovernanceBase
from tests.helpers.utility import (
    get_build_dir,
    originate_from_file,
)
from pytezos.operation.group import OperationGroup
from pytezos.contract.call import ContractCall
from os.path import join
from tests.helpers.metadata import Metadata


class KernelGovernance(GovernanceBase):
    @classmethod
    def originate(self, client: PyTezosClient, custom_config=None, metadata=None) -> OperationGroup:
        """Deploys Kernel Governance"""

        metadata = metadata if metadata != None else dict()
        metadata = Metadata.make_default(**metadata)
        storage = self.make_storage(metadata, custom_config=custom_config)
        filename = join(get_build_dir(), 'kernel_governance.tz')

        return originate_from_file(filename, client, storage)
    

    def trigger_kernel_upgrade(self, rollup_address : str) -> ContractCall:
        """Triggers kernel upgrade transaction to rollup with last winner kernel hash"""

        return self.contract.trigger_kernel_upgrade(rollup_address)
    
    
    def new_proposal(self, kernel_root_hash : bytes) -> ContractCall:
        """Creates a new proposal"""

        return self.contract.new_proposal(kernel_root_hash)
    
    
    def upvote_proposal(self, kernel_root_hash : bytes) -> ContractCall:
        """Upvotes an exist proposal"""

        return self.contract.upvote_proposal(kernel_root_hash)
    
    def vote(self, vote : str) -> ContractCall:
        """Votes for a kernel_root_hash in promotion period"""

        return self.contract.vote(vote)