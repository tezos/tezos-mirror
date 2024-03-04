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


class SequencerGovernance(GovernanceBase):
    @classmethod
    def originate(self, client: PyTezosClient, custom_config=None, metadata=None) -> OperationGroup:
        """Deploys Sequencer Governance"""

        metadata = metadata if metadata != None else dict()
        metadata = Metadata.make_default(**metadata)
        storage = self.make_storage(metadata, custom_config=custom_config)
        filename = join(get_build_dir(), 'sequencer_governance.tz')

        return originate_from_file(filename, client, storage)
    
    def new_proposal(self, sequencer_pk : str, pool_address : bytes) -> ContractCall:
        """Creates a new proposal"""

        return self.contract.new_proposal(sequencer_pk, pool_address)
    
    
    def upvote_proposal(self, sequencer_pk : str, pool_address : bytes) -> ContractCall:
        """Upvotes an exist proposal"""

        return self.contract.upvote_proposal(sequencer_pk, pool_address)
    
    def vote(self, vote : str) -> ContractCall:
        """Votes for a hash in promotion period"""

        return self.contract.vote(vote)

    def trigger_committee_upgrade(self, rollup_address : str) -> ContractCall:
        """Triggers upgrade transaction to rollup with last winner committee addresses set"""

        return self.contract.trigger_committee_upgrade(rollup_address)
