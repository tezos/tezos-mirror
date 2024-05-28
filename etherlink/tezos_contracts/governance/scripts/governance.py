import json
from pytezos import pytezos, PyTezosClient
from scripts.contract_type import ContractType
from tests.helpers.contracts.sequencer_governance import SequencerGovernance
from tests.helpers.contracts.kernel_governance import KernelGovernance
from typing import Optional
import click
from scripts.environment import load_or_ask
from tests.helpers.metadata import Metadata
from tests.helpers.utility import normalize_params, validate_percent_value
from scripts.metadata import metadata_by_contract_type

@click.command()
@click.option(
    '--contract',
    required=True,
    help='"kernel_governance" or "sequencer_governance"',
)
@click.option(
    '--period_length',
    required=True,
    help='The proposal and promotion period life-time counted in blocks',
)
@click.option(
    '--adoption_period_sec',
    required=True,
    help='The duration of the l2 adoption period counted in seconds',
)
@click.option(
    '--upvoting_limit',
    required=True,
    help='The max number of new active proposals for each account',
)
@click.option(
    '--proposal_quorum_percent',
    required=True,
    help='The min proposal proposal_quorum to move the proposal to next promotion phase. Range: [0, 100]',
)
@click.option(
    '--promotion_quorum_percent',
    required=True,
    help='The min promotion_quorum for the proposal to be considered as a winner. Range: [0, 100]',
)
@click.option(
    '--promotion_supermajority_percent',
    required=True,
    help='The min promotion_supermajority for the proposal be considered as a winner. Range: [0, 100]',
)
@click.option('--private-key', default=None, help='Use the provided private key.')
@click.option('--rpc-url', default=None, help='Tezos RPC URL.')
def deploy_contract(
    contract: str,
    period_length: str,
    adoption_period_sec: str,
    upvoting_limit: str,
    proposal_quorum_percent: str,
    promotion_quorum_percent: str,
    promotion_supermajority_percent: str,
    private_key: Optional[str],
    rpc_url: Optional[str],
) -> KernelGovernance:
    """Deploys a governance contract using provided key as a manager"""

    contract_type = ContractType[contract]
    period_length = int(period_length)
    adoption_period_sec = int(adoption_period_sec)
    upvoting_limit = int(upvoting_limit)
    proposal_quorum = float(proposal_quorum_percent)
    promotion_quorum = float(promotion_quorum_percent)
    promotion_supermajority = float(promotion_supermajority_percent)

    validate_percent_value(proposal_quorum)
    validate_percent_value(promotion_quorum)
    validate_percent_value(promotion_supermajority)

    private_key = private_key or load_or_ask('PRIVATE_KEY', is_secret=True)
    rpc_url = rpc_url or load_or_ask('RPC_URL')

    manager = pytezos.using(shell=rpc_url, key=private_key)
    blockchain_info = get_blockchain_info(manager)
    protocol_voting_period_length = blockchain_info['protocol_voting_period_length']
    protocol_voting_started_at_level = blockchain_info['protocol_voting_started_at_level']

    print('')
    print('blockchain_info:', json.dumps(blockchain_info, indent=4))

    if (protocol_voting_period_length % period_length) != 0: 
        raise Exception(f'Period length is incorrect, it must be a divisor of the length of the Tezos protocol voting period length. protocol_period_length={protocol_voting_period_length}, period_length={period_length}') 

    normalized_params = normalize_params([100, proposal_quorum, promotion_quorum, promotion_supermajority])
    [scale, proposal_quorum, promotion_quorum, promotion_supermajority] = normalized_params
    metadata = metadata_by_contract_type[contract_type]
    
    config = {
        'started_at_level': protocol_voting_started_at_level,
        'period_length': period_length,
        'adoption_period_sec': adoption_period_sec,
        'upvoting_limit': upvoting_limit,
        'scale': scale,
        'proposal_quorum': proposal_quorum,
        'promotion_quorum': promotion_quorum,
        'promotion_supermajority': promotion_supermajority,
    }

    print('')
    print('smart_contact_config:', json.dumps(config, indent=4))

    print('')
    print('smart_contact_metadata:', json.dumps(metadata, indent=4))

    print('')
    if not click.confirm('Do you want to originate the smart contract?', default=True):
        return 

    opg = originate_contract(contract_type, manager, config, metadata)
    manager.wait(opg)
    kernelGovernance = KernelGovernance.from_opg(manager, opg)
    return kernelGovernance

def get_blockchain_info(manager: PyTezosClient) -> dict:
    info = manager.shell.head.metadata()
    current_level = int(info['level_info']['level'])
    protocol_voting_position = int(info['voting_period_info']['position'])
    protocol_voting_started_at_level = current_level - protocol_voting_position
    protocol_voting_period_length = protocol_voting_position + int(info['voting_period_info']['remaining']) + 1

    return {
        'rpc_url': manager.shell.node.uri[0],
        'current_level': current_level,
        'protocol_voting_period_length': protocol_voting_period_length,
        'protocol_voting_position': protocol_voting_position,
        'protocol_voting_started_at_level': protocol_voting_started_at_level
    }

def originate_contract(contract_type: ContractType, manager: PyTezosClient, config: dict, metadata: dict):
    if contract_type in [ContractType.kernel_regular_governance, ContractType.kernel_security_governance]:
        return KernelGovernance.originate(manager, config, metadata).send()
    elif contract_type == ContractType.sequencer_governance:
        return SequencerGovernance.originate(manager, config, metadata).send()
    else:
        raise ValueError("Incorrect contract_type")
