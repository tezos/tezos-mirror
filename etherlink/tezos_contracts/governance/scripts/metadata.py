from scripts.contract_type import ContractType

base_template = {
    'version': '0.1.0',
    'interfaces': ['TZIP-016'],
    'license': {'name': 'MIT'},
    'homepage': 'https://www.etherlink.com', 
}

kernel_slow_governance_metadata = base_template | {
    'name': 'Etherlink Slow Kernel Governance',
    'description': 'The contract allows bakers to make proposals and vote on the kernel upgrade (kernel hash) according to the slow kernel governance process',
}

kernel_fast_governance_metadata = base_template | {
    'name': 'Etherlink Fast Kernel Governance',
    'description': 'The contract allows bakers to make proposals and vote on the kernel upgrade (kernel hash) according to the fast kernel governance process',
}

sequencer_governance_metadata = base_template | {
    'name': 'Etherlink Sequencer Committee Governance',
    'description': 'The contract allows bakers to make proposals and vote on the sequencer operator',
}

delegated_governance_metadata = base_template | {
    'name': 'Etherlink Delegated Governance',
    'description': 'The contract allows bakers to delegate their voting rights to another key',
}

metadata_by_contract_type = {
    ContractType.kernel_slow_governance: kernel_slow_governance_metadata,
    ContractType.kernel_fast_governance: kernel_fast_governance_metadata,
    ContractType.sequencer_governance: sequencer_governance_metadata,
    ContractType.delegated_governance: delegated_governance_metadata,
}