from scripts.contract_type import ContractType

base_template = {
    'version': '0.1.0',
    'interfaces': ['TZIP-016'],
    'license': {'name': 'MIT'},
    'homepage': 'https://www.etherlink.com', 
}

kernel_regular_governance_metadata = base_template | {
    'name': 'Etherlink Kernel Governance',
    'description': 'The contract allows bakers to make proposals and vote on the kernel upgrade (kernel hash)',
}

kernel_security_governance_metadata = base_template | {
    'name': 'Etherlink Kernel Security Governance',
    'description': 'The contract allows bakers to make proposals and vote on the security kernel upgrade (kernel hash)',
}

sequencer_governance_metadata = base_template | {
    'name': 'Etherlink Sequencer Committee Governance',
    'description': 'The contract allows bakers to make proposals and vote on the sequencer operator',
}

metadata_by_contract_type = {
    ContractType.kernel_regular_governance: kernel_regular_governance_metadata,
    ContractType.kernel_security_governance: kernel_security_governance_metadata,
    ContractType.sequencer_governance: sequencer_governance_metadata,
}