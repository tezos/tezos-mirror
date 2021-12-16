from os import path
from typing import List
from tools import paths


def all_contracts(directories: List[str] = None) -> List[str]:
    return paths.all_contracts(CONTRACT_PATH, directories)


def all_legacy_contracts() -> List[str]:
    return all_contracts(['legacy'])


CONTRACT_PATH = path.join(paths.TEZOS_HOME, 'tests_python', 'contracts_012')
ATTIC_CONTRACT_PATH = path.join(CONTRACT_PATH, 'attic')
MACROS_CONTRACT_PATH = path.join(CONTRACT_PATH, 'macros')
ILLTYPED_CONTRACT_PATH = path.join(CONTRACT_PATH, 'ill_typed')
LEGACY_CONTRACT_PATH = path.join(CONTRACT_PATH, 'legacy')
OPCODES_CONTRACT_PATH = path.join(CONTRACT_PATH, 'opcodes')
MINI_SCENARIOS_CONTRACT_PATH = path.join(CONTRACT_PATH, 'mini_scenarios')
