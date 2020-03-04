from os import path
import os
from typing import List


def tezos_home() -> str:
    # we rely on a fixed directory structure of the tezos repo
    # TEZOS_HOME/tests_python/tools/paths.py
    cur_path = os.path.dirname(os.path.realpath(__file__))
    tezos_home = os.path.dirname(os.path.dirname(cur_path)) + '/'
    assert os.path.isfile(f'{tezos_home}/tests_python/tools/paths.py')
    return tezos_home


def all_contracts(directories: List[str] = None) -> List[str]:
    if directories is None:
        directories = ['attic', 'opcodes',
                       'macros', 'mini_scenarios', 'non_regression']
    contracts = []
    for directory in directories:
        for contract in os.listdir(path.join(CONTRACT_PATH, directory)):
            contracts.append(path.join(directory, contract))
    return contracts


def all_deprecated_contracts() -> List[str]:
    return all_contracts(['deprecated'])


# Use environment variable if tests_python are put outside the tezos
# TEZOS_HOME = os.environ.get('TEZOS_HOME')
TEZOS_HOME = tezos_home()
TEZOS_BINARIES = os.environ.get('TEZOS_BINARIES')

CONTRACT_PATH = path.join(TEZOS_HOME, 'tests_python', 'contracts')
MACROS_CONTRACT_PATH = path.join(CONTRACT_PATH, 'macros')
ILLTYPED_CONTRACT_PATH = path.join(CONTRACT_PATH, 'ill_typed')
DEPRECATED_CONTRACT_PATH = path.join(CONTRACT_PATH, 'deprecated')
OPCODES_CONTRACT_PATH = path.join(CONTRACT_PATH, 'opcodes')
MINI_SCENARIOS_CONTRACT_PATH = path.join(CONTRACT_PATH,
                                         'mini_scenarios')
ACCOUNT_PATH = path.join(TEZOS_HOME, 'tests_python', 'account')
