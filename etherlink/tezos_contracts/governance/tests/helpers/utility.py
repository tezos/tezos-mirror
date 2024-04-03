from pytezos.client import PyTezosClient
from pytezos.contract.interface import ContractInterface
from pytezos.operation.group import OperationGroup
from os.path import dirname
from os.path import join
from pytezos.michelson.parse import michelson_to_micheline
from pytezos.michelson.types.base import MichelsonType
from typing import Any


DEFAULT_ADDRESS = 'tz1burnburnburnburnburnburnburjAYjjX'
DEFAULT_VOTING_POWER = 4000000000000
DEFAULT_TOTAL_VOTING_POWER = 20000000000000

TEST_ADDRESSES_SET = [
    'tz1RuHDSj9P7mNNhfKxsyLGRDahTX5QD1DdP',
    'tz1Xf8zdT3DbAX9cHw3c3CXh79rc4nK4gCe8',
    'tz1V16tR1LMKRernkmXzngkfznmEcTGXwDuk',
    'tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9',
    'tz1Qf1pSbJzMN4VtGFfVJRgbXhBksRv36TxW',
    'tz1YgDUQV2eXm8pUWNz3S5aWP86iFzNp4jnD',
    'tz1iZ9LkpAhN8X1L6RpBtfy3wxpEWzFrXz8j',
    'tz1PirbogVqfmBT9XCuYJ1KnDx4bnMSYfGru',
    'tz1cg5EqC3WdZgRSvGJeW328S4KQNrT4jvyv',
    'tz1Zt8QQ9aBznYNk5LUBjtME9DuExomw9YRs',
    'tz3ZmB8oWUmi8YZXgeRpgAcPnEMD8VgUa4Ve',
    'tz1XMiZwHpHZ8a1AfwRWKfzLskJgZNyV8PHs',
    'tz1NiaviJwtMbpEcNqSP6neeoBYj8Brb3QPv',
    'tz1TGKSrZrBpND3PELJ43nVdyadoeiM1WMzb',
    'tz1YtjpWwWsTzjoRpSi56eTQnUdomTAtFJih',
    'tz1bQMn5xYFbX6geRxqvuAiTywsCtNywawxH',
    'tz1NFs6yP2sXd5vAAbR43bbDRpV2nahDZope',
    'tz1e42w8ZaGAbM3gucbBy8iRypdbnqUj7oWY',
    'tz3btDQsDkqq2G7eBdrrLqetaAfLVw6BnPez',
    'tz1cjyja1TU6fiyiFav3mFAdnDsCReJ12hPD',
    'tz1LZVSPJw5taDFBVjvcQMUhwehdWgatKbzK',
]
TEST_ADDRESSES_SET.sort()


def pkh(client: PyTezosClient) -> str:
    """Returns public key hash of given client"""

    return str(client.key.public_key_hash())


def find_op_by_hash(client: PyTezosClient, opg: OperationGroup) -> dict:
    """Finds operation group by operation hash"""

    op = client.shell.blocks[-10:].find_operation(opg.hash())
    return op  # type: ignore


def get_address_from_op(op: dict) -> str:
    """Returns originated contract address from given operation dict"""

    contents = op['contents']
    assert len(contents) == 1, 'multiple origination not supported'
    op_result: dict = contents[0]['metadata']['operation_result']
    contracts = op_result['originated_contracts']
    assert len(contracts) == 1, 'multiple origination not supported'
    originated_contract = contracts[0]
    assert isinstance(originated_contract, str)
    return originated_contract


def get_build_dir() -> str:
    """Returns path to the build directory"""

    return join(dirname(__file__), '..', '..', 'build')

def get_tests_dir() -> str:
    """Returns path to the test directory"""

    return join(dirname(__file__), '..')


def load_contract_from_address(
    client: PyTezosClient, contract_address: str
) -> ContractInterface:
    """Loads contract from given address using given client"""

    contract = client.contract(contract_address)
    contract = contract.using(shell=client.shell, key=client.key)
    return contract


def to_micheline(type_expression: str) -> dict:
    """Converts Michelson type expression string to Micheline expression
    (reusing pytezos.michelson.parse.michelson_to_micheline) with
    type checking
    """

    return michelson_to_micheline(type_expression)  # type: ignore


def to_michelson_type(object: Any, type_expression: str) -> MichelsonType:
    """Converts Python object to Michelson type using given type expression"""

    micheline_expression = to_micheline(type_expression)
    michelson_type = MichelsonType.match(micheline_expression)
    return michelson_type.from_python_object(object)


def pack(object: Any, type_expression: str) -> bytes:
    """Packs Python object to bytes using given type expression"""

    return to_michelson_type(object, type_expression).pack()


def pack_sequencer_payload(payload):
    return {
        'sequencer_pk': payload['sequencer_pk'],
        'pool_address': bytes.fromhex(payload['pool_address'])
    }

def originate_from_file(
    filename: str, client: PyTezosClient, storage: Any
) -> OperationGroup:
    """Deploys contract from filename with given storage
    using given client and returns OperationGroup"""

    print(f'deploying contract from filename {filename}')
    raw_contract = ContractInterface.from_file(filename)
    contract = raw_contract.using(key=client.key, shell=client.shell)
    return contract.originate(initial_storage=storage)

def get_digits(value: float) -> int:
    result = 0
    while(value % 1 != 0):
        result += 1
        value *= 10
    return result

def normalize_params(values : list[float]) -> list[int]:
    max_digits = max(map(get_digits, values))
    return list(map(lambda v: int(v * 10**max_digits), values))

def validate_percent_value(value : float):
    if(not (0 <= value <= 100)):
        raise Exception(f'Incorrect percentage value \'{value}\'. Should be in range [0, 100]') 
