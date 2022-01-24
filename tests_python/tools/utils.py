""" Utility functions to check time-dependent assertions in the tests.
Assertions are retried to avoid using arbitrary time constants in test.
"""
import datetime
from typing import Any, List, Tuple, Pattern, Callable
import hashlib
import contextlib
import json
import os
import re
import subprocess
import time

import urllib.error
import base58check
import ed25519
import pyblake2
import requests
from client.client import Client
from client.client_output import (
    BakeForResult,
    RunScriptResult,
    InvalidClientOutput,
)

from . import constants


def retry(timeout: float, attempts: float):  # pylint: disable=unused-argument
    """Retries execution of a decorated function until it returns True.

    Args:
        attempts (int): max number of attempts.
        timeout (float): time to wait between attempts.

    Returns:
        True iff an attempt was successful.
    """

    def decorator_retry(func):
        def wrapper(*args, **kwargs):
            nonlocal timeout, attempts
            while not func(*args, **kwargs):
                if attempts == 0:
                    print("*** Failed after too many retries")
                    return False
                print(f'*** Will retry after {timeout} seconds...')
                time.sleep(timeout)
                attempts -= 1
            return True

        return wrapper

    return decorator_retry


@retry(timeout=1.0, attempts=10)
def check_block_contains_operations(
    client: Client, operation_hashes: List[str]
) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/operation_hashes')
    flatten = (
        res[0] + res[1] + res[2] + res[3]
        if res is not None and len(res) == 4
        else []
    )
    return all(oh in flatten for oh in operation_hashes)


@retry(timeout=1.0, attempts=20)
def check_mempool_contains_operations(
    client: Client, operation_hashes: List[str]
) -> bool:
    mempool = client.get_mempool()['applied']
    res = {x['hash'] for x in mempool}
    return set(operation_hashes).issubset(res)


@retry(timeout=2.0, attempts=120)
def check_protocol(
    client: Client, proto: str, params: List[str] = None
) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/metadata', params=params)
    return res['next_protocol'] == proto


@retry(timeout=1.0, attempts=20)
def check_two_chains(client: Client) -> bool:
    main_id = client.rpc('get', 'chains/main/chain_id')
    try:
        test_id = client.rpc('get', 'chains/test/chain_id')
        return test_id != main_id
    except InvalidClientOutput:
        return False


@retry(timeout=2.0, attempts=10)
def check_level(client: Client, level, chain: str = 'main') -> bool:
    return client.get_level(chain=chain) == level


@retry(timeout=2.0, attempts=10)
def check_level_greater_than(
    client: Client, level, chain: str = 'main'
) -> bool:
    return client.get_level(chain=chain) >= level


@retry(timeout=2.0, attempts=20)
def check_operation_in_receipt(
    client: Client, operation_hash: str, check_previous=None
) -> bool:
    extra_param = (
        ['--check-previous', str(check_previous)] if check_previous else []
    )
    receipt = client.get_receipt(operation_hash, extra_param)
    # TODO deal with case where operation isn't included yet
    return receipt.block_hash is not None


@retry(timeout=5, attempts=20)
def synchronize(clients: List[Client], max_diff: int = 2) -> bool:
    """Return when nodes head levels are within max_diff units"""
    levels = [client.get_level() for client in clients]
    return max(levels) - min(levels) <= max_diff


@retry(timeout=2, attempts=2)
def check_is_bootstrapped(client: Client, chain: str = 'main') -> bool:
    return client.is_bootstrapped(chain)


@contextlib.contextmanager
def assert_request_failure(matcher: Callable[[urllib.error.HTTPError], None]):
    """Context manager that checks enclosed code fails with expected
    HTTP error.

    This context manager checks the contextualized code (in the [with]
    statement) raises [urllib.error.HTTPError] exception, and
    that the string representation of the exception matches [pattern].

    It fails with an [assert False] otherwise.

    Args:
        pattern (Pattern): the pattern that the HTTP exception should match
    """

    try:
        yield None
        assert False, "Code ran without throwing exception"
    except urllib.error.HTTPError as exc:
        matcher(exc)

    except Exception as exc:  # pylint: disable=broad-except
        assert_msg = f'Expected urllib.error.HTTPError but got {type(exc)}'
        assert False, assert_msg


def get_block_at_level(
    client: Client, level: int, expect_failure: bool = False
) -> dict:
    """Return the block at a given level, level must be less or equal
    than the current head. If the level is higher than the
    current, it will fail unless expect_failure is True"""

    # Error that must be raised when a block is unknown
    def error_matcher(exc: urllib.error.HTTPError) -> None:
        assert str(exc) == 'HTTP Error 404: Not Found'

    path = f'/chains/main/blocks/{level}'
    if not expect_failure:
        return client.rpc_raw('get', path)
    with assert_request_failure(error_matcher):
        client.rpc_raw('get', path)
        assert False  # not reachable


def get_block_header_at_level(client: Client, level: int) -> dict:
    """Return the block header at a given level, level must be less
    or equal than the current head. If the level is higher than
    the current, it will fail"""
    block = client.rpc_raw('get', f'/chains/main/blocks/{level}/header')
    return block


def get_block_metadata_at_level(
    client: Client, level: int, expect_failure: bool = False
) -> dict:
    """Return the block metadata at a given level, level must be less
    or equal than the current head. If the level is higher than
    the current, it will fail unless expect_failure is True"""

    # Error that must be raised when a block is not available
    def error_matcher(exc: urllib.error.HTTPError) -> None:
        assert str(exc) == 'HTTP Error 500: Internal Server Error'
        errors = json.loads(exc.read().decode())
        assert errors[0]['id'] == 'store.metadata_not_found'

    path = f'/chains/main/blocks/{level}/metadata'
    if not expect_failure:
        block = client.rpc_raw('get', path)
        return block
    with assert_request_failure(error_matcher):
        client.rpc_raw('get', path)
        assert False  # not reachable


def get_block_hash(client: Client, level: int) -> str:
    """Return block hash at given level, level must be less or equal
    than the current head"""
    block_hash = get_block_at_level(client, level)['hash']
    assert isinstance(block_hash, str)
    return str(block_hash)


def all_blocks(client: Client) -> List[dict]:
    """Return list of all blocks"""
    cur = 'head'
    blocks = []
    while True:
        block = client.rpc('get', f'/chains/main/blocks/{cur}')
        blocks.append(block)
        cur = block['header']['predecessor']
        if block['header']['level'] == 0:
            break
    return list(reversed(blocks))


def operations_hash_from_block(block):
    # TODO type
    _, _, _, operations = block['operations']
    res = []
    for operation in operations:
        res.append(operation['hash'])
    return res


def check_logs(logs: List[str], pattern: str) -> bool:
    for file in logs:
        with open(file, "r") as stream:
            for line in stream:
                if re.search(pattern, line):
                    print('#', stream.name)
                    print(line)
                    return False
    return True


def check_logs_counts(logs: List[str], pattern: str) -> int:
    count = 0
    for file in logs:
        with open(file, "r") as stream:
            for line in stream:
                if re.search(pattern, line):
                    print('#', stream.name)
                    print(line)
                    count += 1
    return count


def activate_protocol(
    client, proto, parameters=None, timestamp=None, activate_in_the_past=False
):
    """Activates a protocol.

    If `activate_in_the_past` is True, protocol is activated with a timestamp
    one year in the past."""
    if parameters is None:
        parameters = constants.ALPHA_PARAMETERS
    delay = None
    if activate_in_the_past:
        delay = datetime.timedelta(seconds=3600 * 24 * 365)
    client.activate_protocol_json(
        proto, parameters, timestamp=timestamp, delay=delay
    )


def activate_alpha(
    client, parameters=None, timestamp=None, activate_in_the_past=False
):
    activate_protocol(
        client, constants.ALPHA, parameters, timestamp, activate_in_the_past
    )


def pprint(json_data: dict) -> None:
    print(json.dumps(json_data, indent=4, sort_keys=True))


def rpc(
    server: str,
    port: int,
    verb: str,
    path: str,
    data: Any = None,
    headers: dict = None,
):
    """Calls a REST API

    Simple wrapper over `requests` methods.
    See `https://2.python-requests.org/en/master/`.

    Parameters:
        server (str): server name/IP
        port (int):  server port
        verb (str): 'get', 'post' or 'options'
        path (str): path of the RPC
        data (dict): json data if post method is used
        headers (dicts): optional headers

    Returns:
        A `Response` object."""

    assert verb in {'get', 'post', 'options'}
    full_path = f'http://{server}:{port}/{path}'
    print(f'# calling RPC {verb} {full_path}')
    if verb == 'get':
        res = requests.get(full_path, headers=headers)
    elif verb == 'post':
        print('# post data BEGIN')
        if data is None:
            data = {}
        pprint(data)
        print('# END')
        res = requests.post(full_path, json=data, headers=headers)
    else:
        res = requests.options(full_path, json=data, headers=headers)
    return res


def sign(data: bytes, secret_key: bytes) -> str:
    """Sign digest of data with secret key

    Uses blake2b hash function (32 bytes digest)
    Ed25519 signing scheme

    Parameters:
        data (bytes): data to be signed
        secret_key (bytes): secret key

    Returns:
        str: signature of digest of data (hex string)
    """
    blake_hash = pyblake2.blake2b(digest_size=32)
    blake_hash.update(data)
    digest = blake_hash.digest()
    res = ed25519.SigningKey(secret_key)
    return res.sign(digest).hex()


def b58_key_to_hex(b58_key: str) -> str:
    """Translate a tezos b58check key encoding to a hex string.

    Params:
        b58_sig (str): tezos b58check encoding of a key

    Returns:
        str: hex string of key
    """
    # we get rid of prefix and final checksum
    return base58check.b58decode(b58_key).hex()[8:-8]


def b58_sig_to_hex(b58_sig: str) -> str:
    """Translate a tezos b58check signature encoding to a hex string.

    Params:
        b58_sig (str): tezos b58check encoding of a signature

    Returns:
        str: hex string of signature
    """
    # we get rid of prefix and final checksum
    return base58check.b58decode(b58_sig).hex()[10:-8]


def hex_sig_to_b58(hexsig: str) -> str:
    """Translate a hex signature to a tezos b58check encoding.

    Params:
        hexsig (str): hex string encoded signature

    Returns:
        str: b58check encoding of signature
    """

    def sha256(data):
        return hashlib.sha256(data).digest()

    bytes_sig = bytes.fromhex(hexsig)
    # Before translating to b58check encoding, we add a prefix at the beginning
    # of the sig, and a checksum at the end
    # The prefix enforces that the b58_sig starts with 'edsig's
    edsig_prefix = bytes([9, 245, 205, 134, 18])
    prefixed_bytes_sig = edsig_prefix + bytes_sig
    checksum = sha256(sha256(prefixed_bytes_sig))[0:4]
    final_sig = prefixed_bytes_sig + checksum
    b58_sig = base58check.b58encode(final_sig)
    return b58_sig.decode('ascii')


def sign_operation(encoded_operation: str, secret_key: str) -> str:
    watermarked_operation = b'\x03' + bytes.fromhex(encoded_operation)
    sender_sk_hex = b58_key_to_hex(secret_key)
    sender_sk_bin = bytes.fromhex(sender_sk_hex)
    sig_hex = sign(watermarked_operation, sender_sk_bin)
    signed_op = encoded_operation + sig_hex
    return signed_op


def mutez_of_tez(tez: float):
    return int(tez * 1000000)


@contextlib.contextmanager
def assert_run_failure(pattern: str, mode: str = 'stderr'):
    """Context manager that checks enclosed code fails with expected error.

    This context manager checks the contextualized code (in the [with]
    statement) raises [subprocess.CalledProcessError] exception, and
    that the called process stdout or stderr matches [pattern].

    It fails with an [assert False] otherwise.

    Args:
        pattern (Pattern): the pattern that the process output should match
        mode (str): if [mode = stderr], try to match process stderr, otherwise
                    stdout

    Note: the contextualized code must fork a subprocess using
          [subprocess.run] and make sure [capture_output=True].
    """
    # TODO this is similar to the pytest context manager [pytest.raises]
    #      that we already use in some tests. See if we can only use one
    #      construct.

    try:
        yield None
        assert False, "Code ran without throwing exception"
    except subprocess.CalledProcessError as exc:
        stdout_output = exc.stdout
        stderr_output = exc.stderr
        data = []  # type: List[str]
        if mode == 'stderr':
            data = stderr_output.split('\n')
        else:
            data = stdout_output.split('\n')
        for line in data:
            if re.search(pattern, line):
                return
        data_pretty = "\n".join(data)
        assert False, f"Could not find '{pattern}' in '{data_pretty}'"
    except Exception as exc:  # pylint: disable=broad-except
        assert_msg = f'Expected CalledProcessError but got {type(exc)}'
        assert False, assert_msg


def assert_storage_contains(
    client: Client, contract: str, expected_storage: str
) -> None:
    actual_storage = client.get_storage(contract)
    equal = actual_storage == expected_storage
    assert equal, (
        f"failed: actual_storage == expected_storage\n"
        f"Actual storage: {actual_storage}\n"
        f"Expected storage: {expected_storage}"
    )


def contract_name_of_file(contract_path: str) -> str:
    return os.path.splitext(os.path.basename(contract_path))[0]


def bake(
    client: Client,
    bake_for: str = 'bootstrap1',
    bake_args: List[str] = None,
) -> BakeForResult:
    default_bake_args = [
        '--minimal-timestamp',
        '--minimal-fees',
        '0',
        '--minimal-nanotez-per-byte',
        '0',
        '--minimal-nanotez-per-gas-unit',
        '0',
    ]
    bake_args = bake_args or default_bake_args
    return client.bake(bake_for, bake_args)


def init_with_transfer(
    client: Client,
    contract: str,
    initial_storage: str,
    amount: float,
    sender: str,
    contract_name: str = None,
):
    if contract_name is None:
        contract_name = contract_name_of_file(contract)
    client.originate(
        contract_name,
        amount,
        sender,
        contract,
        ['-init', initial_storage, '--burn-cap', '10'],
    )
    bake(client)


def assert_balance(
    client: Client, account: str, expected_balance: float
) -> None:
    actual_balance = client.get_balance(account)
    assert actual_balance == expected_balance


def assert_run_script_success(
    client: Client, contract: str, param: str, storage: str
) -> RunScriptResult:
    return client.run_script(contract, param, storage, trace_stack=True)


def assert_run_script_failwith(
    client: Client, contract: str, param: str, storage: str
) -> None:

    pattern = 'script reached FAILWITH instruction'
    with assert_run_failure(pattern):
        client.run_script(contract, param, storage, trace_stack=True)


def assert_typecheck_data_failure(
    client: Client, data: str, typ: str, err: str = 'ill-typed data'
) -> None:
    with assert_run_failure(err):
        client.typecheck_data(data, typ)


def assert_typecheck_failure(
    client: Client,
    script: str,
    err: str = 'ill-typed script',
    file: bool = True,
) -> None:
    with assert_run_failure(err):
        client.typecheck(script, file=file)


def suball(replacements: List[Tuple[Pattern, str]], string: str) -> str:
    """Apply all the replacements in `replacements`, in order, to `string`
    using re.sub"""
    for (pattern, replacement) in replacements:
        string = re.sub(pattern, replacement, string)
    return string


def client_output_converter(pre):
    """Remove variable substrings from client output for regression testing.

    This function is used to remove strings from client output that
    are likely to change from one run to another, but whose content
    is (presumably) not important to the regression test. This
    includes timestamps, hashes, counters, nonces, etc.

    For example, a timestamp such as 2019-09-23T10:59:00Z is
    replaced by [TIMESTAMP].
    """

    # Scrub constants
    pre = re.sub(
        r'"proof_of_work_nonce": "\w{16}"',
        '"proof_of_work_nonce": "[NONCE]"',
        pre,
    )
    pre = re.sub(r'"context": "\w{52}"', '"context": "[CONTEXT]"', pre)
    pre = re.sub(r'"level": \d+', '"level": [LEVEL]', pre)
    pre = re.sub(r'"priority": \d+', '"priority": "[PRIORITY]"', pre)
    pre = re.sub(r'"fitness": \[.*\]', '"fitness": "[FITNESS]"', pre)

    # Scrub hashes
    pre = re.sub(r'sig\w{93}', '[SIGNATURE]', pre)
    pre = re.sub(r'\w{53}', '[OPERATION_HASH]', pre)
    pre = re.sub(r'\w{51}', '[BLOCK_HASH]', pre)
    pre = re.sub(r'tz\w{34}', '[CONTRACT_HASH]', pre)
    pre = re.sub(
        r'fees\(\[CONTRACT_HASH\],\d+\)', 'fees([CONTRACT_HASH],[CTR])', pre
    )

    # Scrub receipt
    # FIXME: Maybe use something more specific like
    # Injecting block for [CONTRACT_HASH] at [LEVEL], [ROUND],
    # with [TIMESTAMP], on top of [BLOCK_HASH]
    # [ROUND] and [TIMESTAMP] needs to be created above
    pre = re.sub(r"Injecting block .+", "Injecting block ", pre)

    pre = re.sub(
        r"Operation hash is '\w+'", "Operation hash is '[OPERATION_HASH]'", pre
    )
    pre = re.sub(r'wait for \w+', 'wait for [OPERATION_HASH]', pre)
    pre = re.sub(r'--branch \w+', '--branch [BRANCH_HASH]', pre)
    pre = re.sub(r'KT\w{34}', '[CONTRACT_HASH]', pre)
    pre = re.sub(r'To: KT\w{34} \.\.\.', 'To: [CONTRACT_HASH] ...', pre)
    pre = re.sub(r'Injected block \w{12}', 'Injected block [BLOCK_HASH]', pre)
    pre = re.sub(
        r'Expected counter: \w+', 'Expected counter: [EXPECTED_COUNTER]', pre
    )

    # Scrub timestamps
    pre = re.sub(r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z', '[TIMESTAMP]', pre)

    return pre


def client_always_output_converter(pre):
    """Remove strings we always want to convert"""

    pre = re.sub(
        r'Runtime error in contract \w+:',
        'Runtime error in contract [CONTRACT_HASH]:',
        pre,
    )

    return pre


def assert_transfer_failwith(
    client: Client, amount: float, sender: str, receiver: str, args
) -> None:
    pattern = 'script reached FAILWITH instruction'
    with assert_run_failure(pattern):
        client.transfer(amount, sender, receiver, args)


# Checks that the node's checkpoint, savepoint and caboose are the
# expected ones after a snapshot import.
def node_consistency_after_import(
    node_id,
    sandbox,
    expected_level,
    expected_checkpoint,
    expected_savepoint,
    expected_caboose,
):
    level = sandbox.client(node_id).get_head()['header']['level']
    checkpoint = sandbox.client(node_id).get_checkpoint()['block']['level']
    savepoint = sandbox.client(node_id).get_savepoint()
    caboose = sandbox.client(node_id).get_caboose()
    assert level == expected_level
    assert checkpoint == expected_checkpoint
    assert savepoint == expected_savepoint
    assert caboose == expected_caboose
    # the metadata of genesis are available
    assert get_block_at_level(sandbox.client(node_id), 0)


# Checks the availability of blocks and its metadata for a full node.
# We assume that:
# - genesis is available with metadata
# - all block headers are available,
# - blocks before the savepoint (excluded) have pruned metadata,
# - blocks from the savepoint (included) have metadata.
def full_node_blocks_availability(node_id, sandbox, savepoint, head):
    # Genesis is available with metadata
    assert get_block_at_level(sandbox.client(node_id), 0)
    # [1;…;savepoint[ headers are available but metadata are not
    for i in range(1, savepoint):
        get_block_header_at_level(sandbox.client(node_id), i)
        get_block_metadata_at_level(
            sandbox.client(node_id), i, expect_failure=True
        )
    # [savepoint;…;head] are available with metadata
    for i in range(savepoint, head + 1):
        get_block_metadata_at_level(sandbox.client(node_id), i)


# Checks the availability of blocks and its metadata for a rolling node.
# We assume that:
# - genesis is available with metadata
# - blocks before caboose (excluded) are unknown,
# - blocks from the caboose (included) and before the savepoint (excluded)
#   have pruned metadata,
# - blocks after the savepoint (included) have metadata.
def rolling_node_blocks_availability(
    node_id, sandbox, savepoint, caboose, head
):
    # Genesis is available with metadata
    assert get_block_at_level(sandbox.client(node_id), 0)
    if caboose == 0:
        pass
    else:
        # [1;…;caboose[ blocks are unknown
        for i in range(1, caboose):
            get_block_at_level(sandbox.client(node_id), i, expect_failure=True)
    # [savepoint;…;head] are available with metadata
    for i in range(savepoint + 1, head):
        get_block_header_at_level(sandbox.client(node_id), i)


def file_basename(path):
    return os.path.splitext(os.path.basename(path))[0]


# pylint: disable=dangerous-default-value
# Generic piece of code to originate a contract
def originate(
    client: Client,
    session: dict,
    contract: str,
    init_storage: str,
    amount: int,
    contract_name=None,
    sender='bootstrap1',
    baker='bootstrap5',
    burn_cap='10.0',
    arguments=None,
):
    if contract_name is None:
        contract_name = file_basename(contract)

    args = ['--init', init_storage, '--burn-cap', burn_cap]
    if arguments is not None:
        args += arguments
    origination = client.originate(
        contract_name, amount, sender, contract, args
    )
    session['contract'] = origination.contract
    bake(client, baker)
    assert check_block_contains_operations(client, [origination.operation_hash])
    return origination


# Check if a binary is statically compiled
# used to skip tests
def check_static_binary(execname: str):
    ret = subprocess.Popen(
        ['file', execname], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    # Example of line returned by "file":
    # tezos-node: ELF 64-bit LSB executable, x86-64, version 1 (SYSV),
    # statically linked, no section header
    return "statically linked" in str(ret.communicate()[0])


def get_tezos_node_version() -> str:
    cmd = ["dune", "exec", "../src/lib_version/tezos_print_version.exe"]
    process_ret = subprocess.run(
        cmd, capture_output=True, text=True, check=True
    )
    version = process_ret.stdout.strip()
    assert version, "version should not be empty"
    return version
