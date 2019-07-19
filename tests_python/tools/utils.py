""" Utility functions to check time-dependent assertions in the tests.

Assertions are retried to avoid using arbitrary time constants in test.
"""
from typing import List  # pylint: disable=unused-import
import json
import time
import re
import hashlib
import requests
import ed25519
import base58check
import pyblake2
from client.client import Client
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


@retry(timeout=1., attempts=10)
def check_block_contains_operations(client: Client,
                                    operation_hashes: List[str]) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/operation_hashes')
    flatten = (res[0] + res[1] + res[2] + res[3] if res is not None and
               len(res) == 4 else [])
    return all(oh in flatten for oh in operation_hashes)


@retry(timeout=1., attempts=20)
def check_mempool_contains_operations(client: Client,
                                      operation_hashes: List[str]) -> bool:
    mempool = client.get_mempool()['applied']
    res = {x['hash'] for x in mempool}
    return set(operation_hashes).issubset(res)


@retry(timeout=1., attempts=20)
def check_protocol(client: Client, proto: str,
                   params: List[str] = None) -> bool:
    res = client.rpc('get', '/chains/main/blocks/head/metadata', params=params)
    return res['next_protocol'] == proto


@retry(timeout=1., attempts=10)
def check_level(client: Client, level) -> bool:
    return client.get_level() == level


@retry(timeout=1., attempts=10)
def check_level_greater_than(client: Client, level) -> bool:
    return client.get_level() >= level


@retry(timeout=2., attempts=20)
def check_operation_in_receipt(client: Client,
                               operation_hash: str,
                               check_previous=None) -> bool:
    extra_param = (['--check-previous', str(check_previous)] if
                   check_previous else [])
    receipt = client.get_receipt(operation_hash, extra_param)
    # TODO deal with case where operation isn't included yet
    return receipt.block_hash is not None


@retry(timeout=5, attempts=20)
def synchronize(clients: List[Client], max_diff: int = 2) -> bool:
    """Return when nodes head levels are within max_diff units"""
    levels = [client.get_level() for client in clients]
    return max(levels) - min(levels) <= max_diff


def get_block_hash(client: Client, level: int) -> str:
    """Return block hash at given level, level must be less or equal
       than current head."""
    cur = 'head'
    while True:
        block = client.rpc('get', f'/chains/main/blocks/{cur}')
        assert level <= block['header']['level']
        if block['header']['level'] == level:
            block_hash = block['hash']
            assert isinstance(block_hash, str)
            return str(block)
        cur = block['header']['predecessor']


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


def activate_alpha(client, parameters=None):
    if parameters is None:
        parameters = constants.PARAMETERS
    proto = constants.ALPHA
    client.activate_protocol_json(proto, parameters)


def pprint(json_data: dict) -> None:
    print(json.dumps(json_data, indent=4, sort_keys=True))


def rpc(server: str, port: int, verb: str, path: str, data: dict = None,
        headers: dict = None):
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
