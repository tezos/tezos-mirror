from random import getrandbits

from os import path

import pytest

from Crypto.Hash import keccak, SHA3_256

from .contract_paths import OPCODES_CONTRACT_PATH

RANDOM_ITERATIONS = 50

HASH_FUNCTIONS = {
    "keccak": lambda bytes_to_hash: keccak.new(digest_bits=256)
    .update(bytes_to_hash)
    .hexdigest(),
    "sha3": lambda bytes_to_hash: SHA3_256.new()
    .update(bytes_to_hash)
    .hexdigest(),
}


@pytest.fixture(params=HASH_FUNCTIONS.keys())
def hash_fun(request):
    return request.param


@pytest.mark.contract
class TestHash:
    @pytest.mark.parametrize(
        "bytes_to_hash",
        [b"", b"a"]
        + [
            getrandbits(bytes_length * 8).to_bytes(
                bytes_length, byteorder="little"
            )
            for bytes_length in [32]
            for _ in range(RANDOM_ITERATIONS)
        ],
    )
    def test_hash(self, client, hash_fun, bytes_to_hash):
        contract = path.join(OPCODES_CONTRACT_PATH, f"{hash_fun}.tz")

        hashed = HASH_FUNCTIONS[hash_fun](bytes_to_hash)
        arg = f"0x{bytes_to_hash.hex()}"
        result = client.run_script(contract, "None", arg)
        assert result.storage == f"(Some 0x{hashed})"
