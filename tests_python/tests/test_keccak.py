
from random import getrandbits

from os import path

import pytest

from Crypto.Hash import keccak

from tools.paths import OPCODES_CONTRACT_PATH

RANDOM_ITERATIONS = 100


@pytest.mark.contract
class TestKeccak:

    def test_keccak(self, client):

        contract = path.join(OPCODES_CONTRACT_PATH, 'keccak.tz')

        for _ in range(RANDOM_ITERATIONS):
            rand_bytes = getrandbits(256).to_bytes(32, byteorder='little')
            keccak_hash = keccak.new(digest_bits=256).update(rand_bytes)
            arg = f'0x{rand_bytes.hex()}'
            result = client.run_script(contract, 'None', arg)
            assert result.storage == f'(Some 0x{keccak_hash.hexdigest()})'
