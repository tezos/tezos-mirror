
from random import getrandbits

from os import path

import pytest

from Crypto.Hash import SHA3_256

from tools.paths import OPCODES_CONTRACT_PATH

RANDOM_ITERATIONS = 100


@pytest.mark.contract
class TestSha3:

    def test_sha3(self, client):

        contract = path.join(OPCODES_CONTRACT_PATH, 'sha3.tz')

        # Check that the contract output is correct
        for _ in range(RANDOM_ITERATIONS):
            rand_bytes = getrandbits(256).to_bytes(32, byteorder='little')
            sha3_hash = SHA3_256.new().update(rand_bytes).hexdigest()
            arg = f'0x{rand_bytes.hex()}'
            result = client.run_script(contract, 'None', arg)
            assert result.storage == f'(Some 0x{sha3_hash})'
