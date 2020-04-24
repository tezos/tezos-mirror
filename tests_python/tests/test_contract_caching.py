import os
import re
import pytest

from tools.constants import IDENTITIES
from tools.paths import CONTRACT_PATH

BAKE_ARGS = ['--minimal-timestamp']

# this contract transfers 1 tz to the contracts given as arguments
TRANSFER_ALL = \
    """
    parameter (list (contract unit));
    storage unit;
    code
      { UNPAIR;
        MAP {PUSH mutez 1000000; PUSH unit Unit; TRANSFER_TOKENS};
        PAIR }"""


def gas_limit(transfer_result):
    pattern = r"Gas limit: ?(\d*)"
    match = re.search(pattern, transfer_result.client_output)
    assert match is not None
    return match.groups()[0]


@pytest.mark.contract
class TestContractCaching:

    def test_setup(self, client, session, tmpdir):
        session['transfer_all'] = f'{tmpdir}/transfer_all.tz'
        with open(session['transfer_all'], 'w') as transfer_all_file:
            transfer_all_file.write(TRANSFER_ALL)

        pubkey = IDENTITIES['bootstrap1']['identity']
        session['no_op'] = os.path.join(CONTRACT_PATH, 'opcodes', 'noop.tz')
        args = ['--init', 'Unit', '--burn-cap', '10.0', '--force']
        session['addresses'] = {}
        for i in range(3):
            client.originate(f'no_op_{i}', 1, pubkey, session['no_op'], args)
            client.bake('bootstrap1', BAKE_ARGS)
            address = client.get_contract_address(f'no_op_{i}')
            session['addresses'][f'no_op_{i}'] = address
        client.originate('transfer_all',
                         1000, pubkey,
                         session['transfer_all'],
                         args)
        client.bake('bootstrap1', BAKE_ARGS)

    def transfer_all(self, client, destinations):
        # quote the addresses
        destinations = [f'"{destination}"' for destination in destinations]
        arg = f'{{ {"; ".join(destinations)} }}'
        args = ['-arg', arg, '--burn-cap', '10']
        result = client.transfer(len(destinations),
                                 "bootstrap1",
                                 "transfer_all",
                                 args)
        client.bake('bootstrap1', BAKE_ARGS)
        return result

    def test_repeat_contract_2_of_2(self, client, session):
        no_op_0 = session['addresses']['no_op_0']
        no_op_1 = session['addresses']['no_op_1']

        repeat_contract = \
            self.transfer_all(client, [no_op_0, no_op_0])

        different_contracts = \
            self.transfer_all(client, [no_op_0, no_op_1])

        assert gas_limit(repeat_contract) < gas_limit(different_contracts)

    def test_repeat_contract_2_of_3(self, client, session):
        no_op_0 = session['addresses']['no_op_0']
        no_op_1 = session['addresses']['no_op_1']
        no_op_2 = session['addresses']['no_op_2']

        repeat_contract = \
            self.transfer_all(client, [no_op_0, no_op_1, no_op_0])

        different_contracts = \
            self.transfer_all(client, [no_op_0, no_op_1, no_op_2])

        assert gas_limit(repeat_contract) < gas_limit(different_contracts)
