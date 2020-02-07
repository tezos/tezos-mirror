from os import path
import pytest
from tools import utils
from tools.paths import all_legacy_contracts, CONTRACT_PATH, \
    LEGACY_CONTRACT_PATH


@pytest.mark.contract
@pytest.mark.regression
class TestContractLegacy:
    """Tests for legacy opcodes."""

    @pytest.mark.parametrize("contract", all_legacy_contracts())
    def test_legacy_typecheck(self, client_regtest, contract):
        client = client_regtest

        with utils.assert_run_failure(r'Use of deprecated instruction'):
            client.typecheck(path.join(CONTRACT_PATH, contract))

    @pytest.mark.parametrize(
        "contract,param,storage,expected",
        [   # FORMAT: assert_output contract_file storage input expected_result

            ('create_contract.tz', 'Unit',
             'Left "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"', 'Unit'),
            ('create_contract_rootname.tz', 'Unit',
             'Left "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"', 'Unit'),
            ('create_account.tz', 'None',
             'Left "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"', 'None'),
            ('steps_to_quota.tz', '0', '100', '133008040'),
        ])
    def test_contract_input_output(self,
                                   client_regtest,
                                   contract,
                                   param,
                                   storage,
                                   expected):
        client = client_regtest
        assert contract.endswith('.tz'), \
            "test contract should have .tz extension"

        contract = path.join(LEGACY_CONTRACT_PATH, contract)
        run_script_res = client.run_script(contract, param,
                                           storage, None, True)
        assert run_script_res.storage == expected

    @pytest.mark.parametrize(
        "spendable, delegatable, expected_contract_type, assert_msg",
        [
            (True, True,
             "(or (lambda %do unit (list operation)) "
             "(string %default %default))",
             "Spendable, delegatable contracts should "
             "have the %do entrypoint"),
            (True, False,
             "(or (lambda %do unit (list operation)) "
             "(string %default %default))",
             "Spendable, non-delegatable contracts should have the "
             "%do entrypoint"),
            (False, True,
             "(or"
             " (or (key_hash %set_delegate) (unit %remove_delegate))"
             " (string %default %default))",
             "Non-spendable, delegatable contracts should have "
             "the %set/remove_delegate entrypoint"),
            (False, False, "%root (string %default)",
             "Non-Spendable and non-delegatable are unmodified")
        ]
    )
    def test_create_contract_flags(
            self, client_regtest, spendable,
            delegatable, expected_contract_type, assert_msg):
        contract = path.join(LEGACY_CONTRACT_PATH,
                             'create_contract_flags.tz')

        arg = ('(Pair "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"'
               f'(Pair {spendable} {delegatable}))')
        output = client_regtest.run_script(contract, 'Unit', arg)

        parameter = f"{expected_contract_type}"
        assert parameter in output.internal_operations, assert_msg
