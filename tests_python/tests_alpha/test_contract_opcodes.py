from os import path

import pytest

from tools.client_regression import ClientRegression
from tools.constants import IDENTITIES
from tools.utils import (
    assert_run_failure,
    assert_run_script_failwith,
    assert_run_script_success,
)
from .contract_paths import MINI_SCENARIOS_CONTRACT_PATH, OPCODES_CONTRACT_PATH


PUBLIC_KEY = IDENTITIES['bootstrap1']['public']


@pytest.mark.slow
@pytest.mark.contract
@pytest.mark.regression
class TestContractOpcodes:
    """Tests for individual opcodes that do not require origination."""

    @pytest.mark.parametrize("balance", [0, 0.000001, 0.5, 1, 5, 1000, 8e12])
    def test_balance(self, client_regtest: ClientRegression, balance: float):
        client = client_regtest
        contract = 'balance.tz'
        contract = path.join(OPCODES_CONTRACT_PATH, contract)
        run_script_res = client.run_script(
            contract, '0', 'Unit', balance=balance, trace_stack=True
        )
        assert run_script_res.storage == str(int(1000000 * balance))

    def test_now(self, client_regtest: ClientRegression):
        """Test that the --now flag of 'octez-client run script' affects the
        value returned by the NOW instruction. See also
        test_contract_onchain_opcodes.py for a complementary test of the NOW
        instruction."""
        client = client_regtest
        contract = 'store_now.tz'
        initial_storage = '"2017-07-13T09:19:01Z"'
        now = '2021-10-13T10:16:52Z'
        contract = path.join(OPCODES_CONTRACT_PATH, contract)
        run_script_res = client.run_script(
            contract,
            storage=initial_storage,
            inp='Unit',
            now=now,
            trace_stack=True,
        )
        assert run_script_res.storage == f'"{now}"'

    def test_level(self, client_regtest: ClientRegression):
        """Test that the --level flag of 'octez-client run script' affects the
        value returned by the LEVEL instruction. See also
        test_contract_onchain_opcodes.py for a complementary test of the LEVEL
        instuction."""
        client = client_regtest
        contract = 'level.tz'
        initial_storage = '9999999'
        level = 10
        contract = path.join(OPCODES_CONTRACT_PATH, contract)
        run_script_res = client.run_script(
            contract,
            storage=initial_storage,
            inp='Unit',
            level=level,
            trace_stack=True,
        )
        assert run_script_res.storage == f'{level}'

    @pytest.mark.parametrize(
        "contract,param,storage,expected,big_map_diff",
        [  # FORMAT: assert_output contract_file storage input expected_result
            #         expected_diffs
            # Get the value stored at the given key in the big map
            (
                'get_big_map_value.tz',
                '(Pair { Elt "hello" "hi" } None)',
                '"hello"',
                '(Pair 4 (Some "hi"))',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["hello"] to "hi"'],
                ],
            ),
            (
                'get_big_map_value.tz',
                '(Pair { Elt "hello" "hi" } None)',
                '""',
                '(Pair 4 None)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["hello"] to "hi"'],
                ],
            ),
            (
                'get_big_map_value.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } None)',
                '"1"',
                '(Pair 4 (Some "one"))',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            # Test updating big maps
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{}',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{ Elt "1" (Some "two") }',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Set map(4)["1"] to "two"'],
                ],
            ),
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{ Elt "3" (Some "three") }',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Set map(4)["3"] to "three"'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{ Elt "3" None }',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Unset map(4)["3"]'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{ Elt "2" None }',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Unset map(4)["2"]'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            (
                'update_big_map.tz',
                '(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)',
                '{ Elt "1" (Some "two") }',
                '(Pair 4 Unit)',
                [
                    ["New map(4) of type (big_map string string)"],
                    ['Set map(4)["2"] to "two"'],
                    ['Set map(4)["1"] to "two"'],
                ],
            ),
            # test the GET_AND_UPDATE instruction on big maps
            # Get and update the value stored at the given key in the map
            (
                'get_and_update_big_map.tz',
                '(Pair None {})',
                '"hello"',
                '(Pair None 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Unset map(4)["hello"]'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair (Some 4) {})',
                '"hello"',
                '(Pair None 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Set map(4)["hello"] to 4'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair None { Elt "hello" 4 })',
                '"hello"',
                '(Pair (Some 4) 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Unset map(4)["hello"]'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair (Some 5) { Elt "hello" 4 })',
                '"hello"',
                '(Pair (Some 4) 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Set map(4)["hello"] to 5'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair (Some 5) { Elt "hello" 4 })',
                '"hi"',
                '(Pair None 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Set map(4)["hello"] to 4'],
                    ['Set map(4)["hi"] to 5'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair None { Elt "1" 1 ; \
            Elt "2" 2 })',
                '"1"',
                '(Pair (Some 1) 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Set map(4)["2"] to 2'],
                    ['Unset map(4)["1"]'],
                ],
            ),
            (
                'get_and_update_big_map.tz',
                '(Pair None { Elt "1" 1 ; \
            Elt "2" 2 })',
                '"1"',
                '(Pair (Some 1) 4)',
                [
                    ["New map(4) of type (big_map string nat)"],
                    ['Set map(4)["2"] to 2'],
                    ['Unset map(4)["1"]'],
                ],
            ),
        ],
    )
    def test__big_map_contract_io(
        self,
        client_regtest: ClientRegression,
        contract: str,
        param: str,
        storage: str,
        expected: str,
        big_map_diff: str,
    ):
        client = client_regtest
        contract = path.join(OPCODES_CONTRACT_PATH, contract)
        run_script_res = client.run_script(
            contract, param, storage, trace_stack=True
        )
        assert run_script_res.storage == expected
        assert run_script_res.big_map_diff == big_map_diff

    @pytest.mark.parametrize(
        "storage,param,expected,big_map_diff",
        [  # test swap
            (
                '(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))',
                '(Left Unit)',
                '(Left (Pair 4 5))',
                [
                    ['New map(5) of type (big_map string string)'],
                    ['Set map(5)["1"] to "one"'],
                    ['New map(4) of type (big_map string string)'],
                    ['Set map(4)["2"] to "two"'],
                ],
            ),
            # test reset with new map
            (
                '(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))',
                '(Right (Left (Left (Pair { Elt "3" "three" } '
                + '{ Elt "4" "four" }))))',
                '(Left (Pair 4 5))',
                [
                    ['New map(5) of type (big_map string string)'],
                    ['Set map(5)["4"] to "four"'],
                    ['New map(4) of type (big_map string string)'],
                    ['Set map(4)["3"] to "three"'],
                ],
            ),
            # test reset to unit
            (
                '(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))',
                '(Right (Left (Right Unit)))',
                '(Right Unit)',
                [],
            ),
            # test import to big_map
            (
                '(Right Unit)',
                '(Right (Right (Left (Pair { Pair "foo" "bar" } '
                + '{ Pair "gaz" "baz" }) )))',
                '(Left (Pair 4 5))',
                [
                    ['New map(5) of type (big_map string string)'],
                    ['Set map(5)["gaz"] to "baz"'],
                    ['New map(4) of type (big_map string string)'],
                    ['Set map(4)["foo"] to "bar"'],
                ],
            ),
            # test add to big_map
            (
                '(Left (Pair { Elt "1" "one" } { Elt "2" "two" }) )',
                '(Right (Right (Right (Left { Pair "3" "three" }))))',
                '(Left (Pair 4 5))',
                [
                    ['New map(5) of type (big_map string string)'],
                    ['Set map(5)["2"] to "two"'],
                    ['New map(4) of type (big_map string string)'],
                    ['Set map(4)["3"] to "three"'],
                    ['Set map(4)["1"] to "one"'],
                ],
            ),
            # test remove from big_map
            (
                '(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))',
                '(Right (Right (Right (Right { "1" }))))',
                '(Left (Pair 4 5))',
                [
                    ['New map(5) of type (big_map string string)'],
                    ['Set map(5)["2"] to "two"'],
                    ['New map(4) of type (big_map string string)'],
                    ['Unset map(4)["1"]'],
                ],
            ),
        ],
    )
    def test_big_map_magic(
        self,
        client_regtest: ClientRegression,
        storage: str,
        param: str,
        expected: str,
        big_map_diff: str,
    ):
        client = client_regtest
        contract = path.join(MINI_SCENARIOS_CONTRACT_PATH, 'big_map_magic.tz')
        run_script_res = client.run_script(
            contract, storage, param, trace_stack=True
        )
        assert run_script_res.storage == expected
        assert run_script_res.big_map_diff == big_map_diff

    def test_packunpack(self, client_regtest: ClientRegression):
        """Test PACK/UNPACK and binary format."""
        client = client_regtest
        assert_run_script_success(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'packunpack.tz'),
            'Unit',
            '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) '
            + '0x05070707070100000004746f746f020000000800030'
            + '007000900010200000006000100020003)',
        )
        assert_run_script_failwith(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'packunpack.tz'),
            'Unit',
            '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) '
            + '0x05070707070100000004746f746f020000000800030'
            + '0070009000102000000060001000200030004)',
        )

    def test_check_signature(self, client_regtest: ClientRegression):
        client = client_regtest
        sig = (
            'edsigu3QszDjUpeqYqbvhyRxMpVFamEnvm9FYnt7YiiNt'
            + '9nmjYfh8ZTbsybZ5WnBkhA7zfHsRVyuTnRsGLR6fNHt1Up1FxgyRtF'
        )
        assert_run_script_success(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'check_signature.tz'),
            f'(Pair "{sig}" "hello")',
            '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"',
        )
        assert_run_script_failwith(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'check_signature.tz'),
            f'(Pair "{sig}" "abcd")',
            '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"',
        )

    def test_hash_consistency_michelson_cli(
        self, client_regtest: ClientRegression
    ):
        client = client_regtest
        hash_result = client.hash(
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))',
            '(pair mutez (pair timestamp int))',
        ).blake2b
        hash_contract = path.join(
            OPCODES_CONTRACT_PATH, 'hash_consistency_checker.tz'
        )
        run_script_res = client.run_script(
            hash_contract,
            '0x00',
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))',
            trace_stack=True,
        )
        assert run_script_res.storage == hash_result
        run_script_res = client.run_script(
            hash_contract,
            '0x00',
            '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))',
            trace_stack=True,
        )
        assert run_script_res.storage == hash_result

    @pytest.mark.parametrize(
        "contract,param,storage",
        [  # FORMAT: assert_output contract_file storage input
            # Test overflow in shift
            ('shifts.tz', 'None', '(Left (Pair 1 257))'),
            ('shifts.tz', 'None', '(Left (Pair 123 257))'),
            ('shifts.tz', 'None', '(Right (Pair 1 257))'),
            ('shifts.tz', 'None', '(Right (Pair 123 257))'),
            ('mul_overflow.tz', 'Unit', 'Left Unit'),
            ('mul_overflow.tz', 'Unit', 'Right Unit'),
        ],
    )
    def test_arithmetic_overflow(
        self,
        client_regtest_scrubbed: ClientRegression,
        contract: str,
        param: str,
        storage: str,
    ):
        client = client_regtest_scrubbed
        contract = path.join(OPCODES_CONTRACT_PATH, contract)

        with assert_run_failure(r'unexpected arithmetic overflow'):
            client.run_script(contract, param, storage, trace_stack=True)

    @pytest.mark.skip(reason="Bug in annotation system")
    def test_fails_annotated_set_car_cdr(
        self, client_regtest: ClientRegression
    ):
        """Tests the SET_CAR and SET_CDR instructions."""
        client = client_regtest

        with assert_run_failure(r'The two annotations do not match'):
            client.run_script(
                path.join(OPCODES_CONTRACT_PATH, 'set_car.tz'),
                '(Pair %wrong %field "hello" 0)',
                '""',
                trace_stack=True,
            )

    @pytest.mark.parametrize(
        "contract,storage,param,expected",
        [  # FORMAT: assert_output contract_file storage input expected_result
            # Mapping over maps
            ('map_map_sideeffect.tz', '(Pair {} 0)', '10', '(Pair {} 0)'),
            (
                'map_map_sideeffect.tz',
                '(Pair { Elt "foo" 1 } 1)',
                '10',
                '(Pair { Elt "foo" 11 } 11)',
            ),
            (
                'map_map_sideeffect.tz',
                '(Pair { Elt "bar" 5 ; Elt "foo" 1 } 6)',
                '15',
                '(Pair { Elt "bar" 20 ; Elt "foo" 16 } 36)',
            ),
        ],
    )
    def test_map_map_sideeffect(
        self,
        client_regtest: ClientRegression,
        contract: str,
        param: str,
        storage: str,
        expected: str,
    ):
        client = client_regtest
        contract = path.join(OPCODES_CONTRACT_PATH, contract)
        run_script_res = client.run_script(
            contract, storage, param, trace_stack=True
        )
        assert run_script_res.storage == expected
