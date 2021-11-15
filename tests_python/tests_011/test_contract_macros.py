from os import path
import pytest
from tools.utils import (
    assert_run_script_failwith,
    assert_transfer_failwith,
    init_with_transfer,
    bake,
    assert_storage_contains,
)
from tools.client_regression import ClientRegression
from client.client import Client
from .contract_paths import MACROS_CONTRACT_PATH, CONTRACT_PATH, all_contracts


@pytest.mark.contract
class TestContractMacros:
    """Tests for contracts using macros that do not require origination."""

    @pytest.mark.parametrize(
        "contract,param,storage,expected",
        [  # FORMAT: assert_output contract_file storage input expected_result
            # Build list
            ('build_list.tz', '{}', '0', '{ 0 }'),
            ('build_list.tz', '{}', '3', '{ 0 ; 1 ; 2 ; 3 }'),
            (
                'build_list.tz',
                '{}',
                '10',
                '{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }',
            ),
            # Find maximum int in list -- returns None if not found
            ('max_in_list.tz', 'None', '{}', 'None'),
            ('max_in_list.tz', 'None', '{ 1 }', '(Some 1)'),
            ('max_in_list.tz', 'None', '{ -1 }', '(Some -1)'),
            (
                'max_in_list.tz',
                'None',
                '{ 10 ; -1 ; -20 ; 100 ; 0 }',
                '(Some 100)',
            ),
            (
                'max_in_list.tz',
                'None',
                '{ 10 ; -1 ; -20 ; 100 ; 0 }',
                '(Some 100)',
            ),
            (
                'max_in_list.tz',
                'None',
                '{ -10 ; -1 ; -20 ; -100 }',
                '(Some -1)',
            ),
            # Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
            (
                'compare.tz',
                '{}',
                '(Pair 1000000 2000000)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare.tz',
                '{}',
                '(Pair 2000000 1000000)',
                '{ False ; True ; False ; True ; False }',
            ),
            (
                'compare.tz',
                '{}',
                '(Pair 2370000 2370000)',
                '{ True ; False ; False ; True ; True }',
            ),
            # Test ASSERT
            ('assert.tz', 'Unit', 'True', 'Unit'),
            # ASSERT_{OP}
            ('assert_eq.tz', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_eq.tz', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_neq.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_lt.tz', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_le.tz', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_le.tz', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_gt.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_ge.tz', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_ge.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            # ASSERT_CMP{OP}
            ('assert_cmpeq.tz', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_cmpneq.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmplt.tz', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_cmple.tz', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_cmple.tz', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_cmpgt.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmpge.tz', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmpge.tz', 'Unit', '(Pair 0 0)', 'Unit'),
            # Tests the SET_CAR and SET_CDR instructions
            (
                'set_caddaadr.tz',
                '(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)',
                '3000000',
                '(Pair (Pair 1 2 (Pair (Pair 3 3000000) 4) 5) 6)',
            ),
            (
                'map_caddaadr.tz',
                '(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)',
                'Unit',
                '(Pair (Pair 1 2 (Pair (Pair 3 1000000) 4) 5) 6)',
            ),
            # Test comparisons on bytes { EQ ; GT ; LT ; GE ; LE }
            (
                'compare_bytes.tz',
                '{}',
                '(Pair 0x33 0x34)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare_bytes.tz',
                '{}',
                '(Pair 0x33 0x33aa)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare_bytes.tz',
                '{}',
                '(Pair 0x33 0x33)',
                '{ True ; False ; False ; True ; True }',
            ),
            (
                'compare_bytes.tz',
                '{}',
                '(Pair 0x34 0x33)',
                '{ False ; True ; False ; True ; False }',
            ),
        ],
    )
    def test_contract_input_output(
        self,
        client: Client,
        contract: str,
        param: str,
        storage: str,
        expected: str,
    ):
        assert contract.endswith(
            '.tz'
        ), "test contract should have .tz extension"
        contract = path.join(MACROS_CONTRACT_PATH, contract)
        run_script_res = client.run_script(contract, param, storage)
        assert run_script_res.storage == expected

    @pytest.mark.parametrize(
        "contract,param,storage",
        [  # FORMAT: assert_output contract_file storage input expected_result
            ('assert.tz', 'Unit', 'False'),
            ('assert_eq.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_eq.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_neq.tz', 'Unit', '(Pair -1 -1)'),
            ('assert_lt.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_lt.tz', 'Unit', '(Pair 0 0)'),
            ('assert_le.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_gt.tz', 'Unit', '(Pair -1 0)'),
            ('assert_gt.tz', 'Unit', '(Pair 0 0)'),
            ('assert_ge.tz', 'Unit', '(Pair -1 0)'),
            ('assert_cmpeq.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_cmpneq.tz', 'Unit', '(Pair -1 -1)'),
            ('assert_cmplt.tz', 'Unit', '(Pair 0 0)'),
            ('assert_cmplt.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_cmple.tz', 'Unit', '(Pair 0 -1)'),
            ('assert_cmpgt.tz', 'Unit', '(Pair 0 0)'),
            ('assert_cmpgt.tz', 'Unit', '(Pair -1 0)'),
            ('assert_cmpge.tz', 'Unit', '(Pair -1 0)'),
        ],
    )
    def test_contract_failures(self, client: Client, contract, param, storage):
        contract = path.join(MACROS_CONTRACT_PATH, contract)
        assert_run_script_failwith(client, contract, param, storage)


@pytest.mark.slow
@pytest.mark.contract
class TestGuestBook:
    """Test on the guestbook contract."""

    def test_guestbook(self, client: Client):
        contract = path.join(MACROS_CONTRACT_PATH, 'guestbook.tz')

        init_with_transfer(
            client,
            contract,
            '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }',
            100,
            'bootstrap1',
        )

        assert_transfer_failwith(
            client,
            0,
            'bootstrap2',
            'guestbook',
            ['--arg', '"Pas moi"', '--burn-cap', '10'],
        )

        client.transfer(
            0,
            'bootstrap1',
            'guestbook',
            ['-arg', '"Coucou"', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(
            client,
            'guestbook',
            '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }',
        )

        assert_transfer_failwith(
            client,
            0,
            'bootstrap3',
            'guestbook',
            ['--arg', '"Pas moi non plus"', '--burn-cap', '10'],
        )
        assert_transfer_failwith(
            client,
            0,
            'bootstrap1',
            'guestbook',
            ['--arg', '"Recoucou ?"', '--burn-cap', '10'],
        )


@pytest.mark.slow
@pytest.mark.contract
class TestBigmap:
    """Tests on the big_map_mem contract."""

    def test_bigmap(self, client: Client):
        contract = path.join(MACROS_CONTRACT_PATH, 'big_map_mem.tz')

        init_with_transfer(
            client,
            contract,
            '(Pair { Elt 1 Unit ; Elt 2 Unit ; Elt 3 Unit } Unit)',
            100,
            'bootstrap1',
        )

        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['-arg', '(Pair 0 False)', '--burn-cap', '10'],
        )
        bake(client)

        assert_transfer_failwith(
            client,
            0,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 0 True)', '--burn-cap', '10'],
        )

        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 0 False)', '--burn-cap', '10'],
        )
        bake(client)
        assert_transfer_failwith(
            client,
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 0 True)', '--burn-cap', '10'],
        )
        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 1 True)', '--burn-cap', '10'],
        )
        bake(client)
        assert_transfer_failwith(
            client,
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 1 False)', '--burn-cap', '10'],
        )
        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 2 True)', '--burn-cap', '10'],
        )
        bake(client)
        assert_transfer_failwith(
            client,
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 2 False)', '--burn-cap', '10'],
        )
        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 3 True)', '--burn-cap', '10'],
        )
        bake(client)
        assert_transfer_failwith(
            client,
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 3 False)', '--burn-cap', '10'],
        )
        client.transfer(
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 4 False)', '--burn-cap', '10'],
        )
        bake(client)
        assert_transfer_failwith(
            client,
            1,
            'bootstrap1',
            'big_map_mem',
            ['--arg', '(Pair 4 True)', '--burn-cap', '10'],
        )


@pytest.mark.slow
@pytest.mark.contract
class TestBigmapGetAdd:
    """Tests on the big_map_get_add contract."""

    def test_bigmap(self, client: Client):
        contract = path.join(MACROS_CONTRACT_PATH, 'big_map_get_add.tz')

        init_with_transfer(
            client,
            contract,
            '(Pair { Elt 0 1 ; Elt 1 2 ; Elt 2 3 } Unit)',
            100,
            'bootstrap1',
        )

        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 200 (Some 2)) (Pair 200 (Some 2)))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 200 None) (Pair 200 None))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 200 None) (Pair 300 None))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 1 None) (Pair 200 None))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 1 (Some 2)) (Pair 0 (Some 1)))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 400 (Some 1232)) (Pair 400 (Some 1232)))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)
        client.transfer(
            1,
            'bootstrap1',
            'big_map_get_add',
            [
                '--arg',
                '(Pair (Pair 401 (Some 0)) (Pair 400 (Some 1232)))',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)


@pytest.mark.regression
class TestMacroExpansion:
    """Test expanding macros"""

    @pytest.mark.parametrize("contract", all_contracts(['macros']))
    def test_macro_expansion(
        self, client_regtest: ClientRegression, contract: str
    ):
        """This test expands macros in all macro test contracts, with
        regression detection enabled. This test should fail if the definition
        of any macros change.
        """
        client_regtest.expand_macros(path.join(CONTRACT_PATH, contract))
