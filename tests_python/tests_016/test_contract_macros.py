import pytest
from tools.utils import (
    assert_run_script_failwith,
    assert_transfer_failwith,
    bake,
    assert_storage_contains,
)
from tools.client_regression import ClientRegression
from client.client import Client
from .contract_paths import (
    find_script,
    find_script_by_name,
    all_contracts,
    init_with_transfer,
)


@pytest.mark.contract
class TestContractMacros:
    """Tests for contracts using macros that do not require origination."""

    @pytest.mark.parametrize(
        "contract,param,storage,expected",
        [  # FORMAT: assert_output contract_file storage input expected_result
            # Build list
            ('build_list', '{}', '0', '{ 0 }'),
            ('build_list', '{}', '3', '{ 0 ; 1 ; 2 ; 3 }'),
            (
                'build_list',
                '{}',
                '10',
                '{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }',
            ),
            # Find maximum int in list -- returns None if not found
            ('max_in_list', 'None', '{}', 'None'),
            ('max_in_list', 'None', '{ 1 }', '(Some 1)'),
            ('max_in_list', 'None', '{ -1 }', '(Some -1)'),
            (
                'max_in_list',
                'None',
                '{ 10 ; -1 ; -20 ; 100 ; 0 }',
                '(Some 100)',
            ),
            (
                'max_in_list',
                'None',
                '{ 10 ; -1 ; -20 ; 100 ; 0 }',
                '(Some 100)',
            ),
            (
                'max_in_list',
                'None',
                '{ -10 ; -1 ; -20 ; -100 }',
                '(Some -1)',
            ),
            # Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
            (
                'compare',
                '{}',
                '(Pair 1000000 2000000)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare',
                '{}',
                '(Pair 2000000 1000000)',
                '{ False ; True ; False ; True ; False }',
            ),
            (
                'compare',
                '{}',
                '(Pair 2370000 2370000)',
                '{ True ; False ; False ; True ; True }',
            ),
            # Test ASSERT
            ('assert', 'Unit', 'True', 'Unit'),
            # ASSERT_{OP}
            ('assert_eq', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_eq', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_neq', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_lt', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_le', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_le', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_gt', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_ge', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_ge', 'Unit', '(Pair 0 -1)', 'Unit'),
            # ASSERT_CMP{OP}
            ('assert_cmpeq', 'Unit', '(Pair -1 -1)', 'Unit'),
            ('assert_cmpneq', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmplt', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_cmple', 'Unit', '(Pair -1 0)', 'Unit'),
            ('assert_cmple', 'Unit', '(Pair 0 0)', 'Unit'),
            ('assert_cmpgt', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmpge', 'Unit', '(Pair 0 -1)', 'Unit'),
            ('assert_cmpge', 'Unit', '(Pair 0 0)', 'Unit'),
            # Tests the SET_CAR and SET_CDR instructions
            (
                'set_caddaadr',
                '(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)',
                '3000000',
                '(Pair (Pair 1 2 (Pair (Pair 3 3000000) 4) 5) 6)',
            ),
            (
                'map_caddaadr',
                '(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)',
                'Unit',
                '(Pair (Pair 1 2 (Pair (Pair 3 1000000) 4) 5) 6)',
            ),
            # Test comparisons on bytes { EQ ; GT ; LT ; GE ; LE }
            (
                'compare_bytes',
                '{}',
                '(Pair 0x33 0x34)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare_bytes',
                '{}',
                '(Pair 0x33 0x33aa)',
                '{ False ; False ; True ; False ; True }',
            ),
            (
                'compare_bytes',
                '{}',
                '(Pair 0x33 0x33)',
                '{ True ; False ; False ; True ; True }',
            ),
            (
                'compare_bytes',
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
        contract = find_script(['macros', contract])
        run_script_res = client.run_script(contract, param, storage)
        assert run_script_res.storage == expected

    @pytest.mark.parametrize(
        "contract,param,storage",
        [  # FORMAT: assert_output contract_file storage input expected_result
            ('assert', 'Unit', 'False'),
            ('assert_eq', 'Unit', '(Pair 0 -1)'),
            ('assert_eq', 'Unit', '(Pair 0 -1)'),
            ('assert_neq', 'Unit', '(Pair -1 -1)'),
            ('assert_lt', 'Unit', '(Pair 0 -1)'),
            ('assert_lt', 'Unit', '(Pair 0 0)'),
            ('assert_le', 'Unit', '(Pair 0 -1)'),
            ('assert_gt', 'Unit', '(Pair -1 0)'),
            ('assert_gt', 'Unit', '(Pair 0 0)'),
            ('assert_ge', 'Unit', '(Pair -1 0)'),
            ('assert_cmpeq', 'Unit', '(Pair 0 -1)'),
            ('assert_cmpneq', 'Unit', '(Pair -1 -1)'),
            ('assert_cmplt', 'Unit', '(Pair 0 0)'),
            ('assert_cmplt', 'Unit', '(Pair 0 -1)'),
            ('assert_cmple', 'Unit', '(Pair 0 -1)'),
            ('assert_cmpgt', 'Unit', '(Pair 0 0)'),
            ('assert_cmpgt', 'Unit', '(Pair -1 0)'),
            ('assert_cmpge', 'Unit', '(Pair -1 0)'),
        ],
    )
    def test_contract_failures(self, client: Client, contract, param, storage):
        contract = find_script(['macros', contract])
        assert_run_script_failwith(client, contract, param, storage)


@pytest.mark.slow
@pytest.mark.contract
class TestGuestBook:
    """Test on the guestbook contract."""

    def test_guestbook(self, client: Client):
        contract = ['macros', 'guestbook']

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
        contract = ['macros', 'big_map_mem']

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
        contract = ['macros', 'big_map_get_add']

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
        client_regtest.expand_macros(find_script_by_name(contract))
