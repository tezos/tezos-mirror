import os
import pytest

from client.client import Client
from tools import utils
from tools.constants import IDENTITIES
from tools.utils import originate
from .contract_paths import (
    CONTRACT_PATH,
    ILLTYPED_CONTRACT_PATH,
    all_legacy_contracts,
)


ID_SCRIPT_LITERAL = '''
parameter unit; storage unit; code {CAR; NIL operation; PAIR}
'''.strip()
ID_SCRIPT_HASH = '''
exprtpyospPfMqcARmu5FGukprC7kbbe4jb4zxFd4Gxrp2vcCPjRNa
'''.strip()


@pytest.mark.contract
@pytest.mark.incremental
class TestManager:
    def test_manager_origination(self, client: Client, session: dict):
        path = os.path.join(CONTRACT_PATH, 'entrypoints', 'manager.tz')
        pubkey = IDENTITIES['bootstrap2']['identity']
        originate(client, session, path, f'"{pubkey}"', 1000)
        originate(
            client, session, path, f'"{pubkey}"', 1000, contract_name="manager2"
        )

    def test_delegatable_origination(self, client: Client, session: dict):
        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'delegatable_target.tz'
        )
        pubkey = IDENTITIES['bootstrap2']['identity']
        originate(
            client, session, path, f'Pair "{pubkey}" (Pair "hello" 45)', 1000
        )

    def test_target_with_entrypoints_origination(self, client: Client, session):
        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'big_map_entrypoints.tz'
        )
        originate(
            client, session, path, 'Pair {} {}', 1000, contract_name='target'
        )

    def test_target_without_entrypoints_origination(
        self, client: Client, session
    ):
        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'no_entrypoint_target.tz'
        )
        originate(
            client,
            session,
            path,
            'Pair "hello" 42',
            1000,
            contract_name='target_no_entrypoints',
        )

    def test_target_without_default_origination(self, client: Client, session):
        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'no_default_target.tz'
        )
        originate(
            client,
            session,
            path,
            'Pair "hello" 42',
            1000,
            contract_name='target_no_default',
        )

    def test_target_with_root_origination(self, client: Client, session):
        path = os.path.join(CONTRACT_PATH, 'entrypoints', 'rooted_target.tz')
        originate(
            client,
            session,
            path,
            'Pair "hello" 42',
            1000,
            contract_name='rooted_target',
        )

    def test_manager_set_delegate(self, client: Client):
        client.set_delegate('manager', 'bootstrap2', [])
        utils.bake(client, bake_for='bootstrap5')
        bootstrap2_pkh = IDENTITIES['bootstrap2']['identity']
        client.set_delegate('delegatable_target', bootstrap2_pkh, [])
        utils.bake(client, bake_for='bootstrap5')
        delegate = IDENTITIES['bootstrap2']['identity']
        assert client.get_delegate('manager', []).delegate == delegate
        assert (
            client.get_delegate('delegatable_target', []).delegate == delegate
        )
        client.set_delegate('manager', 'bootstrap3', [])
        utils.bake(client, bake_for='bootstrap5')
        client.set_delegate('delegatable_target', 'bootstrap3', [])
        utils.bake(client, bake_for='bootstrap5')
        delegate = IDENTITIES['bootstrap3']['identity']
        assert client.get_delegate('manager', []).delegate == delegate
        assert (
            client.get_delegate('delegatable_target', []).delegate == delegate
        )

    def test_manager_withdraw_delegate(self, client: Client):
        client.withdraw_delegate('manager', [])
        utils.bake(client, bake_for='bootstrap5')
        client.withdraw_delegate('delegatable_target', [])
        utils.bake(client, bake_for='bootstrap5')
        assert client.get_delegate('manager', []).delegate is None
        assert client.get_delegate('delegatable_target', []).delegate is None

    def test_transfer_to_manager(self, client: Client):
        balance = client.get_mutez_balance('manager')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10.001
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(
            amount,
            'bootstrap2',
            'manager',
            ['--gas-limit', f'{128 * 15450 + 108}'],
        )
        utils.bake(client, bake_for='bootstrap5')
        new_balance = client.get_mutez_balance('manager')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.000382
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance + amount_mutez == new_balance
        assert (
            balance_bootstrap - fee_mutez - amount_mutez
            == new_balance_bootstrap
        )

    def test_simple_transfer_from_manager_to_implicit(self, client: Client):
        balance = client.get_mutez_balance('manager')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10.1
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(
            amount,
            'manager',
            'bootstrap2',
            ['--gas-limit', f'{128 * 26350 + 12}'],
        )
        utils.bake(client, bake_for='bootstrap5')
        new_balance = client.get_mutez_balance('manager')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.000542
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance - amount_mutez == new_balance
        assert (
            balance_bootstrap + amount_mutez - fee_mutez
            == new_balance_bootstrap
        )

    def test_transfer_from_manager_to_manager(self, client: Client):
        balance = client.get_mutez_balance('manager')
        balance_dest = client.get_mutez_balance('manager2')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(
            amount,
            'manager',
            'manager2',
            ['--gas-limit', f'{128 * 44950 + 112}'],
        )
        utils.bake(client, bake_for='bootstrap5')
        new_balance = client.get_mutez_balance('manager')
        new_balance_dest = client.get_mutez_balance('manager2')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.000731
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance - amount_mutez == new_balance
        assert balance_dest + amount_mutez == new_balance_dest
        assert balance_bootstrap - fee_mutez == new_balance_bootstrap

    def test_transfer_from_manager_to_default(self, client: Client):
        client.transfer(
            10, 'manager', 'bootstrap2', ['--entrypoint', 'default']
        )
        utils.bake(client, bake_for='bootstrap5')
        client.transfer(10, 'manager', 'manager', ['--entrypoint', 'default'])
        utils.bake(client, bake_for='bootstrap5')

    def test_transfer_from_manager_to_target(self, client: Client):
        client.transfer(10, 'manager', 'target', ['--burn-cap', '0.356'])
        utils.bake(client, bake_for='bootstrap5')

    def test_transfer_from_manager_to_entrypoint_with_args(
        self, client: Client
    ):
        arg = 'Pair "hello" 42'
        # using 'transfer'
        client.transfer(
            0,
            'manager',
            'target',
            ['--entrypoint', 'add_left', '--arg', arg, '--burn-cap', '0.067'],
        )
        utils.bake(client, bake_for='bootstrap5')
        client.transfer(
            0,
            'manager',
            'target',
            ['--entrypoint', 'mem_left', '--arg', '"hello"'],
        )
        utils.bake(client, bake_for='bootstrap5')

        # using 'call'
        client.call(
            'manager',
            'target',
            ['--entrypoint', 'add_left', '--arg', arg, '--burn-cap', '0.067'],
        )
        utils.bake(client, bake_for='bootstrap5')
        client.call(
            'manager',
            'target',
            ['--entrypoint', 'mem_left', '--arg', '"hello"'],
        )
        utils.bake(client, bake_for='bootstrap5')

    def test_transfer_from_manager_no_entrypoint_with_args(
        self, client: Client
    ):
        arg = 'Left Unit'
        client.transfer(0, 'manager', 'target_no_entrypoints', ['--arg', arg])
        utils.bake(client, bake_for='bootstrap5')

        client.call('manager', 'target_no_entrypoints', ['--arg', arg])
        utils.bake(client, bake_for='bootstrap5')

    def test_transfer_from_manager_to_no_default_with_args(
        self, client: Client
    ):
        arg = 'Left Unit'
        client.transfer(0, 'manager', 'target_no_default', ['--arg', arg])
        utils.bake(client, bake_for='bootstrap5')

        client.call('manager', 'target_no_default', ['--arg', arg])
        utils.bake(client, bake_for='bootstrap5')

    def test_transfer_from_manager_to_rooted_target_with_args(
        self, client: Client
    ):
        arg = 'Left Unit'
        client.transfer(
            0,
            'manager',
            'rooted_target',
            ['--arg', arg, '--entrypoint', 'root'],
        )
        utils.bake(client, bake_for='bootstrap5')

        client.call(
            'manager', 'rooted_target', ['--arg', arg, '--entrypoint', 'root']
        )
        utils.bake(client, bake_for='bootstrap5')


@pytest.mark.slow
@pytest.mark.contract
class TestContracts:
    """Test type checking errors"""

    @pytest.mark.parametrize("contract", all_legacy_contracts())
    def test_deprecated_typecheck_breaks(self, client, contract):
        if contract in [
            "legacy/create_contract.tz",
            "legacy/create_contract_flags.tz",
            "legacy/create_contract_rootname.tz",
        ]:
            with utils.assert_run_failure(r'ill-typed script'):
                client.typecheck(os.path.join(CONTRACT_PATH, contract))
        else:
            with utils.assert_run_failure(r'Use of deprecated instruction'):
                client.typecheck(os.path.join(CONTRACT_PATH, contract))

    @pytest.mark.parametrize("contract", all_legacy_contracts())
    def test_deprecated_typecheck_in_legacy(self, client, contract):
        if contract in [
            "legacy/create_contract.tz",
            "legacy/create_contract_flags.tz",
            "legacy/create_contract_rootname.tz",
        ]:
            with utils.assert_run_failure(r'ill-typed script'):
                client.typecheck(
                    os.path.join(CONTRACT_PATH, contract), legacy=True
                )
        else:
            with utils.assert_run_failure(r'Use of deprecated instruction'):
                client.typecheck(
                    os.path.join(CONTRACT_PATH, contract), legacy=True
                )

    @pytest.mark.parametrize(
        "contract,error_pattern",
        [
            # Even though the interpreter uses a nonempty stack internally,
            # the typechecker should not be able to observe it.
            (
                "stack_bottom_unfailwithable.tz",
                r'wrong stack type for instruction FAILWITH',
            ),
            (
                "stack_bottom_unrightable.tz",
                r'wrong stack type for instruction RIGHT',
            ),
            (
                "stack_bottom_unleftable.tz",
                r'wrong stack type for instruction LEFT',
            ),
            (
                "stack_bottom_ungetable.tz",
                r'wrong stack type for instruction GET',
            ),
            (
                "stack_bottom_unpairable.tz",
                r'wrong stack type for instruction UNPAIR',
            ),
            (
                "stack_bottom_undug2able.tz",
                r'wrong stack type for instruction DUG',
            ),
            (
                "stack_bottom_undugable.tz",
                r'wrong stack type for instruction DUG',
            ),
            (
                "stack_bottom_undig2able.tz",
                r'wrong stack type for instruction DIG',
            ),
            (
                "stack_bottom_undigable.tz",
                r'wrong stack type for instruction DIG',
            ),
            (
                "stack_bottom_undip2able.tz",
                r'wrong stack type for instruction DUP',
            ),
            (
                "stack_bottom_undipable.tz",
                r'wrong stack type for instruction DUP',
            ),
            (
                "stack_bottom_undup2able.tz",
                r'wrong stack type for instruction DUP',
            ),
            (
                "stack_bottom_undropable.tz",
                r'wrong stack type for instruction DROP',
            ),
            (
                "stack_bottom_unpopable.tz",
                r'wrong stack type for instruction DUP',
            ),
            (
                "stack_bottom_unpopable_in_lambda.tz",
                r'wrong stack type for instruction DUP',
            ),
            # operations cannot be PACKed
            (
                "pack_operation.tz",
                r'operation type forbidden in parameter, storage and constants',
            ),
            # big_maps cannot be PACKed
            (
                "pack_big_map.tz",
                r'big_map or sapling_state type not expected here',
            ),
            (
                "invalid_self_entrypoint.tz",
                r'Contract has no entrypoint named D',
            ),
            (
                "contract_annotation_default.tz",
                r'unexpected_default_entrypoint',
            ),
            # Missing field
            (
                "missing_only_storage_field.tz",
                r'Missing contract field: storage',
            ),
            ("missing_only_code_field.tz", r'Missing contract field: code'),
            (
                "missing_only_parameter_field.tz",
                r'Missing contract field: parameter',
            ),
            (
                "missing_parameter_and_storage_fields.tz",
                r'Missing contract field: parameter',
            ),
            # Duplicated field
            (
                "multiple_parameter_field.tz",
                r'duplicate contract field: parameter',
            ),
            ("multiple_code_field.tz", r'duplicate contract field: code'),
            ("multiple_storage_field.tz", r'duplicate contract field: storage'),
            # The first duplicated field is reported, storage in this case
            (
                "multiple_storage_and_code_fields.tz",
                r'duplicate contract field: storage',
            ),
            # error message for set update on non-comparable type
            (
                "set_update_non_comparable.tz",
                r'Type nat\s+is not compatible with type list operation',
            ),
            # error message for the arity of the chain_id type
            (
                "chain_id_arity.tz",
                r'primitive chain_id expects 0 arguments but is given 1',
            ),
            # error message for DIP over the limit
            ("big_dip.tz", r'expected a positive 10-bit integer'),
            # error message for DROP over the limit
            ("big_drop.tz", r'expected a positive 10-bit integer'),
            # error message for attempting to push a value of type never
            ("never_literal.tz", r'type never has no inhabitant.'),
            # COMB, UNCOMB, and DUP cannot take 0 as argument
            ("comb0.tz", r"PAIR expects an argument of at least 2"),
            ("comb1.tz", r"PAIR expects an argument of at least 2"),
            ("uncomb0.tz", r"UNPAIR expects an argument of at least 2"),
            ("uncomb1.tz", r"UNPAIR expects an argument of at least 2"),
            ("dup0.tz", r"DUP n expects an argument of at least 1"),
            (
                "push_big_map_with_id_with_parens.tz",
                r"big_map or sapling_state type not expected here",
            ),
            (
                "push_big_map_with_id_without_parens.tz",
                r"primitive PUSH expects 2 arguments but is given 4",
            ),
            # sapling_state is not packable
            (
                "pack_sapling_state.tz",
                r"big_map or sapling_state type not expected here",
            ),
            # sapling_state is not packable
            (
                "unpack_sapling_state.tz",
                r"big_map or sapling_state type not expected here",
            ),
            # Ticket duplication attempt
            (
                "ticket_dup.tz",
                r'ticket nat cannot be used here because it is not duplicable',
            ),
            # error message for ticket unpack
            ("ticket_unpack.tz", r'Ticket in unauthorized position'),
            # error message for attempting to use APPLY to capture a ticket
            ("ticket_apply.tz", r'Ticket in unauthorized position'),
            # error message for attempting to wrap a ticket in a ticket
            (
                "ticket_in_ticket.tz",
                r'comparable type expected.Type ticket unit is not comparable',
            ),
            # error message for DIP { FAILWITH }
            (
                "dip_failwith.tz",
                r'The FAIL instruction must appear in a tail position.',
            ),
            # error message for MAP { FAILWITH }
            (
                "map_failwith.tz",
                r'The proper type of the return list cannot be inferred.',
            ),
        ],
    )
    def test_ill_typecheck(self, client: Client, contract, error_pattern):
        with utils.assert_run_failure(error_pattern):
            client.typecheck(os.path.join(ILLTYPED_CONTRACT_PATH, contract))

    def test_zero_transfer_to_implicit_contract(self, client):
        pubkey = IDENTITIES['bootstrap3']['identity']
        err = (
            'Transactions of 0ꜩ towards a contract without code are '
            rf'forbidden \({pubkey}\).'
        )
        with utils.assert_run_failure(err):
            client.transfer(0, 'bootstrap2', 'bootstrap3', [])

    def test_zero_transfer_to_nonexistent_contract(self, client):
        nonexistent = "KT1Fcq4inD44aMhmUiTEHR1QMQwJT7p2u641"
        err = rf'Contract {nonexistent} does not exist'
        with utils.assert_run_failure(err):
            client.transfer(0, 'bootstrap2', nonexistent, [])


@pytest.mark.incremental
@pytest.mark.contract
class TestView:
    def test_deploy_view_lib(self, client, session):
        path = f'{CONTRACT_PATH}/opcodes/view_toplevel_lib.tz'
        originate(client, session, path, '3', 999)
        session['lib'] = session['contract']
        client.bake('bootstrap3', ["--minimal-timestamp"])

    @pytest.mark.parametrize(
        "contract,init_storage,expected",
        [
            ('view_op_id', '(Pair 0 0)', 'Pair 10 3'),
            ('view_op_add', '42', '13'),
            ('view_fib', '0', '55'),
            ('view_mutual_recursion', '0', '20'),
            ('view_op_nonexistent_func', 'True', 'False'),
            ('view_op_nonexistent_addr', 'True', 'False'),
            ('view_op_toplevel_inconsistent_input_type', '5', '0'),
            ('view_op_toplevel_inconsistent_output_type', 'True', 'False'),
        ],
    )
    def test_runtime(self, client, session, contract, init_storage, expected):
        path = f'{CONTRACT_PATH}/opcodes/' + contract + '.tz'
        originate(client, session, path, init_storage, 0)
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                "--arg",
                "(Pair 10 \"" + session['lib'] + "\")",
                '--gas-limit',
                '1000000',
                "--burn-cap",
                "0.1",
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        assert client.get_storage(contract) == expected

    def test_create_contract(
        self,
        client,
        session,
    ):
        contract = 'create_contract_with_view'
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        originate(client, session, path, 'None', 0)
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                "--arg",
                "Unit",
                "--burn-cap",
                "0.1",
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])

        addr = client.get_storage(contract).split()[1]
        contract = 'view_op_constant'
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        originate(client, session, path, '2', 0)
        expected = "10"

        client.transfer(
            0,
            "bootstrap1",
            contract,
            [
                "--arg",
                f"(Pair {expected} {addr})",
                "--burn-cap",
                "0.1",
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])

        assert client.get_storage(contract) == expected

    def test_step_constants(self, client, session):
        contract = 'view_op_test_step_contants'
        path = f'{CONTRACT_PATH}/opcodes/' + contract + '.tz'
        originate(client, session, path, 'None', 0)
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                "--arg",
                "\"" + session['lib'] + "\"",
                '--gas-limit',
                '5000',
                "--burn-cap",
                "0.1",
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])

        source = IDENTITIES['bootstrap1']['identity']
        self_address = session['lib']
        sender = session['contract']
        expected = (
            'Some (Pair (Pair 0 999000000)\n'
            + '           (Pair "'
            + self_address
            + '" "'
            + sender
            + '")\n'
            + '           "'
            + source
            + '")'
        )

        assert client.get_storage(contract) == expected

    @pytest.mark.parametrize(
        "contract",
        [
            'self_after_view',
            'self_after_fib_view',
            'self_after_nonexistent_view',
        ],
    )
    def test_self(self, client, session, contract):
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        lib_address = session['lib']
        originate(client, session, path, f'"{lib_address}"', 1000)
        client.bake('bootstrap2', ["--minimal-timestamp"])
        self_address = session['contract']
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                '--arg',
                f'"{lib_address}"',
                '--burn-cap',
                '0.1',
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        assert client.get_storage(contract) == f'"{self_address}"'

    @pytest.mark.parametrize(
        "contract",
        [
            'self_address_after_view',
            'self_address_after_fib_view',
            'self_address_after_nonexistent_view',
        ],
    )
    def test_self_address(self, client, session, contract):
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        lib_address = session['lib']
        originate(client, session, path, f'"{lib_address}"', 1000)
        client.bake('bootstrap2', ["--minimal-timestamp"])
        self_address = session['contract']
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                '--arg',
                f'"{lib_address}"',
                '--burn-cap',
                '0.1',
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        assert client.get_storage(contract) == f'"{self_address}"'

    @pytest.mark.parametrize(
        "contract",
        [
            'sender_after_view',
            'sender_after_fib_view',
            'sender_after_nonexistent_view',
        ],
    )
    def test_sender(self, client, session, contract):
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        lib_address = session['lib']
        originate(client, session, path, f'"{lib_address}"', 1000)
        client.bake('bootstrap2', ["--minimal-timestamp"])
        sender = IDENTITIES['bootstrap1']['identity']
        client.transfer(
            0,
            'bootstrap1',
            contract,
            [
                '--arg',
                f'"{lib_address}"',
                '--burn-cap',
                '0.1',
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        assert client.get_storage(contract) == f'"{sender}"'

    @pytest.mark.parametrize(
        "contract",
        [
            'balance_after_view',
            'balance_after_fib_view',
            'balance_after_nonexistent_view',
        ],
    )
    def test_balance_after_view(self, client, session, contract):
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        lib_address = session['lib']
        initial_balance = 1000
        originate(client, session, path, '0', initial_balance)
        client.bake('bootstrap2', ["--minimal-timestamp"])
        amount = 10
        client.transfer(
            amount,
            'bootstrap1',
            contract,
            [
                '--arg',
                f'"{lib_address}"',
                '--burn-cap',
                '0.1',
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        expected_balance = initial_balance + amount
        assert (
            client.get_storage(contract)
            == f'{utils.mutez_of_tez(expected_balance)}'
        )

    @pytest.mark.parametrize(
        "contract",
        [
            'amount_after_view',
            'amount_after_fib_view',
            'amount_after_nonexistent_view',
        ],
    )
    def test_amount_after_view(self, client, session, contract):
        path = f'{CONTRACT_PATH}/opcodes/{contract}.tz'
        lib_address = session['lib']
        originate(client, session, path, '0', 1000)
        client.bake('bootstrap2', ["--minimal-timestamp"])
        amount = 3
        client.transfer(
            amount,
            'bootstrap1',
            contract,
            [
                '--arg',
                f'"{lib_address}"',
                '--burn-cap',
                '0.1',
            ],
        )
        client.bake('bootstrap2', ["--minimal-timestamp"])
        assert client.get_storage(contract) == f'{utils.mutez_of_tez(amount)}'

    def test_recursion(self, client, session):
        contract = 'view_rec'
        path = f'{CONTRACT_PATH}/opcodes/' + contract + '.tz'
        originate(client, session, path, 'Unit', 0)
        with utils.assert_run_failure(
            "Gas limit exceeded during typechecking or execution."
        ):
            client.transfer(
                0,
                'bootstrap1',
                contract,
                [
                    "--arg",
                    "Unit",
                    '--gas-limit',
                    '5000',
                ],
            )
        client.bake('bootstrap2', ["--minimal-timestamp"])

    @pytest.mark.parametrize(
        "contract,expected_error",
        [
            (
                'view_toplevel_bad_type',
                'the return of a view block did not match the expected type.',
            ),
            (
                'view_toplevel_bad_return_type',
                'the return of a view block did not match the expected type.',
            ),
            (
                'view_toplevel_bad_input_type',
                'operator ADD is undefined between string',
            ),
            (
                'view_toplevel_invalid_arity',
                'primitive view expects 4 arguments',
            ),
            (
                'view_toplevel_bad_name_too_long',
                'exceeds the maximum length of 31 characters',
            ),
            (
                'view_toplevel_bad_name_invalid_type',
                'only a string can be used here',
            ),
            (
                'view_toplevel_bad_name_non_printable_char',
                'string \\[a-zA-Z0-9_.%@\\]',
            ),
            (
                'view_toplevel_bad_name_invalid_char_set',
                'string \\[a-zA-Z0-9_.%@\\]',
            ),
            (
                'view_toplevel_duplicated_name',
                'the name of view in toplevel should be unique',
            ),
            (
                'view_toplevel_dupable_type_output',
                'Ticket in unauthorized position',
            ),
            (
                'view_toplevel_dupable_type_input',
                'Ticket in unauthorized position',
            ),
            (
                'view_toplevel_lazy_storage_input',
                'big_map or sapling_state type not expected here',
            ),
            (
                'view_toplevel_lazy_storage_output',
                'big_map or sapling_state type not expected here',
            ),
            ('view_op_invalid_arity', 'primitive VIEW expects 2 arguments'),
            (
                'view_op_bad_name_invalid_type',
                'unexpected int, only a string',
            ),
            (
                'view_op_bad_name_too_long',
                'exceeds the maximum length of 31 characters',
            ),
            (
                'view_op_bad_name_non_printable_char',
                'string \\[a-zA-Z0-9_.%@\\]',
            ),
            (
                'view_op_bad_name_invalid_char_set',
                'string \\[a-zA-Z0-9_.%@\\]',
            ),
            (
                'view_op_bad_return_type',
                'two branches don\'t end with the same stack type',
            ),
            (
                'view_op_dupable_type',
                'Ticket in unauthorized position',
            ),
            (
                'view_op_lazy_storage',
                'big_map or sapling_state type not expected here',
            ),
        ],
    )
    def test_typechecking_error(
        self, client, session, contract, expected_error
    ):
        path = f'{CONTRACT_PATH}/ill_typed/' + contract + '.tz'
        with utils.assert_run_failure(expected_error):
            originate(client, session, path, '4', 0)


@pytest.mark.incremental
@pytest.mark.contract
class TestChainId:
    def test_chain_id_opcode(self, client: Client, session: dict):
        path = os.path.join(CONTRACT_PATH, 'opcodes', 'chain_id.tz')
        originate(client, session, path, 'Unit', 0)
        client.call('bootstrap2', "chain_id", [])
        utils.bake(client, bake_for='bootstrap5')

    def test_chain_id_authentication_origination(self, client: Client, session):
        path = os.path.join(
            CONTRACT_PATH, 'mini_scenarios', 'authentication.tz'
        )
        pubkey = IDENTITIES['bootstrap1']['public']
        originate(client, session, path, f'Pair 0 "{pubkey}"', 1000)
        utils.bake(client, bake_for='bootstrap5')

    def test_chain_id_authentication_first_run(
        self, client: Client, session: dict
    ):
        destination = IDENTITIES['bootstrap2']['identity']
        operation = (
            '{DROP; NIL operation; '
            + f'PUSH address "{destination}"; '
            + 'CONTRACT unit; ASSERT_SOME; PUSH mutez 1000; UNIT; '
            + 'TRANSFER_TOKENS; CONS}'
        )
        chain_id = client.rpc('get', 'chains/main/chain_id')
        contract_address = session['contract']
        packed = client.pack(
            f'Pair (Pair "{chain_id}" "{contract_address}") '
            + f'(Pair {operation} 0)',
            'pair (pair chain_id address)'
            + '(pair (lambda unit (list operation)) nat)',
        )
        signature = client.sign_bytes_of_string(packed, "bootstrap1")
        client.call(
            'bootstrap2',
            'authentication',
            ['--arg', f'Pair {operation} \"{signature}\"'],
        )
        utils.bake(client, bake_for='bootstrap5')


@pytest.mark.contract
class TestScriptHashMultiple:
    """Test octez-client hash script with diffent number and type of
    arguments"""

    def test_contract_hashes_empty(self, client: Client):
        assert client.hash_script([]) == []

    def test_contract_hashes_single(self, client: Client):
        assert client.hash_script([ID_SCRIPT_LITERAL]) == [
            (ID_SCRIPT_HASH, None)
        ]

    def test_contract_hashes_single_display_names(self, client: Client):
        assert client.hash_script([ID_SCRIPT_LITERAL], display_names=True,) == [
            (
                ID_SCRIPT_HASH,
                'Literal script 1',
            )
        ]

    def test_contract_hashes_mixed(self, client: Client):
        contract_path = os.path.join(CONTRACT_PATH, 'attic', 'empty.tz')
        script_empty_hash = '''
expruat2BS4KCwn9kbopeX1ZwxtrtJbyFhpnpnG6A5KdCBCwHNsdod
        '''.strip()
        with open(contract_path, 'r') as contract_file:
            script = contract_file.read()

            hashes = client.hash_script([contract_path, script])

            assert hashes == [
                (
                    script_empty_hash,
                    None,
                ),
                (
                    script_empty_hash,
                    None,
                ),
            ]

            hashes = client.hash_script(
                [contract_path, script], display_names=True
            )

            assert hashes == [
                (
                    script_empty_hash,
                    contract_path,
                ),
                (
                    script_empty_hash,
                    'Literal script 2',
                ),
            ]

    @pytest.mark.parametrize(
        "for_script, display_names, results",
        [
            ('csv', True, (ID_SCRIPT_HASH, 'Literal script 1')),
            ('csv', False, (ID_SCRIPT_HASH, None)),
            ('tsv', True, (ID_SCRIPT_HASH, 'Literal script 1')),
            ('tsv', False, (ID_SCRIPT_HASH, None)),
        ],
    )
    def test_contract_hashes_for_script(
        self, client: Client, for_script, display_names, results
    ):
        assert client.hash_script(
            [ID_SCRIPT_LITERAL],
            display_names=display_names,
            for_script=for_script,
        ) == [results]


@pytest.mark.contract
@pytest.mark.incremental
class TestContractTypeChecking:
    """Typechecking tests for the address and (contract _) types."""

    def check_address(self, client, address):
        """An address followed by an entrypoint typechecks at type address if
        and only if the entrypoint is not "default"."""

        address_a = f'"{address}%a"'
        address_opt = client.normalize(
            f'"{address}"', 'address', 'Optimized'
        ).strip()
        address_opt_a = client.normalize(
            address_a, 'address', 'Optimized'
        ).strip()

        client.typecheck_data(f'"{address}"', 'address')
        client.typecheck_data(f'{address_a}', 'address')
        client.typecheck_data(f'{address_opt}', 'address')
        client.typecheck_data(f'{address_opt_a}', 'address')

        unexpected_default_error = "unexpected_default_entrypoint"
        not_an_address_error = "not an expression of type address"

        with utils.assert_run_failure(unexpected_default_error):
            client.typecheck_data(f'"{address}%default"', 'address')

        # 64656661756c74 is "default" in hexa
        with utils.assert_run_failure(not_an_address_error):
            client.typecheck_data(address_opt + '64656661756c74', 'address')

    def check_contract_ok(self, client, address, entrypoint, typ):
        """Helper to check that an address followed by an entrypoint typechecks
        at type (contract typ) using both readable and optimised
        representations."""

        address_readable = f'"{address}"'
        if entrypoint is not None:
            address_readable = f'"{address}%{entrypoint}"'

        address_opt = client.normalize(
            address_readable, 'address', 'Optimized'
        ).strip()

        client.typecheck_data(address_readable, f'contract ({typ})')
        client.typecheck_data(address_opt, f'contract ({typ})')

        client.run_script(
            f"""
parameter unit;
storage address;
code {{
        CDR;
        CONTRACT ({typ});
        ASSERT_SOME;
        ADDRESS;
        NIL operation;
        PAIR }}""",
            address_readable,
            'Unit',
            file=False,
        )

    def check_contract_ko(
        self, client, address, entrypoint, typ, expected_error
    ):
        """Helper to check that an address followed by an entrypoint does not
        typecheck at type (contract typ) using both readable and optimised
        representations."""

        address_readable = f'"{address}"'
        if entrypoint is not None:
            address_readable = f'"{address}%{entrypoint}"'

        address_opt = client.normalize(
            address_readable, 'address', 'Optimized'
        ).strip()

        with utils.assert_run_failure(expected_error):
            client.typecheck_data(address_readable, f'contract ({typ})')
        with utils.assert_run_failure(expected_error):
            client.typecheck_data(address_opt, f'contract ({typ})')

        client.run_script(
            f"""
parameter unit;
storage address;
code {{
        CDR;
        DUP;
        CONTRACT ({typ});
        ASSERT_NONE;
        NIL operation;
        PAIR }}""",
            address_readable,
            'Unit',
            file=False,
        )

    def test_implicit(self, client):
        """The address of an implicit account followed by some entrypoint
        typechecks:
        - at type address if the entrypoint is not "default",
        - at type (contract <ty>) if the entrypoint is empty and ty is unit."""

        tz1 = 'tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx'

        self.check_address(client, tz1)
        self.check_contract_ok(client, tz1, None, 'unit')

        no_entrypoint_error = 'Contract has no entrypoint named a'
        type_mismatch_error = 'Type nat is not compatible with type unit.'
        self.check_contract_ko(client, tz1, 'a', 'unit', no_entrypoint_error)
        self.check_contract_ko(client, tz1, 'a', 'nat', no_entrypoint_error)
        self.check_contract_ko(client, tz1, None, 'nat', type_mismatch_error)

    def test_originated_inexistent(self, client):
        """The address of an inexistent originated account followed by some
        entrypoint typechecks:
        - at type address if the entrypoint is not "default",
        - at no (contract _) type."""

        kt1 = 'KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKWP'

        self.check_address(client, kt1)

        invalid_contract_error = 'invalid contract.'
        self.check_contract_ko(
            client, kt1, None, 'unit', invalid_contract_error
        )
        self.check_contract_ko(client, kt1, 'a', 'unit', invalid_contract_error)
        self.check_contract_ko(client, kt1, None, 'nat', invalid_contract_error)
        self.check_contract_ko(client, kt1, 'a', 'nat', invalid_contract_error)

    def test_originated_no_default(self, client, session):
        """The address of an existent originated account that does not specify
        a default entrypoint followed by some entrypoint typechecks:
        - at type address if the entrypoint is not "default",
        - at type (contract <ty>) if
          - the entrypoint is empty and <ty> is the root type
          - the entrypoint is non-empty, one of the declared entrypoints, and
            <ty> is the type associated to that entrypoint."""

        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'simple_entrypoints.tz'
        )
        origination = originate(client, session, path, 'Unit', 0)
        kt1 = origination.contract
        root_type = 'or (unit %A) (or (string %B) (nat %C))'
        a_type = 'unit'
        b_type = 'string'

        self.check_address(client, kt1)
        self.check_contract_ok(client, kt1, None, root_type)
        self.check_contract_ok(client, kt1, 'A', a_type)
        self.check_contract_ok(client, kt1, 'B', b_type)

        no_entrypoint_error = 'Contract has no entrypoint named a'
        self.check_contract_ko(client, kt1, 'a', a_type, no_entrypoint_error)

    def test_originated_with_default(self, client, session):
        """The address of an existent originated account that specifies
        a default entrypoint followed by some entrypoint typechecks:
        - at type address if the entrypoint is not "default",
        - at type (contract <ty>) if
          - the entrypoint is empty and <ty> is the type of the default
            entrypoint
          - the entrypoint is non-empty, one of the declared entrypoints, and
            <ty> is the type associated to that entrypoint."""

        path = os.path.join(
            CONTRACT_PATH, 'entrypoints', 'delegatable_target.tz'
        )
        initial_storage = 'Pair "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "" 0'
        origination = originate(client, session, path, initial_storage, 0)
        kt1 = origination.contract
        root_type = (
            'or (or (key_hash %set_delegate) (unit %remove_delegate))'
            '(or %default string nat)'
        )
        default_type = 'or string nat'

        self.check_address(client, kt1)
        self.check_contract_ok(client, kt1, None, default_type)
        self.check_contract_ok(client, kt1, 'set_delegate', 'key_hash')

        no_entrypoint_error = 'Contract has no entrypoint named a'
        self.check_contract_ko(client, kt1, 'a', root_type, no_entrypoint_error)

        type_mismatch_error = 'is not compatible with type'
        self.check_contract_ko(
            client, kt1, None, root_type, type_mismatch_error
        )
