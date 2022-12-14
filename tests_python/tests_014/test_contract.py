import os
import pytest

from client.client import Client
from tools import utils
from tools.constants import IDENTITIES
from tools.utils import originate
from .contract_paths import CONTRACT_PATH


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
