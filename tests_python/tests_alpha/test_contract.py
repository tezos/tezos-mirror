import os
import pytest

from client.client import Client
from tools import utils
from tools.constants import IDENTITIES
from tools.utils import originate
from .contract_paths import CONTRACT_PATH


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
