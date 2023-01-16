from typing import List
import pytest
from client.client import Client
from tools import constants, utils
from tools.paths import ACCOUNT_PATH
from .contract_paths import find_script


BAKE_ARGS: List[str] = []
TRANSFER_ARGS = ['--burn-cap', '0.257']


@pytest.mark.incremental
class TestRawContext:
    def test_bake(self, client: Client):
        utils.bake(client, 'bootstrap4')

    def test_gen_keys(self, client: Client, session):
        session['keys'] = ['foo', 'bar', 'boo']
        sigs = [None, 'secp256k1', 'ed25519']
        for key, sig in zip(session['keys'], sigs):
            args = [] if sig is None else ['--sig', sig]
            client.gen_key(key, args)

    def test_transfers(self, client: Client, session):
        client.transfer(1000, 'bootstrap1', session['keys'][0], TRANSFER_ARGS)
        utils.bake(client)
        client.transfer(2000, 'bootstrap1', session['keys'][1], TRANSFER_ARGS)
        utils.bake(client)
        client.transfer(3000, 'bootstrap1', session['keys'][2], TRANSFER_ARGS)
        utils.bake(client)

    def test_balances(self, client: Client, session):
        assert client.get_balance(session['keys'][0]) == 1000
        assert client.get_balance(session['keys'][1]) == 2000
        assert client.get_balance(session['keys'][2]) == 3000

    def test_transfer_bar_foo(self, client: Client, session):
        client.reveal(session['keys'][1], ['--fee', '0', '--force-low-fee'])
        utils.bake(client)
        client.transfer(
            1000,
            session['keys'][1],
            session['keys'][0],
            ['--fee', '0', '--force-low-fee'],
        )
        utils.bake(client)

    def test_balances_bar_foo(self, client: Client, session):
        assert client.get_balance(session['keys'][0]) == 2000
        assert client.get_balance(session['keys'][1]) == 1000

    def test_transfer_foo_bar(self, client: Client, session):
        client.reveal(session['keys'][0], ['--fee', '0', '--force-low-fee'])
        utils.bake(client)
        client.transfer(
            1000, session['keys'][0], session['keys'][1], ['--fee', '0.05']
        )
        utils.bake(client)

    def test_balances_foo_bar(self, client: Client, session):
        # 999.95 = 1000 - transfer fees
        assert client.get_balance(session['keys'][0]) == 999.95
        assert client.get_balance(session['keys'][1]) == 2000

    def test_transfer_failure(self, client: Client, session):
        with pytest.raises(Exception):
            client.transfer(999.95, session['keys'][0], session['keys'][1])

    def test_originate_contract_noop(self, client: Client):
        contract = find_script(['opcodes', 'noop'])
        client.remember('noop', contract)
        client.typecheck(contract)
        client.originate(
            'noop', 1000, 'bootstrap1', contract, ['--burn-cap', '0.295']
        )
        utils.bake(client)

    def test_transfer_to_noop(self, client: Client):
        client.transfer(10, 'bootstrap1', 'noop', ['--arg', 'Unit'])
        utils.bake(client)

    def test_contract_hardlimit(self, client: Client):
        contract = find_script(['mini_scenarios', 'hardlimit'])
        client.originate(
            'hardlimit',
            1000,
            'bootstrap1',
            contract,
            ['--init', '3', '--burn-cap', '0.341'],
        )
        utils.bake(client)
        client.transfer(10, 'bootstrap1', 'hardlimit', ['--arg', 'Unit'])
        utils.bake(client)
        client.transfer(10, 'bootstrap1', 'hardlimit', ['--arg', 'Unit'])
        utils.bake(client)

    def test_transfers_bootstraps5_bootstrap1(self, client: Client):
        bootstrap5 = constants.IDENTITIES['bootstrap5']['identity']
        all_deposits = client.frozen_deposits(bootstrap5)
        balance = client.get_mutez_balance('bootstrap5')
        assert balance + all_deposits == utils.mutez_of_tez(4000000.0)
        client.transfer(
            400000,
            'bootstrap5',
            'bootstrap1',
            ['--fee', '0', '--force-low-fee'],
        )
        utils.bake(client)
        client.transfer(
            400000,
            'bootstrap1',
            'bootstrap5',
            ['--fee', '0', '--force-low-fee'],
        )
        utils.bake(client)
        all_deposits = client.frozen_deposits(bootstrap5)
        assert client.get_mutez_balance(
            'bootstrap5'
        ) + all_deposits == utils.mutez_of_tez(4000000.0)

    def test_activate_accounts(self, client: Client, session):
        account = f"{ACCOUNT_PATH}/king_commitment.json"
        session['keys'] += ['king', 'queen']
        client.activate_account(session['keys'][3], account)
        utils.bake(client)
        account = f"{ACCOUNT_PATH}/queen_commitment.json"
        client.activate_account(session['keys'][4], account)
        utils.bake(client)
        assert client.get_balance(session['keys'][3]) == 23932454.669343
        assert client.get_balance(session['keys'][4]) == 72954577.464032

    def test_transfer_king_queen(self, client: Client, session):
        keys = session['keys']
        client.transfer(10, keys[3], keys[4], TRANSFER_ARGS)
        utils.bake(client)
