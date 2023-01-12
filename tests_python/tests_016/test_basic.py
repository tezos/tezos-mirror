from typing import List
import pytest
from client.client import Client
from tools import utils
from tools.paths import ACCOUNT_PATH
from .contract_paths import find_script


BAKE_ARGS: List[str] = []
TRANSFER_ARGS = ['--burn-cap', '0.257']


@pytest.mark.incremental
class TestRawContext:
    def test_bake(self, client: Client):
        utils.bake(client, 'bootstrap4')

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
