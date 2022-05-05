"""Test the multiple transfer feature of tezos-client"""
import os
import json
import pytest

from client.client import Client
from tools import utils
from tools.constants import IDENTITIES
from .contract_paths import CONTRACT_PATH


def manager(client: Client) -> str:
    """Originate and return the alias of a manager contract"""
    alias = 'manager'
    path = os.path.join(CONTRACT_PATH, 'entrypoints', alias + '.tz')
    pubkey = IDENTITIES['bootstrap2']['identity']
    utils.init_with_transfer(
        client, path, f'"{pubkey}"', 1000, sender='bootstrap1'
    )
    return alias


@pytest.fixture(scope="class")
def big_map_entrypoints(client: Client) -> str:
    """Originate and return the alias of a big_map_entrypoints contract"""
    alias = 'big_map_entrypoints'
    path = os.path.join(CONTRACT_PATH, 'entrypoints', alias + '.tz')
    utils.init_with_transfer(
        client, path, 'Pair {} {Elt "Hello" 42}', 1000, sender='bootstrap1'
    )
    return alias


@pytest.fixture
def source(client: Client, request) -> str:
    """A contract alias that will be used as source for a multiple transfers
    command.

    This fixture is indirectly instantiated: the argument specifies
    whether the alias of an originated manager contract should be
    returned (if argument equals 'manager'). Otherwise, the alias is
    assumed to already exist in the client's wallet and is returned
    unmodified.
    """
    alias = request.param
    return manager(client) if alias == 'manager' else alias


@pytest.mark.contract
@pytest.mark.incremental
class TestMultipleTransfers:
    def test_empty(self, client: Client):
        with utils.assert_run_failure(r'Empty operation list'):
            json_obj = '[]'
            client.run(client.cmd_batch('bootstrap1', json_obj))

    @pytest.mark.parametrize(
        "payer, source",
        [('bootstrap2', 'manager'), ('bootstrap4', 'bootstrap4')],
        indirect=["source"],
    )
    def test_transfer_json_to_entrypoint_with_args(
        self, big_map_entrypoints: str, client: Client, payer: str, source: str
    ):
        """Test the multiple transfers command with a single transfer
        to a contract's entrypoint, with implicit accounts or a manager
        contract as source as per parametrization.
        """
        balance_source = client.get_mutez_balance(source)
        balance_payer = client.get_mutez_balance(payer)
        fee = 0.0123
        fee_mutez = utils.mutez_of_tez(fee)
        json_obj = [
            {
                "destination": big_map_entrypoints,
                "amount": "0",
                "fee": str(fee),
                "gas-limit": "65942",
                "storage-limit": "1024",
                "arg": '"Hello"',
                "entrypoint": "mem_right",
            }
        ]
        json_ops = json.dumps(json_obj, separators=(',', ':'))
        client.run(client.cmd_batch(source, json_ops))
        utils.bake(client, 'bootstrap5')
        new_balance_source = client.get_mutez_balance(source)
        new_balance_payer = client.get_mutez_balance(payer)

        if payer != source:
            assert balance_source == new_balance_source

        assert balance_payer - fee_mutez == new_balance_payer

    @pytest.mark.parametrize(
        "payer, source",
        [('bootstrap2', 'manager'), ('bootstrap4', 'bootstrap4')],
    )
    def test_multiple_transfers(self, client: Client, payer: str, source: str):
        """Test a multiple transfers to implicit accounts, with implicit
        accounts or a manager contract as source as per parametrization.
        """
        balance_source = client.get_mutez_balance(source)
        balance_bootstrap1 = client.get_mutez_balance('bootstrap1')
        balance_bootstrap3 = client.get_mutez_balance('bootstrap3')
        amount_1 = 10.1
        amount_mutez_1 = utils.mutez_of_tez(amount_1)
        amount_3 = 11.01
        amount_mutez_3 = utils.mutez_of_tez(amount_3)
        json_obj = [
            {"destination": "bootstrap1", "amount": str(amount_1)},
            {"destination": "bootstrap3", "amount": str(amount_3)},
        ]
        json_ops = json.dumps(json_obj, separators=(',', ':'))
        client.run(client.cmd_batch(source, json_ops))
        utils.bake(client, 'bootstrap5')
        new_balance_source = client.get_mutez_balance(source)
        new_balance_bootstrap1 = client.get_mutez_balance('bootstrap1')
        new_balance_bootstrap3 = client.get_mutez_balance('bootstrap3')

        if payer == source:
            fee_first_transfer = 394
            fee_second_transfer = 298
            source_fee_mutez = fee_first_transfer + fee_second_transfer
        else:
            source_fee_mutez = 0

        assert (
            balance_source - amount_mutez_1 - amount_mutez_3 - source_fee_mutez
            == new_balance_source
        )
        assert balance_bootstrap1 + amount_mutez_1 == new_balance_bootstrap1
        assert balance_bootstrap3 + amount_mutez_3 == new_balance_bootstrap3
