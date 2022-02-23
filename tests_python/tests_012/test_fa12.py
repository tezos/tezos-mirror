"""This file tests the tezos-client commands offering support for the
FA1.2 standard. Several implementations of the standard are tested."""

import json
import os
import pytest
from tools import utils
from tools.constants import IDENTITIES
from client.client import Client
from .contract_paths import CONTRACT_PATH

BAKE_ARGS = ['--minimal-timestamp']
BURN_CAP_ARGS = ['--burn-cap', '1.0']


def reset_viewer(client: Client, contract: str):
    args = ['--arg', '0']
    client.transfer(0.0, 'bootstrap2', contract, args)
    client.bake('bootstrap5', BAKE_ARGS)


def check_expected_balance(client, token_contract, addr, expected):
    bal = client.fa12_get_balance_offchain(token_contract, addr, [])
    return bal.amount == expected


def check_expected_allowance(client, token_contract, src, dst, expected):
    alw = client.fa12_get_allowance_offchain(token_contract, src, dst, [])
    return alw.amount == expected


@pytest.fixture(
    scope="class",
    ids=[
        'fa12_reference',
        'lqt_fa12',
    ],
    params=[
        (
            'fa12_reference',
            'fa12_reference.tz',
            lambda key: f'Pair {{}} (Pair "{key}" (Pair False 0))',
        ),
        (
            'lqt_fa12',
            'lqt_fa12.mligo.tz',
            lambda key: f'Pair {{}} {{}} "{key}" 0',
        ),
    ],
)
def token_contract(client: Client, session: dict, request):
    (contract_alias, contract_path, init_fun) = request.param

    path = os.path.join(CONTRACT_PATH, 'mini_scenarios', contract_path)
    identity = IDENTITIES['bootstrap2']['identity']
    init = init_fun(identity)

    utils.originate(client, session, path, init, 0, contract_alias)

    if contract_alias == 'fa12_reference':
        identity = IDENTITIES['bootstrap2']['identity']
        param = f'(Pair "{identity}" 20000)'
        args = [
            '--entrypoint',
            'mint',
            '--arg',
            param,
            '--burn-cap',
            '0.078',
        ]
        client.transfer(0, 'bootstrap2', contract_alias, args)
        client.bake('bootstrap5', BAKE_ARGS)
    elif contract_alias == 'lqt_fa12':
        identity = IDENTITIES['bootstrap2']['identity']
        param = f'(Pair 20000 "{identity}")'
        args = [
            '--entrypoint',
            'mintOrBurn',
            '--arg',
            param,
            '--burn-cap',
            '0.078',
        ]
        client.transfer(0, 'bootstrap2', contract_alias, args)
        client.bake('bootstrap5', BAKE_ARGS)

    return contract_alias


@pytest.fixture(scope="class")
def view(client: Client, session: dict):
    # Only deploy once, it can be used by any fa1.2 contract
    contract_alias = 'nat-viewer'
    args = ['--init', '0', '--burn-cap', '0.08']
    viewer = "parameter nat; storage nat; code { CAR; NIL operation; PAIR; }"

    origination = client.originate(
        contract_alias, 0, 'bootstrap1', viewer, args
    )
    session['contract'] = origination.contract
    client.bake('bootstrap5', BAKE_ARGS)
    return contract_alias


class TestFA12Basic:
    def test_check_contract(self, client: Client, token_contract: str):
        assert client.fa12_check(token_contract).check

    # NOTE: this test does not depend on the token_contract
    # fixture (nor should it)
    def test_check_contract_fail(self, client: Client, session: dict):
        path = os.path.join(CONTRACT_PATH, 'entrypoints', 'manager.tz')
        identity = IDENTITIES['bootstrap2']['identity']
        init = f'"{identity}"'
        token_contract = 'manager-fail'
        utils.originate(client, session, path, init, 0, token_contract)

        assert not client.fa12_check(token_contract).check

    def test_get_balance_offchain(self, client: Client, token_contract: str):
        res = client.fa12_get_balance_offchain(token_contract, 'bootstrap2', [])
        assert res.amount == 20000

    def test_get_allowance_offchain(self, client: Client, token_contract: str):
        res = client.fa12_get_allowance_offchain(
            token_contract, 'bootstrap2', 'bootstrap3', []
        )
        assert res.amount == 0

    def test_get_total_supply_offchain(
        self, client: Client, token_contract: str
    ):
        res = client.fa12_get_total_supply_offchain(token_contract, [])
        expected = 20000
        assert res.amount == expected

    def test_get_balance_callback(
        self, client: Client, token_contract: str, view
    ):
        reset_viewer(client, view)
        client.fa12_get_balance_callback(
            token_contract, 'bootstrap2', view, BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)
        utils.assert_storage_contains(client, view, '20000')

    def test_get_allowance_callback(
        self, client: Client, token_contract: str, view
    ):
        reset_viewer(client, view)
        client.fa12_get_allowance_callback(
            token_contract, 'bootstrap2', 'bootstrap3', view, BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)
        utils.assert_storage_contains(client, view, '0')

    def test_get_total_supply_callback(
        self, client: Client, token_contract: str, view
    ):
        reset_viewer(client, view)
        client.fa12_get_total_supply_callback(
            token_contract, 'bootstrap2', view, BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)
        expected = '20000'
        utils.assert_storage_contains(client, view, expected)


@pytest.mark.incremental
class TestFA12Incremental:
    def test_transfer(self, client: Client, token_contract: str):
        client.fa12_transfer(
            token_contract, 100, 'bootstrap2', 'bootstrap3', BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)

        assert check_expected_balance(
            client, token_contract, 'bootstrap2', 19900
        ) and check_expected_balance(client, token_contract, 'bootstrap3', 100)

    def test_transfer_not_enough_balance(
        self, client: Client, token_contract: str
    ):
        if token_contract == 'fa12_reference':
            error_pattern = r'Not enough balance'
        else:
            error_pattern = ''
        with utils.assert_run_failure(error_pattern):
            client.fa12_transfer(
                token_contract, 200, 'bootstrap3', 'bootstrap2', BURN_CAP_ARGS
            )

    def test_approve(self, client: Client, token_contract: str):
        client.fa12_approve(
            token_contract, 20, 'bootstrap2', 'bootstrap3', BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)

        assert check_expected_allowance(
            client, token_contract, 'bootstrap2', 'bootstrap3', 20
        )

    def test_approve_unsafe_allowance_change(
        self, client: Client, token_contract
    ):
        if token_contract == 'fa12_reference':
            error_pattern = r'Unsafe allowance change'
        else:
            error_pattern = ''
        with utils.assert_run_failure(error_pattern):
            client.fa12_approve(
                token_contract, 30, 'bootstrap2', 'bootstrap3', BURN_CAP_ARGS
            )

    def test_transfer_as(self, client: Client, token_contract: str):
        client.fa12_transfer_as(
            token_contract,
            10,
            'bootstrap2',
            'bootstrap3',
            'bootstrap3',
            BURN_CAP_ARGS,
        )
        client.bake('bootstrap5', BAKE_ARGS)

        assert (
            check_expected_balance(client, token_contract, 'bootstrap2', 19890)
            and check_expected_balance(
                client, token_contract, 'bootstrap3', 110
            )
            and check_expected_allowance(
                client, token_contract, 'bootstrap2', 'bootstrap3', 10
            )
        )

    def test_transfer_as_not_enough_allowance(
        self, client: Client, token_contract
    ):
        if token_contract == 'fa12_reference':
            error_pattern = r'Not enough allowance'
        else:
            error_pattern = ''
        with utils.assert_run_failure(error_pattern):
            client.fa12_transfer_as(
                token_contract,
                20,
                'bootstrap2',
                'bootstrap3',
                'bootstrap3',
                BURN_CAP_ARGS,
            )

    def test_multiple_transfers(self, client: Client, token_contract: str):
        op1 = client.fa12_mk_batch_transfer('bootstrap4', token_contract, 100)
        op2 = client.fa12_mk_batch_transfer('bootstrap5', token_contract, 10)
        json_ops = json.dumps(op1 + op2, separators=(',', ':'))
        client.fa12_multiple_tokens_transfers(
            'bootstrap2', json_ops, BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)

        assert (
            check_expected_balance(client, token_contract, 'bootstrap2', 19780)
            and check_expected_balance(
                client, token_contract, 'bootstrap4', 100
            )
            and check_expected_balance(client, token_contract, 'bootstrap5', 10)
            and check_expected_allowance(
                client, token_contract, 'bootstrap2', 'bootstrap3', 10
            )
        )

    def test_multiple_transfers_fail(self, client: Client, token_contract: str):
        op1 = client.fa12_mk_batch_transfer('bootstrap4', token_contract, 100)
        op2 = client.fa12_mk_batch_transfer(
            'bootstrap5', token_contract, 100000
        )
        json_ops = json.dumps(op1 + op2, separators=(',', ':'))

        bal_sender_before = client.fa12_get_balance_offchain(
            token_contract, 'bootstrap2', []
        )
        bal_receiver1_before = client.fa12_get_balance_offchain(
            token_contract, 'bootstrap4', []
        )
        bal_receiver2_before = client.fa12_get_balance_offchain(
            token_contract, 'bootstrap5', []
        )

        error_pattern = r'multiple transfers simulation failed'
        with utils.assert_run_failure(error_pattern):
            client.fa12_multiple_tokens_transfers(
                'bootstrap2', json_ops, BURN_CAP_ARGS
            )

        client.bake('bootstrap5', BAKE_ARGS)

        assert (
            check_expected_balance(
                client,
                token_contract,
                'bootstrap2',
                bal_sender_before.amount,
            )
            and check_expected_balance(
                client,
                token_contract,
                'bootstrap4',
                bal_receiver1_before.amount,
            )
            and check_expected_balance(
                client,
                token_contract,
                'bootstrap5',
                bal_receiver2_before.amount,
            )
        )

    def test_multiple_transfers_as(self, client: Client, token_contract: str):
        client.fa12_approve(
            token_contract, 0, 'bootstrap2', 'bootstrap3', BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)

        client.fa12_approve(
            token_contract, 30, 'bootstrap2', 'bootstrap3', BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)
        check_allowance_before = check_expected_allowance(
            client, token_contract, 'bootstrap2', 'bootstrap3', 30
        )

        op1 = client.fa12_mk_batch_transfer('bootstrap4', token_contract, 10)
        op2 = client.fa12_mk_batch_transfer('bootstrap5', token_contract, 20)
        json_ops = json.dumps(op1 + op2, separators=(',', ':'))

        client.fa12_multiple_tokens_transfers_as(
            'bootstrap2', 'bootstrap3', json_ops, BURN_CAP_ARGS
        )
        client.bake('bootstrap5', BAKE_ARGS)

        assert (
            check_expected_balance(client, token_contract, 'bootstrap2', 19750)
            and check_expected_balance(
                client, token_contract, 'bootstrap4', 110
            )
            and check_expected_balance(client, token_contract, 'bootstrap5', 30)
            and check_allowance_before
            and check_expected_allowance(
                client, token_contract, 'bootstrap2', 'bootstrap3', 0
            )
        )
