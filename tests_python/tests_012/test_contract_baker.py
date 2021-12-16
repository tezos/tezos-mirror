import os
import pytest
from tools import constants, utils
from client.client import Client
from . import contract_paths


@pytest.mark.contract
@pytest.mark.baker
@pytest.mark.incremental
class TestOriginationCall:
    """Test a simple contract origination and call"""

    def test_originate(self, client: Client, session: dict):
        initial_storage = 'Unit'
        contract = os.path.join(
            contract_paths.OPCODES_CONTRACT_PATH, 'transfer_tokens.tz'
        )
        args = ['--init', initial_storage, '--burn-cap', '0.400']
        origination = client.originate(
            'foobar', 1000, 'bootstrap1', contract, args
        )
        session['contract'] = origination.contract
        utils.bake(client, bake_for="bootstrap5")

        # Unsolved mystery:
        #    client.wait_for_inclusion(origination.operation_hash)
        # fails sometimes with tezos-client crashing. Maybe caused with
        # subprocess captured of forked process output?
        #
        # Safer to poll with `check_block_contain_operations`
        assert utils.check_block_contains_operations(
            client, [origination.operation_hash]
        )

    def test_call(self, client: Client, session: dict):
        contract = session['contract']
        bootstrap3 = '"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"'
        transfer = client.call('bootstrap2', contract, ['--arg', bootstrap3])
        utils.bake(client, bake_for="bootstrap5")
        assert utils.check_block_contains_operations(
            client, [transfer.operation_hash]
        )

    def test_balance(self, client: Client):
        bootstrap3 = constants.IDENTITIES['bootstrap3']['identity']
        deposit = client.frozen_deposits(bootstrap3)
        balance = client.get_mutez_balance('bootstrap3')
        assert balance + deposit == utils.mutez_of_tez(4000100.0)

    def test_query_storage(self, client: Client, session: dict):
        contract = session['contract']
        url = f'/chains/main/blocks/head/context/contracts/{contract}/storage'
        res = client.rpc('get', url)
        assert res['prim'] == 'Unit'
