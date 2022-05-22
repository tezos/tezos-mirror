import os
import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol
from . import contract_paths

CHAIN_ID = "main"
BLOCK_ID = "head"
PKH = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
BLOCK_LEVEL = "3"
LIST_OFFSET = "0"
OPERATION_OFFSET = "0"


@pytest.fixture(scope="class")
def session():
    session = {}
    session["implicit_accounts"] = [
        "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv",
        "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
    ]
    return session


@pytest.fixture(scope="class")
def contract_name():
    return "contract_identity"


@pytest.fixture(scope="class", params=[None, "proxy"])
def sandbox(request, sandbox: Sandbox, contract_name, session: dict):
    """Adds a node to sandbox. Originates the identity contract
    `id.tz` with the name contract_name and makes it address available
    under session['originated_accounts'].
    """
    sandbox.add_node(1, params=constants.NODE_PARAMS, mode=request.param)
    client = sandbox.client(1)
    parameters = protocol.get_parameters()
    parameters['consensus_threshold'] = 0
    protocol.activate(
        sandbox.client(1), activate_in_the_past=True, parameters=parameters
    )

    utils.bake(client)
    time.sleep(2)
    # Deploy a contract
    contract = os.path.join(contract_paths.CONTRACT_PATH, 'attic', 'id.tz')
    args = ['--init', "\"tezos\"", '--burn-cap', '10.0']
    origination = client.originate(
        contract_name, 10.0, "bootstrap1", contract, args
    )
    session['originated_accounts'] = [origination.contract]
    utils.bake(client)
    assert utils.check_block_contains_operations(
        client, [origination.operation_hash]
    )
    return sandbox


class TestDeprecatedRPCs:
    def test_chain_block_context_contract_delegatable(
        self, sandbox: Sandbox, session: dict
    ):
        for contract_id in session["implicit_accounts"]:
            with utils.assert_run_failure(r"Did not find service"):
                sandbox.client(1).rpc(
                    'get',
                    f'/chains/{CHAIN_ID}/blocks/'
                    f'{BLOCK_ID}/context/contracts/'
                    f'{contract_id}/delegatable',
                )

    def test_chain_block_context_contract_spendable(
        self, sandbox: Sandbox, session: dict
    ):
        accounts = session["originated_accounts"] + session["implicit_accounts"]
        for contract_id in accounts:
            with utils.assert_run_failure(r"Did not find service"):
                sandbox.client(1).rpc(
                    'get',
                    f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                    f'context/contracts/{contract_id}/'
                    'spendable',
                )
