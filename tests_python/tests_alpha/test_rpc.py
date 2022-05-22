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


@pytest.mark.incremental
@pytest.mark.mempool
@pytest.mark.multinode
@pytest.mark.slow
class TestRPCsExistence:
    """
    Tests the existence of RPCs. It does not check the output!
    Existence relying on the storage are tested using bootstrap
    accounts/originated contracts.
    """

    block_hash = ""

    def test_chain_blocks(self, sandbox: Sandbox):
        utils.bake(sandbox.client(1))
        time.sleep(3)
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks')

    def test_chain_chain_id(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/chain_id')

    def test_chain_invalid_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/invalid_blocks')

    def test_chain_block(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}')

    def test_chain_block_context_constants_errors(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/constants/errors',
        )

    def test_chain_block_context_nonces_block_level(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'context/nonces/{BLOCK_LEVEL}',
        )

    def test_chain_block_context_sc_rollup(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/sc_rollup'
        )

    def test_chain_block_context_raw_bytes(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/raw/bytes'
        )

    def test_chain_block_hash(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/hash'
        )

    def test_chain_block_header(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'header'
        )

    def test_chain_block_header_protocol_data(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'header/protocol_data',
        )

    def test_chain_block_header_protocol_data_raw(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'header/protocol_data/raw',
        )

    def test_chain_block_header_raw(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'header/raw'
        )

    def test_chain_block_helpers_complete_prefix1(self, sandbox: Sandbox):
        prefix = PKH[:10]
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'helpers/complete/{prefix}',
        )

    def test_chain_block_helpers_complete_prefix2(self, sandbox: Sandbox):
        res = utils.bake(sandbox.client(1))
        prefix = res.block_hash[:5]
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'helpers/complete/{prefix}',
        )

    def test_chain_block_live_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'live_blocks'
        )

    def test_chain_block_metadata(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'metadata'
        )

    def test_chain_block_operation_hashes(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'operation_hashes'
        )

    def test_add_transactions(self, sandbox: Sandbox):
        sandbox.client(1).transfer(1.000, 'bootstrap1', 'bootstrap2')
        # FIXME: Use client.endorse
        # Not clear where to put it w.r.t to Tenderbake,
        # knowing that bake for does endorse
        sandbox.client(1).run(["endorse", "for", 'bootstrap2', '--force'])
        utils.bake(sandbox.client(1))
        time.sleep(3)

    def test_chain_block_operation_hashes_list_offset(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'operation_hashes/{LIST_OFFSET}',
        )

    def test_chain_block_operation_hashes_list_operation(
        self, sandbox: Sandbox
    ):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'operation_hashes/{LIST_OFFSET}/{OPERATION_OFFSET}',
        )

    def test_chain_block_operations(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'operations'
        )

    def test_chain_block_operations_list(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'operations/{LIST_OFFSET}',
        )

    def test_chain_block_operations_list_operation(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'operations/{LIST_OFFSET}/'
            f'{OPERATION_OFFSET}',
        )


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
