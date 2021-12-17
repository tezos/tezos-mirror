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
PROTOCOL_HASH = protocol.HASH
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
    """Adds two nodes to sandbox. Using the first node, originates the
    identity contract `id.tz` with the name contract_name and makes it
    address available under session['originated_accounts'].
    """
    sandbox.add_node(1, params=constants.NODE_PARAMS, mode=request.param)
    sandbox.add_node(2, params=constants.NODE_PARAMS, mode=request.param)
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

    def test_config_file(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/config')

    def test_network_self(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/self')

    def test_constants(self, sandbox: Sandbox):
        sandbox.client(2).rpc('get', '/network/self')
        utils.bake(sandbox.client(1))
        time.sleep(3)

    def test_chain_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks')

    def test_chain_chain_id(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/chain_id')

    def test_chain_invalid_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/invalid_blocks')

    def test_errors(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/errors')

    def test_fetch_protocol_protocol_hash(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/fetch_protocol/{PROTOCOL_HASH}')

    def test_network_connections(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/connections')

    def test_network_connections_peer_id(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/connections/{peer_id}')

    def test_network_greylist_clear(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/greylist/clear')

    def test_network_peers(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/peers')

    def test_network_peers_peer_id(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}')

    def test_network_peers_peer_id_ban(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/ban')

    def test_network_peers_peer_id_banned(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/banned')

    def test_network_peers_peer_id_unban(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/unban')

    def test_network_peers_peer_id_untrust(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/untrust')

    def test_network_peers_peer_id_trust(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/peers/{peer_id}/trust')

    def test_network_points(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/points')

    def test_network_points_point(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}')

    def test_network_points_point_ban(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/ban')

    def test_network_points_point_banned(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/banned')

    def test_network_points_point_trust(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/trust')

    def test_network_points_point_unban(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/unban')

    def test_network_points_point_untrust(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', '/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/untrust')

    def test_network_stat(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/stat')

    def test_network_version(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/version')

    def test_network_versions(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/network/versions')

    def test_protocols(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/protocols')

    def test_protocols_protocol_hash(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/protocols/{PROTOCOL_HASH}')

    def test_workers_block_validator(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/workers/block_validator')

    def test_workers_chain_validators(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/workers/chain_validators')

    def test_workers_chain_validator(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/chain_validators/{CHAIN_ID}')

    def test_workers_chain_validator_ddb(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/workers/chain_validators/{CHAIN_ID}/ddb'
        )

    def test_workers_chain_validator_peers_validators(self, sandbox):
        sandbox.client(1).rpc(
            'get', f'/workers/chain_validators/{CHAIN_ID}/' 'peers_validators'
        )

    def test_workers_prevalidators(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/workers/prevalidators')

    def test_workers_prevalidators_chain_id(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/prevalidators/{CHAIN_ID}')

    def test_chain_block(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}')

    def test_chain_block_context_constants(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/constants'
        )

    def test_chain_block_context_constants_errors(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/constants/errors',
        )

    def test_chain_block_context_contracts(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/contracts'
        )

    def test_chain_block_context_contract_id(
        self, sandbox: Sandbox, session: dict
    ):
        accounts = session["originated_accounts"] + session["implicit_accounts"]
        for contract_id in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}',
            )

    def test_chain_block_context_contract_balance(
        self, sandbox: Sandbox, session: dict
    ):
        accounts = session["originated_accounts"] + session["implicit_accounts"]
        for contract_id in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}/balance',
            )

    def test_chain_block_context_contract_counter(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit contracts, see
        # proto_012_PsiThaCa/lib_protocol/contract_repr.ml
        for contract_id in session["implicit_accounts"]:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}/counter',
            )

    def test_chain_block_context_contract_delegate(
        self, sandbox: Sandbox, session: dict
    ):
        for contract_id in session["implicit_accounts"]:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}/delegate',
            )

    def test_chain_block_context_contract_script_originated(
        self, sandbox: Sandbox, session: dict
    ):
        # only originated contracts
        accounts = session["originated_accounts"]
        for contract_id in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}/script',
            )

    def test_chain_block_context_contract_script_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        accounts = session["implicit_accounts"]
        for contract_id in accounts:
            with utils.assert_run_failure('No service found at this URL'):
                sandbox.client(1).rpc(
                    'get',
                    f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                    f'context/contracts/{contract_id}/'
                    'script',
                )

    def test_chain_block_context_contract_storage_originated(
        self, sandbox: Sandbox, session: dict
    ):
        # only originated contracts
        accounts = session["originated_accounts"]
        for contract_id in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/contracts/{contract_id}/storage',
            )

    def test_chain_block_context_contract_storage_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit contracts
        accounts = session["implicit_accounts"]
        for contract_id in accounts:
            with utils.assert_run_failure('No service found at this URL'):
                sandbox.client(1).rpc(
                    'get',
                    f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                    f'context/contracts/{contract_id}/'
                    'storage',
                )

    def test_chain_block_context_delegates(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'context/delegates'
        )

    def test_chain_block_context_delegate_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}',
            )

    def test_chain_block_context_delegate_deactivated_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/deactivated',
            )

    def test_chain_block_context_delegate_delegated_balance_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/delegated_balance',
            )

    def test_chain_block_context_delegate_delegated_contracts_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/'
                'delegated_contracts',
            )

    def test_chain_block_context_delegate_frozen_deposits_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/'
                'frozen_deposits',
            )

    def test_chain_block_context_delegate_grace_period_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/grace_period',
            )

    def test_chain_block_context_delegate_staking_balance_implicit(
        self, sandbox: Sandbox, session: dict
    ):
        # only implicit accounts
        accounts = session["implicit_accounts"]
        for pkh in accounts:
            sandbox.client(1).rpc(
                'get',
                f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                f'context/delegates/{pkh}/staking_balance',
            )

    def test_chain_block_context_nonces_block_level(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            f'context/nonces/{BLOCK_LEVEL}',
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

    def test_chain_block_header_shell(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'header/shell'
        )

    def test_chain_block_helpers_baking_rights(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'helpers/baking_rights',
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

    def test_chain_block_helpers_current_level(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'helpers/current_level',
        )

    def test_chain_block_helpers_endorsing_rights(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'helpers/endorsing_rights',
        )

    def test_chain_block_helpers_levels_in_current_cycle(
        self, sandbox: Sandbox
    ):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
            'helpers/levels_in_current_cycle',
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
        sandbox.client(2).transfer(1.000, 'bootstrap3', 'bootstrap4')
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

    def test_chain_block_votes_ballot_list(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' f'votes/ballot_list'
        )

    def test_chain_block_votes_ballots(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/ballots'
        )

    def test_chain_block_votes_current_period(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/current_period',
        )

    def test_chain_block_votes_current_proposal(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/current_proposal',
        )

    def test_chain_block_votes_current_quorum(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get',
            f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/current_quorum',
        )

    def test_chain_block_votes_listings(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/listings'
        )

    def test_chain_block_votes_proposals(self, sandbox: Sandbox):
        sandbox.client(1).rpc(
            'get', f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/' 'votes/proposals'
        )

    def test_stat_gc(self, sandbox: Sandbox):
        assert sandbox.client(1).rpc('get', "/stats/gc")

    def test_stat_memory(self, sandbox: Sandbox):
        assert sandbox.client(1).rpc('get', "/stats/memory")


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
