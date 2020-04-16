import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox

BAKE_ARGS = ['--max-priority', '512', '--minimal-timestamp']
CHAIN_ID = "main"
BLOCK_ID = "head"
PKH = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
PROTOCOL_HASH = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
BLOCK_LEVEL = "3"
LIST_OFFSET = "0"
OPERATION_OFFSET = "0"
CONTRACT_ID = constants.IDENTITIES['bootstrap1']['identity']


@pytest.mark.mempool
@pytest.mark.multinode
@pytest.mark.slow
class TestRPCs:
    " Tests RPCs"

    block_hash = ""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(1, params=constants.NODE_PARAMS)
        sandbox.add_node(2, params=constants.NODE_PARAMS)
        utils.activate_alpha(sandbox.client(1))
        time.sleep(2)

    def test_bake_for(self, sandbox: Sandbox):
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)

    def test_network_self(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/self')

    def test_constants(self, sandbox: Sandbox):
        sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        time.sleep(3)

    def test_chain_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/blocks')

    def test_chain_chain_id(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/chain_id')

    @pytest.mark.skip
    def test_chain_invalid_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/chains/{CHAIN_ID}/invalid_blocks')

    @pytest.mark.skip
    def test_chain_invalid_blocks_block_hash(self, sandbox: Sandbox):
        res = sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/invalid_blocks/'
                              f'{res.block_hash}')

    @pytest.mark.skip
    def test_describe(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/describe')

    @pytest.mark.skip(reason="bug on double encoding registration")
    def test_errors(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', '/errors')

    def test_fetch_protocol_protocol_hash(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/fetch_protocol/{PROTOCOL_HASH}')

    def test_network_connections(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/connections')

    def test_network_connections_peer_id(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get', f'/network/connections/{peer_id}')

    def test_network_greylist_clear(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/greylist/clear')

    def test_network_peers(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/peers')

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
        sandbox.client(1).rpc('get', f'/network/points')

    def test_network_points_point(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}')

    def test_network_points_point_ban(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/ban')

    def test_network_points_point_banned(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/banned')

    def test_network_points_point_trust(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/trust')

    def test_network_points_point_unban(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/unban')

    def test_network_points_point_untrust(self, sandbox: Sandbox):
        points = sandbox.client(1).rpc('get', f'/network/points')
        point = points[-1][0]
        sandbox.client(1).rpc('get', f'/network/points/{point}/untrust')

    def test_network_stat(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/stat')

    def test_network_version(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/version')

    @pytest.mark.skip
    def test_network_versions(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/network/versions')

    def test_protocols(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/protocols')

    def test_protocols_protocol_hash(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/protocols/{PROTOCOL_HASH}')

    def test_workers_block_validator(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/block_validator')

    def test_workers_chain_validators(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/chain_validators')

    def test_workers_chain_validator(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}')

    def test_workers_chain_validator_ddb(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}/ddb')

    def test_workers_chain_validator_peers_validators(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}/'
                              f'peers_validators')

    @pytest.mark.skip
    def test_workers_chain_validator_peer_validator(self, sandbox: Sandbox):
        peer_id = sandbox.client(2).rpc('get', '/network/self')
        sandbox.client(1).rpc('get',
                              f'/workers/chain_validators/{CHAIN_ID}'
                              f'/peers_validators/{peer_id}')

    def test_workers_prevalidators(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/prevalidators')

    def test_workers_prevalidators_chain_id(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get', f'/workers/prevalidators/{CHAIN_ID}')

    def test_chain_block(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}')

    def test_chain_block_context_constants(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/constants')

    def test_chain_block_context_constants_errors(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/constants/errors')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contracts(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/contracts')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_balance(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/balance')

    def test_chain_block_context_contract_counter(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/contracts/{CONTRACT_ID}/counter')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_delegatable(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/delegatable')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_delegate(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/delegate')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_manager(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/manager')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_manager_key(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/manager_key')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_script(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/script')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_spendable(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/spendable')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_contract_storage(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/contracts/{CONTRACT_ID}/storage')

    def test_chain_block_context_delegates(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/delegates')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_balance(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_deactivated(self,
                                                      sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/deactivated')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_delegated_balance(self,
                                                            sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/delegated_balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_delegated_contracts(self,
                                                              sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/delegated_contracts')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_frozen_balance(self,
                                                         sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/frozen_balance')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_frozen_balance_by_cycle(self,
                                                                  sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/delegates/{PKH}/'
                              f'frozen_balance_by_cycle')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_grace_period(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/grace_period')

    @pytest.mark.skip
    # TODO
    def test_chain_block_context_delegate_staking_balance(self,
                                                          sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/delegates/{PKH}/staking_balance')

    def test_chain_block_context_nonces_block_level(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/nonces/{BLOCK_LEVEL}')

    def test_chain_block_context_raw_bytes(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'context/raw/bytes')

    def test_chain_block_context_global_counter(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'context/global_counter')

    def test_chain_block_hash(self, sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/hash')

    def test_chain_block_header(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header')

    def test_chain_block_header_protocol_data(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/protocol_data')

    def test_chain_block_header_protocol_data_raw(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/protocol_data/raw')

    def test_chain_block_header_raw(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/raw')

    def test_chain_block_header_shell(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'header/shell')

    def test_chain_block_helpers_baking_rights(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/baking_rights')

    def test_chain_block_helpers_complete_prefix1(self, sandbox: Sandbox):
        prefix = PKH[:10]
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/complete/{prefix}')

    def test_chain_block_helpers_complete_prefix2(self, sandbox: Sandbox):
        res = sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        prefix = res.block_hash[:5]
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/complete/{prefix}')

    def test_chain_block_helpers_current_level(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/current_level')

    def test_chain_block_helpers_endorsing_rights(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/endorsing_rights')

    def test_chain_block_helpers_levels_in_current_cycle(self,
                                                         sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'helpers/levels_in_current_cycle')

    def test_chain_block_live_blocks(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'live_blocks')

    def test_chain_block_metadata(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'metadata')

    def test_chain_block_operation_hashes(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes')

    def test_add_transactions(self, sandbox: Sandbox):
        sandbox.client(1).transfer(1.000, 'bootstrap1', 'bootstrap2')
        sandbox.client(2).transfer(1.000, 'bootstrap3', 'bootstrap4')
        sandbox.client(1).endorse('bootstrap1')
        sandbox.client(1).bake('bootstrap1', BAKE_ARGS)
        time.sleep(3)

    def test_chain_block_operation_hashes_list_offset(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes/{LIST_OFFSET}')

    def test_chain_block_operation_hashes_list_operation(self,
                                                         sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operation_hashes/{LIST_OFFSET}/'
                              f'{OPERATION_OFFSET}')

    def test_chain_block_operations(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations')

    def test_chain_block_operations_list(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations/{LIST_OFFSET}')

    def test_chain_block_operations_list_operation(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'operations/{LIST_OFFSET}/'
                              f'{OPERATION_OFFSET}')

    def test_chain_block_votes_ballot_list(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/ballot_list')

    def test_chain_block_votes_ballots(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/ballots')

    def test_chain_block_votes_current_period_kind(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_period_kind')

    def test_chain_block_votes_current_proposal(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_proposal')

    def test_chain_block_votes_current_quorum(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              'votes/current_quorum')

    def test_chain_block_votes_listings(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/listings')

    def test_chain_block_votes_proposals(self, sandbox: Sandbox):
        sandbox.client(1).rpc('get',
                              f'/chains/{CHAIN_ID}/blocks/{BLOCK_ID}/'
                              f'votes/proposals')

    def test_stat_gc(self, sandbox: Sandbox):
        assert sandbox.client(1).rpc('get', "/stats/gc")

    def test_stat_memory(self, sandbox: Sandbox):
        assert sandbox.client(1).rpc('get', "/stats/memory")
