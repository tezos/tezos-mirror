from os import path

import pytest

from client import client_output
from client.client import Client
from tools import constants, utils
from tools.constants import BOOTSTRAP_BAKERS, IDENTITIES
from . import protocol
from .contract_paths import CONTRACT_PATH

BAKE_ARGS = ['--minimal-timestamp']
BLOCKS_PER_CYCLE = 4


@pytest.fixture(scope="class")
def client(sandbox):
    """One node, 4 blocks per voting period and cycle for fast voting
    progression."""
    parameters = protocol.PARAMETERS
    parameters["time_between_blocks"] = ["1", "0"]
    parameters["blocks_per_voting_period"] = 4
    parameters["blocks_per_cycle"] = BLOCKS_PER_CYCLE
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    protocol.activate(sandbox.client(0), parameters)
    yield sandbox.client(0)


@pytest.mark.contract
@pytest.mark.baker
@pytest.mark.incremental
class TestBakerCompatibility:
    """Test actions where bakers' consensus keys are being used as sources and
    targets of operations."""

    def test_set_delegation_to_baker_key(self, client: Client):
        client.set_delegate('bootstrap1', 'baker3_key')
        client.bake('baker5', BAKE_ARGS)
        delegate = BOOTSTRAP_BAKERS[2]['hash']
        assert client.get_delegate('bootstrap1', []).delegate == delegate
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker3_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[2]['hash'],
            }
        ]

    def test_transfer_to_baker_key(self, client: Client):
        balance_source = client.get_mutez_balance('bootstrap2')
        balance_target = client.get_mutez_balance('baker1')
        amount = 10.001
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'bootstrap2', 'baker1_key')
        client.bake('baker5', BAKE_ARGS)
        new_balance_source = client.get_mutez_balance('bootstrap2')
        new_balance_target = client.get_mutez_balance('baker1')
        fee = 0.000455
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance_source - fee_mutez - amount_mutez == new_balance_source
        assert balance_target + amount_mutez == new_balance_target
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker1_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[0]['hash'],
            }
        ]

    def test_transfer_from_baker_key(self, client: Client):
        balance_source = client.get_mutez_balance('baker1')
        balance_target = client.get_mutez_balance('bootstrap2')
        amount = 10.001
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'baker1_key', 'bootstrap2')
        client.bake('baker5', BAKE_ARGS)
        new_balance_source = client.get_mutez_balance('baker1')
        new_balance_target = client.get_mutez_balance('bootstrap2')
        fee = 0.000404
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance_source - fee_mutez - amount_mutez == new_balance_source
        assert balance_target + amount_mutez == new_balance_target
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker1_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[0]['hash'],
            }
        ]

    def test_transfer_to_and_from_baker_key(self, client: Client):
        balance_source = client.get_mutez_balance('baker1')
        balance_target = client.get_mutez_balance('baker2')
        amount = 10.001
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'baker1_key', 'baker2_key')
        client.bake('baker5', BAKE_ARGS)
        new_balance_source = client.get_mutez_balance('baker1')
        new_balance_target = client.get_mutez_balance('baker2')
        fee = 0.000455
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance_source - fee_mutez - amount_mutez == new_balance_source
        assert balance_target + amount_mutez == new_balance_target
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker1_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[0]['hash'],
            },
            {
                "consensus_key": IDENTITIES['baker2_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[1]['hash'],
            },
        ]

    def test_originate_contract_from_baker_key(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'opcodes', 'noop.tz')
        client.originate(
            'noop1', 1000, 'baker1_key', contract, ['--burn-cap', '0.295']
        )
        client.bake('baker5', BAKE_ARGS)
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker1_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[0]['hash'],
            }
        ]

    def test_originate_contract_with_baker_key_delegate(self, client: Client):
        contract = path.join(CONTRACT_PATH, 'opcodes', 'noop.tz')
        client.originate(
            'noop2',
            1000,
            'bootstrap1',
            contract,
            ['--burn-cap', '0.295', '--delegate', 'baker3_key'],
        )
        client.bake('baker5', BAKE_ARGS)
        delegate = BOOTSTRAP_BAKERS[2]['hash']
        assert client.get_delegate('noop2', []).delegate == delegate
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker3_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[2]['hash'],
            }
        ]

    def test_register_baker_from_baker_key(self, client: Client):
        new_baker_key = 'baker-0-key'
        client.gen_key(new_baker_key)
        client.register_baker(
            'new-baker-0',
            10000,
            'baker3_key',
            consensus_key=new_baker_key,
            owner_keys=[new_baker_key],
        )
        client.bake('baker5', BAKE_ARGS)
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/3/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker3_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[2]['hash'],
            }
        ]

    def test_submit_proposal_from_baker_key(self, client: Client):
        cmd = [
            '-l',
            'submit',
            'proposals',
            'for',
            'baker3_key',
            'PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb',
        ]
        client_output.SubmitProposalsResult(client.run(cmd))
        client.bake('baker5', BAKE_ARGS)
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/1/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker3_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[2]['hash'],
            }
        ]

    def test_submit_ballot_from_baker_key(self, client: Client):
        # submit proposal first
        for key in ['baker1_key', 'baker2_key', 'baker3_key']:
            cmd = [
                'submit',
                'proposals',
                'for',
                key,
                'PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb',
            ]
            client_output.SubmitProposalsResult(client.run(cmd))
        # bake until testing vote period
        utils.bake_until_cycle_end(
            client, 'baker5', BLOCKS_PER_CYCLE, BAKE_ARGS
        )
        cmd = [
            'submit',
            'ballot',
            'for',
            'baker3_key',
            'PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb',
            'yay',
        ]
        client.run(cmd)
        client.bake('baker5', BAKE_ARGS)
        metadata = client.rpc('get', '/chains/main/blocks/head/operations')
        metadata = client.rpc('get', '/chains/main/blocks/head/operations/1/0')
        assert metadata['mapped_keys'] == [
            {
                "consensus_key": IDENTITIES['baker3_key']['identity'],
                "baker": BOOTSTRAP_BAKERS[2]['hash'],
            }
        ]
