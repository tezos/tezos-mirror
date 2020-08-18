import pytest

from client.client import Client
from tools import constants, utils
from tools.constants import IDENTITIES
from . import protocol

BAKE_ARGS = ['--minimal-timestamp']
BLOCKS_PER_CYCLE = 4
PROTO_A = "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
PROTO_B = "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
BAKER = 'baker3'
KEY0 = 'key_0'
KEY1 = 'key_1'
KEY2 = 'key_2'
SRC = 'bootstrap1'


@pytest.fixture(scope="class")
def client(sandbox):
    """One node, 4 blocks per voting period and cycle for fast voting
    progression."""
    parameters = protocol.PARAMETERS
    parameters["time_between_blocks"] = ["1", "0"]
    parameters["blocks_per_voting_period"] = 4
    parameters["blocks_per_cycle"] = BLOCKS_PER_CYCLE
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    utils.activate_alpha(sandbox.client(0), parameters)
    yield sandbox.client(0)


@pytest.mark.contract
@pytest.mark.baker
@pytest.mark.incremental
class TestBakerMultisig:
    """Test baker contract multisig commands"""

    def test_baker_multisig_init(self, client: Client):
        # Setup a baker with 3 owner keys and threshold set to 2
        client.gen_key(KEY0)
        client.gen_key(KEY1)
        client.gen_key(KEY2)
        client.set_baker_threshold_and_owner_keys(
            BAKER, 2, [KEY0, KEY1, KEY2], burn_cap='0.076'
        )
        client.bake('baker1', BAKE_ARGS)
        # For all actions, prepare, sign & call the transactions

    def test_transfer(self, client: Client):
        prep_transfer = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'transferring',
            '10',
            'to',
            'bootstrap1',
        ]
        client.run(prep_transfer)
        sign_transfer = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'transferring',
            '10',
            'to',
            'bootstrap1',
            'with',
            'key',
        ]
        sig0 = client.run(sign_transfer + [KEY0])[:-1]
        sig1 = client.run(sign_transfer + [KEY1])[:-1]
        call_transfer = [
            'from',
            'multisig',
            'baker',
            BAKER,
            'transfer',
            '10',
            'to',
            'bootstrap1',
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
        ]
        client.run(call_transfer)
        client.bake('baker1', BAKE_ARGS)

    def test_submit_proposals(self, client: Client):
        prep_proposal = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'submitting',
            'proposals',
            PROTO_A,
            PROTO_B,
        ]
        client.run(prep_proposal)
        sign_proposal = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'submitting',
            'proposals',
            PROTO_A,
            PROTO_B,
            'with',
            'key',
        ]
        sig0 = client.run(sign_proposal + [KEY0])[:-1]
        sig1 = client.run(sign_proposal + [KEY2])[:-1]
        call_proposal = [
            'from',
            'multisig',
            'baker',
            BAKER,
            'submit',
            'proposals',
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
            'for',
            'protocols',
            PROTO_A,
            PROTO_B,
        ]
        client.run(call_proposal)
        client.bake('baker1', BAKE_ARGS)

    def test_submit_ballot(self, client: Client):
        # Make PROTO_A the winning protocol and bake until testing vote period
        # to be able to submit a ballot
        client.submit_proposals('baker1', [PROTO_A])
        utils.bake_until_cycle_end(
            client, 'baker5', BLOCKS_PER_CYCLE, BAKE_ARGS
        )

        prep_ballot = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'submitting',
            'ballot',
            PROTO_A,
            'yay',
        ]
        client.run(prep_ballot)
        sign_ballot = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'submitting',
            'ballot',
            PROTO_A,
            'yay',
            'with',
            'key',
        ]
        sig0 = client.run(sign_ballot + [KEY0])[:-1]
        sig1 = client.run(sign_ballot + [KEY2])[:-1]
        call_ballot = [
            'from',
            'multisig',
            'baker',
            BAKER,
            'submit',
            'ballot',
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
            'for',
            'protocol',
            PROTO_A,
            'yay',
        ]
        client.run(call_ballot)
        client.bake('baker1', BAKE_ARGS)

    def test_deactivate_baker(self, client: Client):
        prep_deactivate = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'it',
            'inactive',
        ]
        client.run(prep_deactivate)
        sign_deactivate = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'it',
            'inactive',
            'with',
            'key',
        ]
        sig0 = client.run(sign_deactivate + [KEY0])[:-1]
        sig1 = client.run(sign_deactivate + [KEY1])[:-1]
        call_deactivate = [
            'set',
            'multisig',
            'baker',
            BAKER,
            'inactive',
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
        ]
        client.run(call_deactivate)
        client.bake('baker1', BAKE_ARGS)

    def test_set_declining_delegation(self, client: Client):
        prep_decline_delegations = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'it',
            'declining',
            'new',
            'delegations',
        ]
        client.run(prep_decline_delegations)
        sign_decline_delegations = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'it',
            'declining',
            'new',
            'delegations',
            'with',
            'key',
        ]
        sig0 = client.run(sign_decline_delegations + [KEY0])[:-1]
        sig1 = client.run(sign_decline_delegations + [KEY1])[:-1]
        sig2 = client.run(sign_decline_delegations + [KEY2])[:-1]
        call_decline_delegations = [
            'set',
            'multisig',
            'baker',
            BAKER,
            'declining',
            'new',
            'delegations',
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
            sig2,
        ]
        client.run(call_decline_delegations)
        client.bake('baker1', BAKE_ARGS)

    def test_set_consensus_key(self, client: Client):
        new_key = 'new_consensus_key'
        client.gen_key(new_key)
        prep_set_consensus_key = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'consensus',
            'key',
            'to',
            new_key,
        ]
        client.run(prep_set_consensus_key)
        sign_set_consensus_key = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'consensus',
            'key',
            'to',
            new_key,
            'with',
            'key',
        ]
        sig0 = client.run(sign_set_consensus_key + [KEY1])[:-1]
        sig1 = client.run(sign_set_consensus_key + [KEY0])[:-1]
        call_set_consensus_key = [
            'set',
            'multisig',
            'baker',
            BAKER,
            'consensus',
            'key',
            'to',
            new_key,
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
        ]
        client.run(call_set_consensus_key)
        client.bake('baker1', BAKE_ARGS)

    def test_generic_call(self, client: Client):
        # generic lambda type is:
        # `lambda unit (pair (list operation) (list baker_operation))`
        param = (
            '{ DROP ; NIL baker_operation ; NIL operation ; '
            f'PUSH key_hash "{IDENTITIES["bootstrap1"]["identity"]}" ; '
            'IMPLICIT_ACCOUNT ; PUSH mutez 10000 ; UNIT ; '
            'TRANSFER_TOKENS ; CONS ; PAIR }'
        )
        prep_generic_call = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'calling',
            'generic',
            'lambda',
            param,
        ]
        client.run(prep_generic_call)
        sign_generic_call = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'calling',
            'generic',
            'lambda',
            param,
            'with',
            'key',
        ]
        sig0 = client.run(sign_generic_call + [KEY0])[:-1]
        sig1 = client.run(sign_generic_call + [KEY1])[:-1]
        call_generic_call = [
            'call',
            'generic',
            'multisig',
            'baker',
            BAKER,
            'with',
            'lambda',
            param,
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
        ]
        client.run(call_generic_call)
        client.bake('baker1', BAKE_ARGS)

    def test_set_owner_keys(self, client: Client):
        new_key0 = 'new_owner_key_0'
        new_key1 = 'new_owner_key_1'
        threshold = str(2)
        client.gen_key(new_key0)
        client.gen_key(new_key1)
        prep_set_owner_keys = [
            'prepare',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'threshold',
            'to',
            threshold,
            'and',
            'owner',
            'keys',
            'to',
            new_key0,
            new_key1,
        ]
        client.run(prep_set_owner_keys)
        sign_set_owner_keys = [
            'sign',
            'baker',
            'transaction',
            'on',
            BAKER,
            'setting',
            'threshold',
            'to',
            threshold,
            'and',
            'owner',
            'keys',
            'to',
            new_key0,
            new_key1,
            'with',
            'key',
        ]
        sig0 = client.run(sign_set_owner_keys + [KEY1])[:-1]
        sig1 = client.run(sign_set_owner_keys + [KEY2])[:-1]
        call_set_owner_keys = [
            'set',
            'multisig',
            'baker',
            BAKER,
            'threshold',
            'to',
            threshold,
            'and',
            'owner',
            'keys',
            'to',
            new_key0,
            new_key1,
            'on',
            'behalf',
            'of',
            SRC,
            'with',
            'signatures',
            sig0,
            sig1,
        ]
        client.run(call_set_owner_keys)
        client.bake('baker1', BAKE_ARGS)
