import subprocess
import pytest
from tools import paths, constants, utils
from launchers.sandbox import Sandbox

BOOTSTRAP_ACCOUNTS = [
    ["edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
     "400000000000"],  # 500 rolls
    ["edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
     "16000000000000"]   # 2000 rolls
]

GENESIS_SK = "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"
GENESIS_PK = "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2"

IDENTITIES = {
    'bootstrap1': {
        'identity': "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        'public': "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        'secret': ("unencrypted:"
                   "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh")
    },
    'bootstrap2': {
        'identity': "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        'public': "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
        'secret': ("unencrypted:"
                   "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo")
    },
    'baker1_key': {
        'public': 'edpkuDjETsuw7Tve4hczaEiDd5q8q5CnzppnoENpvf3hSt2g2N6fy5',
        'secret': ('unencrypted:'
                   'edsk432L71B91i1sE8rQxPDMo2Yxo4qaYqhktvpt8yovaMpo1NUbBt')
    },
    'activator': {
        'secret': "unencrypted:" + GENESIS_SK
    }
}

BAKER = {
    "hash": "SG1fpFaowYY8G7PfkYdKkGmsMziHKUfrHRHW",
    "amount": "400000000000",
    'key': IDENTITIES['baker1_key']['public']
}

BOOTSTRAP_BAKERS = [BAKER]

BAKE_ARGS = ['--minimal-fees', '0', '--minimal-nanotez-per-byte', '0',
             '--minimal-nanotez-per-gas-unit', '0', '--max-priority', '512',
             '--minimal-timestamp']


PARAMETERS = dict(constants.PARAMETERS)
PARAMETERS["time_between_blocks"] = ["1", "0"]
PARAMETERS["blocks_per_voting_period"] = 4
PARAMETERS["bootstrap_accounts"] = BOOTSTRAP_ACCOUNTS
PARAMETERS["bootstrap_bakers"] = BOOTSTRAP_BAKERS

PROTOGENESIS = 'ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im'


@pytest.fixture(scope="session")
def singleprocess(request):
    """Retrieve user-provided single process mode on the command line."""
    yield request.config.getoption("--singleprocess")


DEAD_DAEMONS_WARN = '''
It seems some daemons terminated unexpectedly, or didn't launch properly.
You can investigate daemon logs by running this test using the
`--log-dir=LOG_DIR` option.'''


@pytest.fixture(scope="class")
def sandbox(log_dir):
    with Sandbox(paths.TEZOS_HOME,
                 IDENTITIES,
                 log_dir=log_dir,
                 singleprocess=singleprocess) as sandbox:
        yield sandbox
        assert sandbox.are_daemons_alive(), DEAD_DAEMONS_WARN


@pytest.fixture(scope="class")
def client(sandbox):
    """One snode, 4 blocks per voting period."""
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    utils.activate_alpha(sandbox.client(0), PARAMETERS)
    yield sandbox.client(0)


def bake_until_period_position(client, position, baker, args):
    period_position = client.get_period_position()
    blocks_to_bake = PARAMETERS["blocks_per_voting_period"] - period_position
    client.bake_many(blocks_to_bake, baker, args)
    assert client.get_period_position() == position


# set period position to 0
def initialize(client):
    client.set_delegate('bootstrap1', 'baker1')
    client.set_delegate('bootstrap2', 'baker1')
    bake_until_period_position(client, 0, 'baker1', BAKE_ARGS)


PROTO = (f'{paths.TEZOS_HOME}/src/'
         f'bin_client/test/proto_test_injection')
COMPILER = (f'{paths.TEZOS_HOME}/_build/default/src/lib_protocol_compiler/'
            'main_native.exe')


@pytest.mark.vote
@pytest.mark.incremental
class TestVoteOverriding:

    def test_vote_override(self, client):

        initialize(client)

        # inject a protocol
        cmd = [COMPILER, '-hash-only', PROTO]
        res = subprocess.run(cmd,
                             universal_newlines=True,
                             check=True,
                             stdout=subprocess.PIPE)
        proto_hash = res.stdout[:-1]
        assert len(proto_hash) == 51
        client.inject_protocol(PROTO)
        proto_hashes = client.list_protocols()
        assert proto_hash in proto_hashes

        # submit a proposal
        assert client.get_proposals() == []
        client.submit_proposals('baker1', [proto_hash])
        client.bake('baker1', BAKE_ARGS)
        assert len(client.get_proposals()) == 1

        # voting
        bake_until_period_position(client, 0, 'baker1', BAKE_ARGS)
        assert client.get_period_position() == 0
        assert client.get_current_period_kind() == 'testing_vote'
        client.bake('baker1', BAKE_ARGS)
        client.submit_ballot(f'baker1', proto_hash, '100', '0', '0')
        client.override_ballot(f'bootstrap2', proto_hash, '100', '0', '0')

        # testing phase
        bake_until_period_position(client, 0, 'baker1', BAKE_ARGS)
        assert client.get_current_period_kind() == "testing"

        # promotion phase
        print(client.get_protocol())
        bake_until_period_position(client, 0, 'baker1', BAKE_ARGS)
        assert client.get_current_period_kind() == "promotion_vote"
        client.bake('baker1', BAKE_ARGS)
        client.submit_ballot(f'baker1', proto_hash, '0', '100', '0')
        client.override_ballot(f'bootstrap2', proto_hash, '100', '0', '0')

        # adoption phase
        bake_until_period_position(client, 0, 'baker1', BAKE_ARGS)
        assert client.get_current_period_kind() == "adoption"

        # activation
        client.bake('baker1', BAKE_ARGS)
        client.bake('baker1', BAKE_ARGS)
        client.bake('baker1', BAKE_ARGS)
        client.bake('baker1', BAKE_ARGS)
        params = ['-p', PROTOGENESIS]
        assert utils.check_protocol(client, proto_hash, params=params)
