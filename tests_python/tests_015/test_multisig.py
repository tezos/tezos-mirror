# tezos-client has builtin support for multisig smart contracts. See
# docs/user/multisig.rst for more details about it.

# This file tests the client multisig support; more precisely it tests
# that both the generic and the legacy versions of the multisig smart
# contract behave as intended. For all commands, we check that we can
# interact with the multisig contract when invoking it by its address
# or by its alias.

import os
import re
from typing import List
import pytest
from tools import utils, constants
from client.client import Client
from .contract_paths import MINI_SCENARIOS_CONTRACT_PATH, ATTIC_CONTRACT_PATH


def get_keys(client):
    """Generate 3 pairs of keys using various schemes and return the list
    of aliases."""
    keys = ['foo', 'bar', 'boo']
    sigs = [None, 'secp256k1', 'ed25519']
    for key, sig in zip(keys, sigs):
        args = [] if sig is None else ['--sig', sig]
        client.gen_key(key, args)
    return keys


@pytest.fixture(scope="class")
def keys(client):
    return get_keys(client)


def msig_path(msig_version: str) -> str:
    return os.path.join(
        MINI_SCENARIOS_CONTRACT_PATH, f'{msig_version}_multisig.tz'
    )


MSIG_PARAMS = [
    {'by_address': by_address, 'msig_version': msig_version}
    for msig_version in ['generic', 'legacy']
    for by_address in [True, False]
]


def parse_msig_storage(storage: str):
    """Parse the storage of a multisig contract to get its counter (as a
    number), threshold (as a number), and the keys of the signers (as
    Micheline sequence in a string)."""
    # put everything on a single line
    storage = ' '.join(storage.split('\n'))
    storage_regexp = r'Pair\s+?([0-9]+)\s+?([0-9]+)\s+?(.*)\s*'
    match = re.search(storage_regexp, storage)
    assert match is not None
    return {
        'counter': int(match[1]),
        'threshold': int(match[2]),
        'keys': match[3],
    }


def resolve_key_alias(client: Client, alias: str) -> str:
    """Convert a key alias into a public key that can be understood in
    Michelson."""
    ret = client.show_address(alias).public_key
    assert ret is not None
    return ret


def build_michelson_key_list(client: Client, keys: List[str]):
    """From a list of key aliases, build a Michelson list of public keys."""
    keys = [resolve_key_alias(client, k) for k in keys]
    return '{"' + '"; "'.join(keys) + '"}'


def build_msig_storage(
    client: Client, counter: int, threshold: int, keys: List[str]
):
    """Build a multisig storage from its components: a counter, a
    threshold and the list of signer public keys."""
    keys = build_michelson_key_list(client, keys)
    return f'Pair {counter} {threshold} {keys}'


def assert_michelson_eq(client, data1, data2, typ):
    """Check that two Michelson expressions of the same type are equal by
    normalizing them."""
    normalized1 = client.normalize(data=data1, typ=typ)
    normalized2 = client.normalize(data=data2, typ=typ)
    assert normalized1 == normalized2


def assert_msig_storage_eq(client, data1, data2):
    """Check that two multisig storages are equal."""
    assert_michelson_eq(client, data1, data2, 'pair nat nat (list key)')


def assert_msig_counter_incr(current_storage, new_storage):
    """Check that [new_storage] is the same multisig storage than
    [current_storage] except that it uses the next counter."""
    current_storage = parse_msig_storage(current_storage)
    new_storage = parse_msig_storage(new_storage)
    assert new_storage['counter'] == 1 + current_storage['counter']
    assert new_storage['threshold'] == current_storage['threshold']
    assert new_storage['keys'] == current_storage['keys']


@pytest.fixture(scope="class", params=MSIG_PARAMS)
def msig(client: Client, keys, request):
    """This fixture originates a multisig contract with a threshold of 2
    and the keys given as parameter. The version of the script is given by
    the msig_version parameter, it can be either 'generic' or
    'legacy'. This fixture returns a dictionary containing:

        - a [handle] that can be used to interact with the contract:
          either the address of the script or an alias, depending on the
          [by_address] parameter

        - the list of [keys] that are stored in the contract which is a
          copy of the [keys] parameter

        - the [version], which is a copy of the [msig_version] parameter

    """

    # We use the same alias for all multisig originations, this makes
    # testing simpler but requires using the '--force' option.
    msig_alias = 'msig'
    msig_version = request.param['msig_version']
    by_address = request.param['by_address']
    initial_storage = build_msig_storage(
        client=client, counter=0, threshold=2, keys=keys
    )
    deployment = client.originate(
        msig_alias,
        100,
        'bootstrap1',
        msig_path(msig_version),
        # Initialize with empty key list and null threshold
        ['--init', initial_storage, '--burn-cap', '100', '--force'],
    )
    utils.bake(client)
    handle = deployment.contract if by_address else msig_alias
    return {'handle': handle, 'keys': keys, 'version': msig_version}


@pytest.mark.incremental
class TestMultisig:
    def test_deploy_multisig(self, msig, client: Client):
        """Test that:
        - the script originated by the "deploy multisig" command,
        - the generic_multisig.tz script found in the mini_scenarios
          directory, and
        - the script printed by the "show multisig script"
        are the same."""
        keys = msig['keys']

        # The command cannot originate the legacy contract so there is
        # nothing to test in the legacy case.
        if msig['version'] == 'generic':
            client.deploy_msig(
                'dummy_msig',
                100,
                'bootstrap1',
                2,
                keys,
                ['--burn-cap', '100', '--force'],
            )
            utils.bake(client)
            expected_hash = (
                'exprub9UzpxmhedNQnsv1J1DazWGJnj1dLhtG1fxkUoWSdFLBGLqJ4'
            )
            assert expected_hash in client.run(
                ['show', 'supported', 'multisig', 'hashes']
            )
            assert client.get_script_hash(msig['handle']) == expected_hash
            assert client.get_script_hash('dummy_msig') == expected_hash
            assert client.hash_script(
                [client.run(['show', 'multisig', 'script'])]
            ) == [(expected_hash, None)]
            assert client.get_balance('dummy_msig') == 100

    def test_transfer(self, msig, client: Client, session: dict):
        """Test the client command for signing a multisig transfer from key
        number 0 and store the signature in the session."""
        keys = msig['keys']
        key = keys[0]
        session['sig0'] = client.msig_sign_transfer(
            msig['handle'], 10, 'bootstrap2', key
        )

    def test_prepare_msig_transfer(self, msig, client: Client):
        """Test the client command for preparing a transfer. The result of the
        command is ignored in this test, we only test that the command
        succeeds."""
        client.msig_prepare_transfer(msig['handle'], 10, 'bootstrap2')

    def test_prepare_msig_sign(self, msig, client: Client, session: dict):
        """Produce signatures for keys number 1 and 2 using the the
        preparation command together with the sign_bytes client command. The
        signatures are stored in the session."""
        to_sign = client.msig_prepare_transfer(
            msig['handle'], 10, 'bootstrap2', ['--bytes-only']
        )
        session['sig1'] = client.sign_bytes_of_string(to_sign, msig['keys'][1])
        session['sig2'] = client.sign_bytes_of_string(to_sign, msig['keys'][2])

    def test_transfer_failure(self, msig, client: Client, session: dict):
        """Test transfer failure when there are too few signatures."""
        error_pattern = (
            r"Not enough signatures: "
            + r"only 1 signatures were given "
            + r"but the threshold is currently 2"
        )

        with utils.assert_run_failure(error_pattern):
            client.msig_transfer(
                msig['handle'],
                10,
                'bootstrap2',
                'bootstrap1',
                [session['sig2']],
            )

    def test_transfer_success(self, msig, client: Client, session: dict):
        """Test a successful transfer using signatures obtained by different
        methods. The signatures are taken from the session."""
        current_storage = client.get_storage(msig['handle'])
        current_balance = client.get_balance(msig['handle'])

        client.msig_transfer(
            msig['handle'],
            10,
            'bootstrap2',
            'bootstrap1',
            [session['sig0'], session['sig2']],
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        assert_msig_counter_incr(current_storage, new_storage)
        new_balance = client.get_balance(msig['handle'])
        assert new_balance == current_balance - 10

    def test_default_entrypoint(self, msig, client):
        """The generic multisig contract features an unauthorized default
        entrypoint to receive donations but the legacy one does not."""

        def cmd():
            client.transfer(
                amount=100, giver='bootstrap1', receiver=msig['handle']
            )

        if msig['version'] == 'legacy':
            error_pattern = r'Invalid argument passed to contract'
            with utils.assert_run_failure(error_pattern):
                cmd()
        else:
            current_storage = client.get_storage(msig['handle'])
            current_balance = client.get_balance(msig['handle'])
            cmd()
            utils.bake(client)
            new_storage = client.get_storage(msig['handle'])
            new_balance = client.get_balance(msig['handle'])
            assert new_storage == current_storage
            assert new_balance == current_balance + 100

    def test_transfer_with_entrypoint(self, msig, client: Client):
        """Both versions of the contract can call arbitrary entrypoints of
        type unit. This test uses the two possible methods to produce the
        signatures."""
        current_storage = client.get_storage(msig['handle'])
        current_balance = client.get_balance(msig['handle'])
        contract = (
            'parameter (or (unit %a) (string %b)); '
            'storage unit; '
            'code {CDR; NIL operation; PAIR}'
        )
        client.originate(
            'dest_entrypoint',
            0,
            'bootstrap1',
            contract,
            args=['--burn-cap', '10.0', '--force'],
        )
        args = ['--entrypoint', 'a']
        utils.bake(client)
        to_sign = client.msig_prepare_transfer(
            msig_name=msig['handle'],
            amount=10,
            dest='dest_entrypoint',
            args=args + ['--bytes-only'],
        )
        sig0 = client.sign_bytes_of_string(to_sign, msig['keys'][0])
        sig2 = client.msig_sign_transfer(
            msig_name=msig['handle'],
            amount=10,
            dest='dest_entrypoint',
            secret_key=msig['keys'][2],
            args=args,
        )
        client.msig_transfer(
            msig_name=msig['handle'],
            amount=10,
            dest='dest_entrypoint',
            src='bootstrap1',
            signatures=[sig0, sig2],
            args=args,
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        new_balance = client.get_balance(msig['handle'])
        assert_msig_counter_incr(current_storage, new_storage)
        assert new_balance == current_balance - 10

    def test_transfer_with_arg(self, msig, client: Client):
        """The generic multisig contract can call other contracts with
        arbitrary parameters but the legacy one can only send Unit."""
        contract = (
            'parameter (or (int %a) (string %b)); '
            'storage unit; '
            'code {CDR; NIL operation; PAIR}'
        )
        client.originate(
            'dest',
            0,
            'bootstrap1',
            contract,
            args=['--burn-cap', '10.0', '--force'],
        )
        args = ['--entrypoint', 'a', '--arg', '42']
        utils.bake(client)

        def cmd():
            return client.msig_prepare_transfer(
                msig_name=msig['handle'],
                amount=10,
                dest='dest',
                args=args + ['--bytes-only'],
            )

        if msig['version'] == 'legacy':
            error_pattern = (
                r'This multisig contract can only transfer tokens to'
                ' contracts of type unit; calling a contract with argument 42'
                ' is not supported.'
            )
            with utils.assert_run_failure(error_pattern):
                cmd()
        else:
            current_storage = client.get_storage(msig['handle'])
            current_balance = client.get_balance(msig['handle'])
            to_sign = cmd()
            utils.bake(client)
            sig0 = client.sign_bytes_of_string(to_sign, msig['keys'][0])
            sig2 = client.msig_sign_transfer(
                msig_name=msig['handle'],
                amount=10,
                dest='dest',
                secret_key=msig['keys'][2],
                args=args,
            )
            client.msig_transfer(
                msig_name=msig['handle'],
                amount=10,
                dest='dest',
                src='bootstrap1',
                signatures=[sig0, sig2],
                args=args,
            )
            utils.bake(client)
            new_storage = client.get_storage(msig['handle'])
            new_balance = client.get_balance(msig['handle'])
            assert_msig_counter_incr(current_storage, new_storage)
            assert new_balance == current_balance - 10

    def test_transfer_ill_typed(self, msig, client: Client):
        """Test that the multisig transfer preparation command type checks the
        parameter."""
        error_pattern = (
            (
                r'The entrypoint b of contract .* '
                'called from a multisig contract is of type string; '
                'the provided parameter 42 is ill-typed.'
            )
            if msig['version'] == 'generic'
            else (
                r'This multisig contract can only transfer tokens to'
                ' contracts of type unit; calling a contract with argument 42'
                ' is not supported.'
            )
        )

        args = ['--entrypoint', 'b', '--arg', '42']

        def cmd():
            client.msig_prepare_transfer(
                msig_name=msig['handle'],
                amount=10,
                dest='dest',
                args=args + ['--bytes-only'],
            )

        with utils.assert_run_failure(error_pattern):
            cmd()

    def test_transfer_too_high(self, msig, client: Client):
        """Test that the multisig transfer preparation command checks the
        balance."""
        expected_warning = (
            'Transferred amount is bigger than current multisig balance'
        )

        client.msig_prepare_transfer(
            msig_name=msig['handle'],
            amount=1000,
            dest='bootstrap1',
            args=['--bytes-only'],
            expected_warning=expected_warning,
        )

    def test_multiple_operations(self, msig, client: Client):
        """The generic multisig client can run lambdas, this can be used to
        atomically run several operations."""
        bootstrap1_address = constants.IDENTITIES['bootstrap1']['identity']
        bootstrap2_address = constants.IDENTITIES['bootstrap2']['identity']
        bootstrap3_address = constants.IDENTITIES['bootstrap3']['identity']
        lam = (
            '{ DROP; NIL operation; '
            f'PUSH key_hash "{bootstrap1_address}"; IMPLICIT_ACCOUNT; '
            'PUSH mutez 1000000; UNIT; TRANSFER_TOKENS; CONS; '
            f'PUSH key_hash "{bootstrap2_address}"; IMPLICIT_ACCOUNT; '
            'PUSH mutez 2000000; UNIT; TRANSFER_TOKENS; CONS; '
            f'PUSH key_hash "{bootstrap3_address}"; SOME; '
            'SET_DELEGATE; CONS}'
        )
        lam = client.normalize(
            lam, typ='lambda unit (list operation)', mode='Optimized'
        )

        def cmd():
            return client.msig_prepare_lambda(
                msig_name=msig['handle'], lam=lam, args=['--bytes-only']
            )

        if msig['version'] == 'legacy':
            error_pattern = 'This multisig contract has a fixed set of actions'
            with utils.assert_run_failure(error_pattern):
                cmd()
        else:
            current_storage = client.get_storage(msig['handle'])
            current_balance = client.get_balance(msig['handle'])
            to_sign = cmd()
            sig0 = client.sign_bytes_of_string(to_sign, msig['keys'][0])
            sig2 = client.msig_sign_lambda(
                msig_name=msig['handle'], lam=lam, secret_key=msig['keys'][2]
            )
            client.msig_run_lambda(
                msig_name=msig['handle'],
                lam=lam,
                src='bootstrap1',
                signatures=[sig0, sig2],
            )
            utils.bake(client)
            new_storage = client.get_storage(msig['handle'])
            new_balance = client.get_balance(msig['handle'])
            assert_msig_counter_incr(current_storage, new_storage)
            assert new_balance == current_balance - 3
            # TODO: check the delegate change

    def test_multiple_operations_failure(self, msig, client: Client):
        """Test for the error message for ill-typed lambdas."""
        lam = '{ DROP }'

        def cmd():
            client.msig_prepare_lambda(
                msig_name=msig['handle'], lam=lam, args=['--bytes-only']
            )

        error_pattern = (
            (
                r'The provided lambda .* for multisig contract'
                r' is ill-typed; .* is expected.'
            )
            if msig['version'] == 'generic'
            else 'This multisig contract has a fixed set of actions'
        )

        with utils.assert_run_failure(error_pattern):
            cmd()

    def test_delegate_change(self, msig, client: Client):
        """Test the multisig command for changing delegate."""
        current_storage = client.get_storage(msig['handle'])
        current_balance = client.get_balance(msig['handle'])
        sig0 = client.msig_sign_set_delegate(
            msig['handle'], 'bootstrap5', msig['keys'][0]
        )
        to_sign = client.msig_prepare_set_delegate(
            msig['handle'], 'bootstrap5', ['--bytes-only']
        )
        sig2 = client.sign_bytes_of_string(to_sign, msig['keys'][2])
        client.msig_set_delegate(
            msig['handle'], 'bootstrap5', 'bootstrap1', [sig0, sig2]
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        new_balance = client.get_balance(msig['handle'])
        assert_msig_counter_incr(current_storage, new_storage)
        assert new_balance == current_balance

    def test_delegate_withdraw(self, msig, client: Client):
        """Test the multisig command for removing delegation."""
        current_storage = client.get_storage(msig['handle'])
        current_balance = client.get_balance(msig['handle'])
        sig0 = client.msig_sign_withdrawing_delegate(
            msig['handle'], msig['keys'][0]
        )
        to_sign = client.msig_prepare_withdrawing_delegate(
            msig['handle'], ['--bytes-only']
        )

        sig1 = client.sign_bytes_of_string(to_sign, msig['keys'][1])
        client.msig_withdrawing_delegate(
            msig['handle'], 'bootstrap1', [sig0, sig1]
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        new_balance = client.get_balance(msig['handle'])
        assert_msig_counter_incr(current_storage, new_storage)
        assert new_balance == current_balance

    def test_run_transaction_change_keys_and_threshold(
        self, msig, client: Client
    ):
        """Test changing the keys and threshold with the `run transaction`
        command."""
        current_storage = client.get_storage(msig['handle'])
        current_counter = parse_msig_storage(storage=current_storage)['counter']
        current_balance = client.get_balance(msig['handle'])
        keys = msig['keys']
        sig0 = client.msig_sign_setting_threshold(
            msig['handle'], keys[0], 2, [keys[0], keys[2]]
        )
        to_sign = client.msig_prepare_setting_threshold(
            msig['handle'], 2, [keys[0], keys[2]], ['--bytes-only']
        )
        sig2 = client.sign_bytes_of_string(to_sign, msig['keys'][2])
        client.msig_run_transaction(
            msig['handle'], to_sign, 'bootstrap1', [sig0, sig2]
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        expected_counter = 1 + current_counter
        expected_storage = build_msig_storage(
            client=client,
            counter=expected_counter,
            threshold=2,
            keys=[keys[0], keys[2]],
        )
        assert_msig_storage_eq(client, new_storage, expected_storage)
        new_balance = client.get_balance(msig['handle'])
        assert new_balance == current_balance

    def test_change_keys_and_threshold(self, msig, client: Client):
        """Test changing the keys and threshold with `set threshold of
        multisig` command."""
        current_storage = client.get_storage(msig['handle'])
        current_counter = parse_msig_storage(storage=current_storage)['counter']
        current_balance = client.get_balance(msig['handle'])
        keys = msig['keys']
        new_keys = [keys[0], keys[2]]
        sig0 = client.msig_sign_setting_threshold(
            msig['handle'], keys[0], 2, new_keys
        )
        to_sign = client.msig_prepare_setting_threshold(
            msig['handle'], 2, new_keys, ['--bytes-only']
        )
        sig2 = client.sign_bytes_of_string(to_sign, msig['keys'][2])
        client.msig_set_threshold(
            msig['handle'], 2, new_keys, 'bootstrap1', [sig0, sig2]
        )
        utils.bake(client)
        new_storage = client.get_storage(msig['handle'])
        expected_counter = 1 + current_counter
        expected_storage = build_msig_storage(
            client=client,
            counter=expected_counter,
            threshold=2,
            keys=[keys[0], keys[2]],
        )
        assert_msig_storage_eq(client, new_storage, expected_storage)
        new_balance = client.get_balance(msig['handle'])
        assert new_balance == current_balance


class TestUnsupportedMultisig:
    """Verify that non-multisig contracts are rejected"""

    def test_deploy_nonmultisig(self, client: Client):
        contract = os.path.join(ATTIC_CONTRACT_PATH, 'id.tz')
        client.originate(
            'id',
            0,
            'bootstrap1',
            contract,
            args=['--burn-cap', '10.0', '--force', '--init', '""'],
        )
        utils.bake(client)

        error_pattern = (
            'The hash of this script is '
            'exprv8K6ceBpFH5SFjQm4BRYSLJCHQBFeQU6BFTdvQSRPaPkzdLyAL, '
            'it was not found among in the list of known multisig '
            'script hashes.'
        )

        with utils.assert_run_failure(error_pattern):
            client.msig_transfer('id', 10, 'bootstrap2', 'bootstrap1', [])
