import os
import re
from os import path

import pytest

from client.client import Client
from tools import utils
from tools.constants import BOOTSTRAP_BAKERS, IDENTITIES
from .contract_paths import CONTRACT_PATH, ILLTYPED_CONTRACT_PATH

BAKE_ARGS = ['--minimal-timestamp']
PROXY_SCRIPT = os.path.join(CONTRACT_PATH, 'baker-proxy.tz')
BAKER_2_CONSENSUS_KEY = BOOTSTRAP_BAKERS[1]['key']
BAKER_ACTIONS = [
    # transfer tokens
    (
        'Left { DROP ; NIL baker_operation ; NIL operation ;'
        'PUSH address "tz1ivoFEvbfbUNav5FwLvmxzMGcNXWxY9qTD" ;'
        'CONTRACT unit ; ASSERT_SOME ; PUSH mutez 1 ; UNIT ;'
        'TRANSFER_TOKENS ; CONS ; PAIR }',
        None,
    ),
    # submit proposals
    (
        'Left { DROP ; NIL baker_operation ;'
        'PUSH (list string) { '
        '"PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" } ;'
        'SUBMIT_PROPOSALS ; CONS ; NIL operation ; PAIR }',
        None,
    ),
    # submit ballot - the call is expected to fail because we're not in testing
    # vote or promotion vote period
    (
        'Left { DROP ; NIL baker_operation ;'
        'PUSH string "yay" ;'
        'PUSH string '
        '"PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" ;'
        'SUBMIT_BALLOT ; CONS ; NIL operation ; PAIR }',
        r'Unexpected ballot',
    ),
    # set _baker active
    (
        'Left { DROP ; NIL baker_operation ; PUSH bool False ;'
        'SET_BAKER_ACTIVE ; CONS ; NIL operation ; PAIR }',
        None,
    ),
    # set baker consensus key - assuming the given consensus key is unique
    (
        'Left { DROP ; NIL baker_operation ;'
        'PUSH key'
        '"edpktxxnBn5YqwF6aTJLz69AEt4L17QZhbCH5KhTxe1o2tv8nHKEoV" ;'
        'SET_BAKER_CONSENSUS_KEY ; CONS ; NIL operation ; PAIR }',
        None,
    ),
    # set baker consensus key - expected to fail, because the given consensus
    # key is already used
    (
        'Left { DROP ; NIL baker_operation ;'
        'PUSH key'
        f'"{BAKER_2_CONSENSUS_KEY}" ;'
        'SET_BAKER_CONSENSUS_KEY ; CONS ; NIL operation ; PAIR }',
        rf'The given baker consensus key {BAKER_2_CONSENSUS_KEY} is already '
        r'being used\. A unique consensus key must be used\.',
    ),
    # set baker PVSS key
    (
        'Left { DROP ; NIL baker_operation ;'
        'PUSH pvss_key'
        '"GSp8PUBkYJzkg9e3EXHYeWVcC8EPnrjLMRTJmkcQ1iiyxNSXTBtcW6" ;'
        'SET_BAKER_PVSS_KEY ; CONS ; NIL operation ; PAIR }',
        None,
    ),
    # set baker owner keys
    (
        'Right (Pair 1 { '
        '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" })',
        None,
    ),
]


@pytest.mark.contract
@pytest.mark.baker
@pytest.mark.incremental
class TestBaker:
    """Test baker contract registration, commands and helpers"""

    def test_register_baker(self, client: Client, session):
        baker_0_key = 'baker-0-key'
        client.gen_key(baker_0_key)
        address_0 = client.show_address(baker_0_key)
        session['address_0'] = address_0
        client.register_baker(
            'new-baker-0',
            10000,
            'bootstrap1',
            consensus_key=baker_0_key,
            owner_keys=[baker_0_key],
        )
        client.bake('baker5', BAKE_ARGS)

        with utils.assert_run_failure(
            rf'The given baker consensus key {address_0.public_key} is '
            r'already being used\. A unique consensus key must be used\.'
        ):
            client.register_baker(
                'new-baker-1',
                10000,
                'bootstrap1',
                consensus_key=baker_0_key,
                owner_keys=[baker_0_key],
            )

        with utils.assert_run_failure(
            r'Threshold too high: 2; at most 1 expected\.'
        ):
            baker_1_key = 'baker-1-key'
            client.gen_key(baker_1_key)
            client.register_baker(
                'new-baker-2',
                10000,
                'bootstrap1',
                consensus_key=baker_1_key,
                owner_keys=[baker_1_key],
                threshold=2,
            )

        with utils.assert_run_failure(
            r'Multisig threshold 0 should be positive\.'
        ):
            baker_1_key = 'baker-2-key'
            client.gen_key(baker_1_key)
            client.register_baker(
                'new-baker-3',
                10000,
                'bootstrap1',
                consensus_key=baker_1_key,
                owner_keys=[],
                threshold=0,
            )

    def test_set_consensus_key(self, client: Client, session):
        address_0 = session['address_0']
        with utils.assert_run_failure(
            rf'The given baker consensus key {address_0.public_key} is '
            r'already being used\. A unique consensus key must be used\.'
        ):
            client.set_baker_consensus_key('baker1', 'baker-0-key')

    def test_set_threshold_and_owner_keys(self, client: Client):
        with utils.assert_run_failure(
            r'Threshold too high: 2; at most 1 expected\.'
        ):
            client.set_baker_threshold_and_owner_keys(
                'baker1', 2, ['baker1_key']
            )

    def test_set_multisig_threshold_and_owner_keys(self, client: Client):
        baker = 'baker1'
        threshold = 2
        key = 'baker1_key'
        expected_failure = r'Threshold too high: 2; at most 1 expected\.'
        with utils.assert_run_failure(expected_failure):
            cmd = [
                'prepare',
                'baker',
                'transaction',
                'on',
                baker,
                'setting',
                'threshold',
                'to',
                str(threshold),
                'and',
                'owner',
                'keys',
                'to',
                key,
            ]
            client.run(cmd)

        with utils.assert_run_failure(expected_failure):
            cmd = [
                'sign',
                'baker',
                'transaction',
                'on',
                baker,
                'setting',
                'threshold',
                'to',
                str(threshold),
                'and',
                'owner',
                'keys',
                'to',
                key,
                'with',
                'key',
                'baker1_key',
            ]
            client.run(cmd)

        with utils.assert_run_failure(expected_failure):
            fake_signature = (
                'sigbQ5ZNvkjvGssJgoAnUAfY4Wvvg3QZqawBYB1j1VDBNTMBAALnCzRHWzer3'
                '4bnfmzgHg3EvwdzQKdxgSghB897cono6gbQ'
            )
            cmd = [
                'set',
                'multisig',
                'baker',
                baker,
                'threshold',
                'to',
                str(threshold),
                'and',
                'owner',
                'keys',
                'to',
                key,
                'on',
                'behalf',
                'of',
                'baker1',
                'with',
                'signatures',
                fake_signature,
            ]
            client.run(cmd)

    def test_transfer_to_a_pending_consensus_key_fails(self, client: Client):
        baker = 'baker2'
        new_consensus_key = 'baker-0-consensus-key'
        client.gen_key(new_consensus_key)
        new_consensus_key_address = client.show_address(new_consensus_key)
        client.set_baker_consensus_key(baker, new_consensus_key)
        client.bake('baker1', BAKE_ARGS)

        with utils.assert_run_failure(
            r'Forbidden transaction to a destination '
            f'{new_consensus_key_address.hash} that is being '
            'used as a pending consensus key.'
        ):
            client.transfer(
                10000, 'bootstrap1', new_consensus_key, ['--burn-cap', '0.257']
            )

    @pytest.mark.parametrize(
        "contract",
        [
            ("submit_proposals.tz"),
            ("submit_ballot.tz"),
            ("set_baker_active.tz"),
            ("set_baker_consensus_key.tz"),
            ("set_baker_pvss_key.tz"),
        ],
    )
    def test_ill_typed_originated_script_with_baker_operation(
        self, client: Client, contract
    ):
        path = os.path.join(ILLTYPED_CONTRACT_PATH, contract)

        error_pattern = (
            r"Type list baker_operation is not compatible with "
            "type list operation"
        )

        # Test that an originated script with baker operation fails typecheck
        with utils.assert_run_failure(error_pattern):
            client.typecheck(path)

        with utils.assert_run_failure(error_pattern):
            client.run_script(path, 'Unit', 'Unit')

        with utils.assert_run_failure(error_pattern):
            client.originate(
                'noop', 1000, 'bootstrap1', path, ['--burn-cap', '0.295']
            )

    def test_get_baker_contract_storage(self, client: Client):
        key = BOOTSTRAP_BAKERS[0]['key']
        storage = client.get_storage('baker1')
        assert storage == f'Pair 0 1 {{ "{key}" }}'

        client.gen_key('new_key')
        address = client.show_address('new_key', show_secret=True)
        new_key_pk = address.public_key
        client.set_baker_threshold_and_owner_keys(
            'baker1', 2, ['baker1_key', 'new_key']
        )
        client.bake('baker1', BAKE_ARGS)
        storage = client.get_storage('baker1')
        keys = [key, new_key_pk]
        pattern = fr'Pair 1\s+ 2\s+{{ \"{keys[0]}\" ;\s+\"{keys[1]}\" }}'
        if not re.fullmatch(pattern, storage):
            assert False, (
                f"unexpected storage, found:\n{storage}\n"
                f"expected pattern:\n{pattern}"
            )

    def test_originate_proxy(self, client: Client):
        client.originate(
            'proxy', 1000, 'bootstrap1', PROXY_SCRIPT, ['--burn-cap', '0.715']
        )
        client.bake('baker5', BAKE_ARGS)

    @pytest.mark.parametrize("expression,expected_failure", BAKER_ACTIONS)
    def test_run_baker_and_baker_proxy_script(
        self, client: Client, expression, expected_failure
    ):
        # Test that an originated script with baker operation fails typecheck
        baker = BOOTSTRAP_BAKERS[1]['hash']
        source = 'baker2_key'
        dummy_contract = client.rpc(
            'get',
            'chains/main/blocks/head/helpers/scripts/run_baker_code_contract',
        )
        owner_key = IDENTITIES[source]['public']
        chain_id = client.rpc('get', 'chains/main/chain_id')
        param_type = '''
        pair
           (pair chain_id address)
           (pair
              (nat %counter)
              (or
                 (lambda %operation
                         unit
                         (pair (list operation) (list baker_operation)))
                 (pair %change_keys
                    (nat %threshold)
                    (list %keys key))))'''

        def mk_signed_payload(counter, target):
            payload = f'Pair {counter} ({expression})'
            param = f'Pair (Pair "{chain_id}" "{target}") ({payload})'
            packed_hash = client.pack(param, param_type)
            signature = client.sign_bytes_of_string(packed_hash, source)
            return f'(Pair ({payload}) {{Some "{signature}" }})'

        # test `run baker script` command
        counter = 0
        param = mk_signed_payload(counter, dummy_contract)
        storage = f'Pair {counter} (Pair 1 {{ "{owner_key}" }})'
        client.run_baker_script(
            storage, param, source=source, entrypoint='main'
        )

        # test baker-proxy.tz script using `run script` command
        proxy_storage = 'Unit'
        run_proxy_param = f'(Pair "{baker}" (Right {param}))'
        client.run_script(PROXY_SCRIPT, proxy_storage, run_proxy_param)

        # test call originated contract with baker-proxy.tz script
        baker_storage = client.get_storage(baker)
        pattern = r'Pair (\d+)'
        match_counter = re.search(pattern, baker_storage)
        match_fail = f"Cannot find baker's stored counter in {baker_storage}"
        assert match_counter is not None, match_fail
        stored_counter = match_counter.group(1)
        payload = mk_signed_payload(stored_counter, baker)
        proxy_param = f'(Pair "{baker}" (Right {payload}))'
        if expected_failure is None:
            client.call(
                'bootstrap1',
                'proxy',
                ['--arg', proxy_param, '--burn-cap', '0.312'],
            )
            client.bake('baker5', BAKE_ARGS)
        else:
            with utils.assert_run_failure(expected_failure):
                client.call(
                    'bootstrap1',
                    'proxy',
                    ['--arg', proxy_param, '--burn-cap', '0.312'],
                )

    def test_toggle_baker_delegations(self, client: Client):
        client.set_delegate('bootstrap1', 'baker3')
        client.bake('baker5', BAKE_ARGS)
        delegate = BOOTSTRAP_BAKERS[2]['hash']
        assert client.get_delegate('bootstrap1', []).delegate == delegate

        client.toggle_baker_delegations('baker3', False)
        client.bake('baker5', BAKE_ARGS)
        expected_error = (
            rf'The baker {BOOTSTRAP_BAKERS[2]["hash"]} already declines '
            'delegations, no need to set it again'
        )
        with utils.assert_run_failure(expected_error):
            client.toggle_baker_delegations('baker3', False)

        # new delegation should be declined
        expected_error = "Baker declines new delegations"
        with utils.assert_run_failure(expected_error):
            client.set_delegate('bootstrap2', 'baker3')
        assert client.get_delegate('bootstrap2', []).delegate is None

        # re-delegation should be declined too
        expected_error = "Baker declines new delegations"
        with utils.assert_run_failure(expected_error):
            client.set_delegate('bootstrap1', 'baker3')
        assert client.get_delegate('bootstrap2', []).delegate is None

        # existing delegation can be withdrawn
        client.withdraw_delegate('bootstrap1')
        client.bake('baker5', BAKE_ARGS)
        assert client.get_delegate('bootstrap1', []).delegate is None

        # trying to delegate again after withdrawing should be declined
        expected_error = "Baker declines new delegations"
        with utils.assert_run_failure(expected_error):
            client.set_delegate('bootstrap1', 'baker3')
        assert client.get_delegate('bootstrap2', []).delegate is None

        # allow delegations and try again
        client.toggle_baker_delegations('baker3', True)
        client.bake('baker5', BAKE_ARGS)
        expected_error = (
            rf'The baker {BOOTSTRAP_BAKERS[2]["hash"]} already accepts '
            'delegations, no need to set it again'
        )
        with utils.assert_run_failure(expected_error):
            client.toggle_baker_delegations('baker3', True)
        client.set_delegate('bootstrap1', 'baker3')
        client.set_delegate('bootstrap2', 'baker3')

    def test_call_generic(self, client: Client):
        # generic lambda type is:
        # `lambda unit (pair (list operation) (list baker_operation))`
        param = (
            '{ DROP ; NIL baker_operation ; NIL operation ; '
            f'PUSH key_hash "{IDENTITIES["bootstrap1"]["identity"]}" ; '
            'IMPLICIT_ACCOUNT ; PUSH mutez 10000 ; UNIT ; '
            'TRANSFER_TOKENS ; CONS ; PAIR }'
        )
        generic_call = [
            'call',
            'generic',
            'baker',
            'baker3',
            'with',
            'lambda',
            param,
        ]
        client.run(generic_call)
        client.bake('baker5', BAKE_ARGS)

    def test_transfer(self, client: Client):
        source = 'baker3'
        amount = 101.301
        amount_mutez = utils.mutez_of_tez(amount)
        cmd = [
            'from',
            'baker',
            'contract',
            source,
            'transfer',
            str(amount),
            'to',
        ]

        # transfer to an implicit account
        target = 'bootstrap1'
        fee = 0.000859
        fee_mutez = utils.mutez_of_tez(fee)
        balance_source = client.get_mutez_balance(source)
        balance_target = client.get_mutez_balance(target)
        client.run(cmd + [target])
        client.bake('baker5', BAKE_ARGS)
        new_balance_source = client.get_mutez_balance(source)
        new_balance_target = client.get_mutez_balance(target)
        assert balance_source - fee_mutez - amount_mutez == new_balance_source
        assert balance_target + amount_mutez == new_balance_target

        # transfer to an originated account
        target = 'str_id_contract'
        fee = 0.001098
        fee_mutez = utils.mutez_of_tez(fee)
        contract = path.join(
            CONTRACT_PATH, 'entrypoints', 'simple_entrypoints.tz'
        )
        client.originate(
            target, 1000, source, contract, args=['--burn-cap', '0.321']
        )
        client.bake('baker5', BAKE_ARGS)
        balance_source = client.get_mutez_balance(source)
        balance_target = client.get_mutez_balance(target)
        client.run(cmd + [target, '--arg', '"foo"', '--entrypoint', 'B'])
        client.bake('baker5', BAKE_ARGS)
        new_balance_source = client.get_mutez_balance(source)
        new_balance_target = client.get_mutez_balance(target)
        assert balance_source - fee_mutez - amount_mutez == new_balance_source
        assert balance_target + amount_mutez == new_balance_target
