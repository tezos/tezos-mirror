import os
import re
import pytest
from tools import paths
from tools import utils
from tools.constants import IDENTITIES

CONTRACT_PATH = f'{paths.TEZOS_HOME}/src/bin_client/test/contracts'
ILLTYPED_CONTRACT_PATH = f'{CONTRACT_PATH}/ill_typed'
DEPRECATED_CONTRACT_PATH  = f'{CONTRACT_PATH}/deprecated'

BAKE_ARGS = ['--minimal-timestamp']


def file_basename(path):
    return os.path.splitext(os.path.basename(path))[0]


# Generic piece of code to originate a contract
def originate(client,
              session,
              contract,
              init_storage,
              amount,
              contract_name=None,
              sender='bootstrap1',
              baker='bootstrap5'):
    if contract_name is None:
        contract_name = file_basename(contract)
    args = ['--init', init_storage, '--burn-cap', '10.0']
    origination = client.originate(contract_name, amount,
                                   sender, contract, args)
    session['contract'] = origination.contract
    print(origination.contract)
    client.bake(baker, BAKE_ARGS)
    assert utils.check_block_contains_operations(client,
                                                 [origination.operation_hash])


@pytest.mark.contract
class TestManager:

    def test_manager_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/manager.tz'
        pubkey = constants.IDENTITIES['bootstrap2']['identity']
        originate(client, session, path, f'"{pubkey}"', 1000)
        originate(client, session, path, f'"{pubkey}"', 1000,
                  contract_name="manager2")

    def test_delegatable_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/delegatable_target.tz'
        pubkey = constants.IDENTITIES['bootstrap2']['identity']
        originate(client, session, path,
                  f'Pair "{pubkey}" (Pair "hello" 45)', 1000)

    def test_target_with_entrypoints_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/big_map_entrypoints.tz'
        originate(client, session, path, 'Pair {} {}', 1000,
                  contract_name='target')

    def test_target_without_entrypoints_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/no_entrypoint_target.tz'
        originate(client, session, path, 'Pair "hello" 42', 1000,
                  contract_name='target_no_entrypoints')

    def test_target_without_default_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/no_default_target.tz'
        originate(client, session, path, 'Pair "hello" 42', 1000,
                  contract_name='target_no_default')

    def test_target_with_root_origination(self, client, session):
        path = f'{CONTRACT_PATH}/entrypoints/rooted_target.tz'
        originate(client, session, path, 'Pair "hello" 42', 1000,
                  contract_name='rooted_target')

    def test_manager_set_delegate(self, client):
        client.set_delegate('manager', 'bootstrap2', [])
        client.bake('bootstrap5', BAKE_ARGS)
        client.set_delegate('delegatable_target', 'bootstrap2', [])
        client.bake('bootstrap5', BAKE_ARGS)
        delegate = constants.IDENTITIES['bootstrap2']['identity']
        assert client.get_delegate('manager', []) == delegate
        assert client.get_delegate('delegatable_target', []) == delegate
        client.set_delegate('manager', 'bootstrap3', [])
        client.bake('bootstrap5', BAKE_ARGS)
        client.set_delegate('delegatable_target', 'bootstrap3', [])
        client.bake('bootstrap5', BAKE_ARGS)
        delegate = constants.IDENTITIES['bootstrap3']['identity']
        assert client.get_delegate('manager', []) == delegate
        assert client.get_delegate('delegatable_target', []) == delegate

    def test_manager_withdraw_delegate(self, client):
        client.withdraw_delegate('manager', [])
        client.bake('bootstrap5', BAKE_ARGS)
        client.withdraw_delegate('delegatable_target', [])
        client.bake('bootstrap5', BAKE_ARGS)
        assert client.get_delegate('manager', []) is None
        assert client.get_delegate('delegatable_target', []) is None

    def test_transfer_to_manager(self, client):
        balance = client.get_mutez_balance('manager')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10.001
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'bootstrap2', 'manager',
                        ['--gas-limit', '15285'])
        client.bake('bootstrap5', BAKE_ARGS)
        new_balance = client.get_mutez_balance('manager')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.00178
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance + amount_mutez == new_balance
        assert (balance_bootstrap - fee_mutez - amount_mutez
                == new_balance_bootstrap)

    def test_simple_transfer_from_manager_to_implicit(self, client):
        balance = client.get_mutez_balance('manager')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10.1
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'manager', 'bootstrap2',
                        ['--gas-limit', '26183'])
        client.bake('bootstrap5', BAKE_ARGS)
        new_balance = client.get_mutez_balance('manager')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.002931
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance - amount_mutez == new_balance
        assert (balance_bootstrap + amount_mutez - fee_mutez
                == new_balance_bootstrap)

    def test_transfer_from_manager_to_manager(self, client):
        balance = client.get_mutez_balance('manager')
        balance_dest = client.get_mutez_balance('manager2')
        balance_bootstrap = client.get_mutez_balance('bootstrap2')
        amount = 10
        amount_mutez = utils.mutez_of_tez(amount)
        client.transfer(amount, 'manager', 'manager2',
                        ['--gas-limit', '44625'])
        client.bake('bootstrap5', BAKE_ARGS)
        new_balance = client.get_mutez_balance('manager')
        new_balance_dest = client.get_mutez_balance('manager2')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.004804
        fee_mutez = utils.mutez_of_tez(fee)
        assert balance_bootstrap - fee_mutez == new_balance_bootstrap
        assert balance - amount_mutez == new_balance
        assert balance_dest + amount_mutez == new_balance_dest

    def test_transfer_from_manager_to_default(self, client):
        client.transfer(10, 'manager', 'bootstrap2',
                        ['--entrypoint', 'default'])
        client.bake('bootstrap5', BAKE_ARGS)
        client.transfer(10, 'manager', 'manager',
                        ['--entrypoint', 'default'])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_transfer_from_manager_to_target(self, client):
        client.transfer(10, 'manager', 'target', ['--burn-cap', '0.356'])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_transfer_from_manager_to_entrypoint_with_args(self, client):
        arg = 'Pair "hello" 42'
        client.transfer(0, 'manager', 'target',
                        ['--entrypoint', 'add_left',
                         '--arg', arg,
                         '--burn-cap', '0.067'])
        client.bake('bootstrap5', BAKE_ARGS)
        client.transfer(0, 'manager', 'target',
                        ['--entrypoint', 'mem_left',
                         '--arg', '"hello"'])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_transfer_from_manager_no_entrypoint_with_args(self, client):
        arg = 'Left Unit'
        client.transfer(0, 'manager', 'target_no_entrypoints',
                        ['--arg', arg])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_transfer_from_manager_to_no_default_with_args(self, client):
        arg = 'Left Unit'
        client.transfer(0, 'manager', 'target_no_default',
                        ['--arg', arg])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_transfer_from_manager_to_rooted_target_with_args(self, client):
        arg = 'Left Unit'
        client.transfer(0, 'manager', 'rooted_target',
                        ['--arg', arg,
                         '--entrypoint', 'root'])
        client.bake('bootstrap5', BAKE_ARGS)


def all_contracts():
    directories = ['attic', 'opcodes',
                   'macros', 'mini_scenarios', 'non_regression']
    contracts = []
    for directory in directories:
        for contract in os.listdir(f'{CONTRACT_PATH}/{directory}'):
            contracts.append(f'{directory}/{contract}')
    return contracts

def all_deprecated_contract():
    contracts = []
    for contract in os.listdir(f'{DEPRECATED_CONTRACT_PATH}'):
        contracts.append(f'{DEPRECATED_CONTRACT_PATH}/{contract}')
    return contracts


@pytest.mark.slow
@pytest.mark.contract
class TestContracts:
    """Test type checking and execution of a bunch of contracts"""

    @pytest.mark.parametrize("contract", all_contracts())
    def test_typecheck(self, client, contract):
        if contract.endswith('.tz'):
            client.typecheck(f'{CONTRACT_PATH}/{contract}')

    # TODO add more tests here
    @pytest.mark.parametrize("contract,param,storage,expected",
                             [('opcodes/ret_int.tz', 'None', 'Unit',
                               '(Some 300)')])
    def test_run(self, client, contract, param, storage, expected):
        if contract.endswith('.tz'):
            contract = f'{CONTRACT_PATH}/{contract}'
            run_script_res = client.run_script(contract, param, storage)
            assert run_script_res.storage == expected

    @pytest.mark.parametrize("contract", all_deprecated_contract())
    def test_deprected_typecheck(self, client, contract):
        def cmd():
            client.typecheck(contract)

        assert utils.check_run_failure(cmd, r'Use of deprecated instruction')

    @pytest.mark.parametrize("contract,error_pattern", [
        # operations cannot be PACKed
        ("pack_operation.tz",
         r'operation type forbidden in parameter, storage and constants'),
        # big_maps cannot be PACKed
        ("pack_big_map.tz",
         r'big_map type not allowed inside another big_map')
    ])
    def test_ill_typecheck(self, client, contract, error_pattern):
        def cmd():
            client.typecheck(f'{ILLTYPED_CONTRACT_PATH}/{contract}')

        assert utils.check_run_failure(cmd, error_pattern)


FIRST_EXPLOSION = '''
{ parameter unit;
  storage unit;
  code{ DROP; PUSH nat 0 ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DROP ; UNIT ; NIL operation ; PAIR} }
'''


# FIRST_EXPLOSION costs a large amount of gas just for typechecking.
# FIRST_EXPLOSION_BIGTYPE type size exceeds the protocol set bound.
FIRST_EXPLOSION_BIGTYPE = '''
{ parameter unit;
  storage unit;
  code{ DROP; PUSH nat 0 ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DROP ; UNIT ; NIL operation ; PAIR} }
'''


SECOND_EXPLOSION = '''
{ parameter (list int) ;
  storage (list (list (list int))) ;
  code { CAR ; DIP { NIL (list int) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; DIP { NIL (list (list int)) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; NIL operation ; PAIR } }
'''


@pytest.mark.contract
class TestGasBound:

    def test_write_contract(self, tmpdir, session):
        items = {'first_explosion.tz': FIRST_EXPLOSION,
                 'first_explosion_bigtype.tz': FIRST_EXPLOSION_BIGTYPE,
                 'second_explosion.tz': SECOND_EXPLOSION}.items()
        for name, script in items:
            contract = f'{tmpdir}/{name}'
            with open(contract, 'w') as contract_file:
                contract_file.write(script)
                session[name] = contract

    def test_originate_first_explosion(self, client, session):
        name = 'first_explosion.tz'
        contract = session[name]
        client.typecheck(contract)
        args = ['-G', '8000', '--burn-cap', '10']

        def client_cmd():
            client.originate(f'{name}', 0,
                             'bootstrap1', contract, args)
        expected_error = "Gas limit exceeded during typechecking or execution"
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_originate_big_type(self, client, session):
        name = 'first_explosion_bigtype.tz'
        contract = session[name]

        def client_cmd():
            client.typecheck(contract)
        # We could not be bothered with finding how to escape parentheses
        # so we put dots
        expected_error = "type size .1023. exceeded maximum type size .1000."
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_originate_second_explosion(self, client, session):
        name = 'second_explosion.tz'
        contract = session[name]
        storage = '{}'
        inp = '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
        client.run_script(contract, storage, inp)

    def test_originate_second_explosion_fail(self, client, session):
        name = 'second_explosion.tz'
        contract = session[name]
        storage = '{}'
        inp = ('{1;2;3;4;5;6;7;8;9;0;1;2;3;4;5;6;7;1;1;1;1;1;1;1;1;1;1;1' +
               ';1;1;1;1;1;1;1;1;1;1;1}')

        def client_cmd():
            client.run_script(contract, storage, inp)
        expected_error = \
            ("Cannot serialize the resulting storage" +
             " value within the provided gas bounds.")
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_typecheck_map_dup_key(self, client):

        def client_cmd():
            client.typecheck_data('{ Elt 0 1 ; Elt 0 1}', '(map nat nat)')
        expected_error = \
            ('Map literals cannot contain duplicate' +
             ' keys, however a duplicate key was found')
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_typecheck_map_bad_ordering(self, client):

        def client_cmd():
            client.typecheck_data('{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }',
                                  '(map nat nat)')
        expected_error = \
            ("Keys in a map literal must be in strictly" +
             " ascending order, but they were unordered in literal")
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_typecheck_set_bad_ordering(self, client):

        def client_cmd():
            client.typecheck_data('{ "A" ; "C" ; "B" }', '(set string)')
        expected_error = \
            ("Values in a set literal must be in strictly" +
             " ascending order, but they were unordered in literal")
        assert utils.check_run_failure(client_cmd, expected_error)

    def test_typecheck_set_no_duplicates(self, client):
        def client_cmd():
            client.typecheck_data('{ "A" ; "B" ; "B" }', '(set string)')
        expected_error = \
            ("Set literals cannot contain duplicate values," +
             " however a duplicate value was found")
        assert utils.check_run_failure(client_cmd, expected_error)


@pytest.mark.contract
class TestChainId:

    def test_chain_id_opcode(self, client, session):
        path = f'{CONTRACT_PATH}/opcodes/chain_id.tz'
        originate(client, session, path, 'Unit', 0)
        client.transfer(0, 'bootstrap2', "chain_id", [])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_chain_id_authentication_origination(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/authentication.tz'
        pubkey = IDENTITIES['bootstrap1']['public']
        originate(client, session, path, f'Pair 0 "{pubkey}"', 1000)
        client.bake('bootstrap5', BAKE_ARGS)

    def test_chain_id_authentication_first_run(self, client, session):
        destination = IDENTITIES['bootstrap2']['identity']
        operation = '{DROP; NIL operation; ' + \
            f'PUSH address "{destination}"; ' + \
            'CONTRACT unit; ASSERT_SOME; PUSH mutez 1000; UNIT; ' + \
            'TRANSFER_TOKENS; CONS}'
        chain_id = client.rpc('get', 'chains/main/chain_id')
        contract_address = session['contract']
        packed = client.pack(
            f'Pair (Pair "{chain_id}" "{contract_address}") ' +
            f'(Pair {operation} 0)',
            'pair (pair chain_id address)' +
            '(pair (lambda unit (list operation)) nat)')
        signature = client.sign(packed, "bootstrap1")
        client.transfer(0, 'bootstrap2', 'authentication',
                        ['--arg', f'Pair {operation} \"{signature}\"'])
        client.bake('bootstrap5', BAKE_ARGS)


@pytest.mark.contract
class TestBigMapToSelf:

    def test_big_map_to_self_origination(self, client, session):
        path = f'{CONTRACT_PATH}/opcodes/big_map_to_self.tz'
        originate(client, session, path, '{}', 0)
        client.bake('bootstrap5', BAKE_ARGS)

    def test_big_map_to_self_transfer(self, client):
        client.transfer(0, 'bootstrap2', "big_map_to_self", [])


@pytest.mark.contract
class TestNonRegression:
    """Test contract-related non-regressions"""

    def test_issue_242_originate(self, client, session):
        path = f'{CONTRACT_PATH}/non_regression/bug_262.tz'
        originate(client, session, path, 'Unit', 1)

    def test_issue_242_assert_balance(self, client):
        assert client.get_balance('bug_262') == 1


@pytest.mark.contract
class TestMiniScenarios:
    """Test mini scenarios"""

    # replay.tz related tests
    def test_replay_originate(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/replay.tz'
        originate(client, session, path, 'Unit', 0)

    def test_replay_transfer_fail(self, client):
        def client_cmd():
            client.transfer(0, "bootstrap1", "replay", [])
        assert utils.check_run_failure(client_cmd,
                                       "Internal operation replay attempt")

    # create_contract.tz related tests
    def test_create_contract_originate(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/create_contract.tz'
        originate(client, session, path, 'Unit', 1000)

    def test_create_contract_balance(self, client):
        assert client.get_balance('create_contract') == 1000

    def test_create_contract_perform_creation(self, client):
        transfer_result = client.transfer(0, "bootstrap1", "create_contract",
                                          ['-arg',
                                           'None',
                                           '--burn-cap',
                                           '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        pattern = r"New contract (\w*) originated"
        match = re.search(pattern, transfer_result.client_output)
        kt_1 = match.groups()[0]
        assert client.get_storage(kt_1) == '"abcdefg"'
        assert client.get_balance(kt_1) == 100
        assert client.get_balance('create_contract') == 900

    # default_account.tz related tests
    def test_default_account_originate(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/default_account.tz'
        originate(client, session, path, 'Unit', 1000)

    def test_default_account_transfer_then_bake(self, client):
        tz1 = IDENTITIES['bootstrap4']['identity']
        client.transfer(0, "bootstrap1", "default_account",
                        ['-arg', f'"{tz1}"', '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        account = 'tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5'
        client.transfer(0, "bootstrap1", "default_account",
                        ['-arg', f'"{account}"', '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        assert client.get_balance(account) == 100

    # Test bytes, SHA252, CHECK_SIGNATURE
    def test_reveal_signed_preimage_originate(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/reveal_signed_preimage.tz'
        byt = ('0x9995c2ef7bcc7ae3bd15bdd9b02' +
               'dc6e877c27b26732340d641a4cbc6524813bb')
        sign = f'p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz'
        storage = f'(Pair {byt} "{sign}")'
        originate(client, session, path, storage, 1000)

    def test_wrong_preimage(self, client):
        byt = ('0x050100000027566f756c657a2d766f75732' +
               '0636f75636865722061766563206d6f692c20636520736f6972')
        sign = ('p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJ' +
                'VQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8')
        arg = f'(Pair {byt} "{sign}")'

        def client_cmd():
            client.transfer(0, "bootstrap1", "reveal_signed_preimage",
                            ['-arg', arg, '--burn-cap', '10'])
        # We check failure of ASSERT_CMPEQ in the script.
        assert utils.check_run_failure(client_cmd,
                                       "At line 8 characters 9 to 21")

    def test_wrong_signature(self, client):
        byt = ('0x050100000027566f756c657a2d766f757320636' +
               'f75636865722061766563206d6f692c20636520736f6972203f')
        sign = ('p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6' +
                'LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8')
        arg = f'(Pair {byt} "{sign}")'

        def client_cmd():
            client.transfer(0, "bootstrap1", "reveal_signed_preimage",
                            ['-arg', arg, '--burn-cap', '10'])
        # We check failure of CHECK_SIGNATURE ; ASSERT in the script.
        assert utils.check_run_failure(client_cmd,
                                       "At line 15 characters 9 to 15")

    def test_good_preimage_and_signature(self, client):
        byt = ('0x050100000027566f756c657a2d766f757320636f7563' +
               '6865722061766563206d6f692c20636520736f6972203f')
        sign = ('p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1F' +
                'hfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ')
        arg = f'(Pair {byt} "{sign}")'
        client.transfer(0, "bootstrap1", "reveal_signed_preimage",
                        ['-arg', arg, '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)

    # Test vote_for_delegate
    def test_vote_for_delegate_originate(self, client, session):
        b_3 = IDENTITIES['bootstrap3']['identity']
        b_4 = IDENTITIES['bootstrap4']['identity']
        path = f'{CONTRACT_PATH}/mini_scenarios/vote_for_delegate.tz'
        storage = f'''(Pair (Pair "{b_3}" None) (Pair "{b_4}" None))'''
        originate(client, session, path, storage, 1000)
        assert client.get_delegate('vote_for_delegate').delegate is None

    def test_vote_for_delegate_wrong_identity1(self, client):
        def client_cmd():
            client.transfer(0, "bootstrap1", "vote_for_delegate",
                            ['-arg', 'None', '--burn-cap', '10'])
        # We check failure of CHECK_SIGNATURE ; ASSERT in the script.
        assert utils.check_run_failure(client_cmd,
                                       "At line 15 characters 57 to 61")

    def test_vote_for_delegate_wrong_identity2(self, client):
        def client_cmd():
            client.transfer(0, "bootstrap2", "vote_for_delegate",
                            ['-arg', 'None', '--burn-cap', '10'])
        # We check failure of CHECK_SIGNATURE ; ASSERT in the script.
        assert utils.check_run_failure(client_cmd,
                                       "At line 15 characters 57 to 61")

    def test_vote_for_delegate_b3_vote_for_b5(self, client):
        b_5 = IDENTITIES['bootstrap5']['identity']
        client.transfer(0, "bootstrap3", "vote_for_delegate",
                        ['-arg', f'(Some "{b_5}")', '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        storage = client.get_storage('vote_for_delegate')
        assert re.search(b_5, storage)

    def test_vote_for_delegate_still_no_delegate1(self, client):
        assert client.get_delegate('vote_for_delegate').delegate is None

    def test_vote_for_delegate_b4_vote_for_b2(self, client):
        b_2 = IDENTITIES['bootstrap2']['identity']
        client.transfer(0, "bootstrap4", "vote_for_delegate",
                        ['-arg', f'(Some "{b_2}")', '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        storage = client.get_storage('vote_for_delegate')
        assert re.search(b_2, storage)

    def test_vote_for_delegate_still_no_delegate2(self, client):
        assert client.get_delegate('vote_for_delegate').delegate is None

    def test_vote_for_delegate_b4_vote_for_b5(self, client):
        b_5 = IDENTITIES['bootstrap5']['identity']
        client.transfer(0, "bootstrap4", "vote_for_delegate",
                        ['-arg', f'(Some "{b_5}")', '--burn-cap', '10'])
        client.bake('bootstrap5', BAKE_ARGS)
        storage = client.get_storage('vote_for_delegate')
        assert re.search(b_5, storage)

    def test_vote_for_delegate_has_delegate(self, client):
        b_5 = IDENTITIES['bootstrap5']['identity']
        result = client.get_delegate('vote_for_delegate')
        assert result.delegate == b_5
