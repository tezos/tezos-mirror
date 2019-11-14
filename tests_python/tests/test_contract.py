import os
import subprocess
import pytest
from tools import utils, paths, constants

CONTRACT_PATH = f'{paths.TEZOS_HOME}/src/bin_client/test/contracts'

BAKE_ARGS = ['--minimal-timestamp']


def file_basename(path):
    return os.path.splitext(os.path.basename(path))[0]


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
        assert client.get_delegate('manager', []) == "none"
        assert client.get_delegate('delegatable_target', []) == "none"

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
                        ['--gas-limit', '36558'])
        client.bake('bootstrap5', BAKE_ARGS)
        new_balance = client.get_mutez_balance('manager')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.004001
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
                        ['--gas-limit', '44659'])
        client.bake('bootstrap5', BAKE_ARGS)
        new_balance = client.get_mutez_balance('manager')
        new_balance_dest = client.get_mutez_balance('manager2')
        new_balance_bootstrap = client.get_mutez_balance('bootstrap2')
        fee = 0.004811
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
    directories = ['attic', 'opcodes']
    contracts = []
    for directory in directories:
        for contract in os.listdir(f'{CONTRACT_PATH}/{directory}'):
            contracts.append(f'{directory}/{contract}')
    return contracts


@pytest.mark.slow
@pytest.mark.contract
class TestContracts:
    """Test type checking and execution of a bunch of contracts"""

    def test_gen_keys(self, client):
        client.gen_key('foo')
        client.gen_key('bar')

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
        DUP ; PAIR } }
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
                 'second_explosion.tz': SECOND_EXPLOSION}.items()
        for name, script in items:
            contract = f'{tmpdir}/{name}'
            with open(contract, 'w') as contract_file:
                contract_file.write(script)
                session[name] = contract

    def test_originate_first_explosion(self, client, session):
        name = 'first_explosion.tz'
        contract = session[name]
        # TODO client.typecheck(contract) -> type error not what we expect?
        args = ['-G', '8000', '--burn-cap', '10']
        with pytest.raises(subprocess.CalledProcessError) as _exc:
            client.originate(f'{name}', 0, 'bootstrap1', contract, args)
        # TODO capture output and check error message is correct

    def test_originate_second_explosion(self, client, session):
        name = 'second_explosion.tz'
        contract = session[name]
        storage = '{}'
        inp = '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
        client.run_script(contract, storage, inp)

    # TODO complete with tests from test_contract.sh


@pytest.mark.contract
class TestChainId:

    def test_chain_id_opcode(self, client, session):
        path = f'{CONTRACT_PATH}/opcodes/chain_id.tz'
        originate(client, session, path, 'Unit', 0)
        client.transfer(0, 'bootstrap2', "chain_id", [])
        client.bake('bootstrap5', BAKE_ARGS)

    def test_chain_id_authentication_origination(self, client, session):
        path = f'{CONTRACT_PATH}/mini_scenarios/authentication.tz'
        pubkey = constants.IDENTITIES['bootstrap1']['public']
        originate(client, session, path, f'Pair 0 "{pubkey}"', 1000)
        client.bake('bootstrap5', BAKE_ARGS)

    def test_chain_id_authentication_first_run(self, client, session):
        destination = constants.IDENTITIES['bootstrap2']['identity']
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
class TestComparablePairs:

    def test_comparable_pair(self, client):
        # tests that comb pairs are comparable and that the order is the
        # expected one
        client.typecheck_data('{}', '(set (pair nat string))')
        client.typecheck_data('{Pair 0 "foo"}', '(set (pair nat string))')
        client.typecheck_data('{Pair 0 "foo"; Pair 1 "bar"}',
                              '(set (pair nat string))')
        client.typecheck_data('{Pair 0 "bar"; Pair 0 "foo"; \
                                Pair 1 "bar"; Pair 1 "foo"}',
                              '(set (pair nat string))')
        client.typecheck_data('{}', '(set (pair nat (pair string bytes)))')

        client.typecheck_data('{}', '(map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "foo") Unit}',
                              '(map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "foo") Unit; \
                                Elt (Pair 1 "bar") Unit}',
                              '(map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "bar") Unit; \
                                Elt (Pair 0 "foo") Unit; \
                                Elt (Pair 1 "bar") Unit; \
                                Elt (Pair 1 "foo") Unit}',
                              '(map (pair nat string) unit)')
        client.typecheck_data('{}',
                              '(map (pair nat (pair string bytes)) unit)')

        client.typecheck_data('{}', '(big_map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "foo") Unit}',
                              '(big_map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "foo") Unit; \
                                Elt (Pair 1 "bar") Unit}',
                              '(big_map (pair nat string) unit)')
        client.typecheck_data('{Elt (Pair 0 "bar") Unit; \
                                Elt (Pair 0 "foo") Unit; \
                                Elt (Pair 1 "bar") Unit; \
                                Elt (Pair 1 "foo") Unit}',
                              '(big_map (pair nat string) unit)')
        client.typecheck_data('{}',
                              '(big_map (pair nat (pair string bytes)) unit)')

    def test_order_of_pairs(self, client):
        # tests that badly-ordered set literals are rejected
        utils.check_typecheck_data_failure(
            client, '{Pair 0 "foo"; Pair 0 "bar"}', '(set (pair nat string))')
        utils.check_typecheck_data_failure(
            client, '{Pair 1 "bar"; Pair 0 "foo"}', '(set (pair nat string))')

    def test_non_comparable_non_comb_pair(self, client):
        # tests that non-comb pairs are rejected by the typechecker
        utils.check_typecheck_data_failure(
            client, '{}', '(set (pair (pair nat nat) nat))')

    # This should be moved to test_contract_opcodes.py once MR !1261 is merged
    @pytest.mark.parametrize(
        "contract,param,storage,expected",
        [   # FORMAT: assert_output contract_file storage input expected_result
            # Mapping over maps
            ('map_map_sideeffect.tz',
             '(Pair {} 0)', '10', '(Pair {} 0)'),
            ('map_map_sideeffect.tz',
             '(Pair { Elt "foo" 1 } 1)', '10', '(Pair { Elt "foo" 11 } 11)'),
            ('map_map_sideeffect.tz',
             '(Pair { Elt "bar" 5 ; Elt "foo" 1 } 6)', '15',
             '(Pair { Elt "bar" 20 ; Elt "foo" 16 } 36)')
        ])
    def test_map_map_sideeffect(self,
                                client,
                                contract,
                                param,
                                storage,
                                expected):
        contract = f'{CONTRACT_PATH}/opcodes/{contract}'
        run_script_res = client.run_script(contract, param, storage)
        assert run_script_res.storage == expected


class TestTypecheckingErrors:
    def test_big_map_arity_error(self, client):
        def cmd():
            client.typecheck(f'{CONTRACT_PATH}/illtyped/big_map_arity.tz')

        utils.check_run_failure(
            cmd,
            'primitive EMPTY_BIG_MAP expects 2 arguments but is given 1.'
        )
