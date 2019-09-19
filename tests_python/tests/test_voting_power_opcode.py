import pytest
from tools import constants

VOTING_POWER_CONTRACT = '''{
  parameter (pair key nat); # A public key, and the number of rolls expected
  storage unit;
  code { CAR; UNPAIR;
         HASH_KEY; VOTING_POWER; # Get the number of rolls for the key
         ASSERT_CMPEQ; # Number of rolls should equal to rolls expected
         UNIT; NIL operation; PAIR } }'''


@pytest.mark.contract
class TestWotingPowerOpcode:

    def test_write_voting_power_contract(self, tmpdir, session):
        name = 'voting_power_contract.tz'
        contract = f'{tmpdir}/{name}'
        with open(contract, 'w') as contract_file:
            contract_file.write(VOTING_POWER_CONTRACT)
            session[name] = contract

    def test_typecheck_voting_power_contract(self, client, session):
        name = 'voting_power_contract.tz'
        contract = session[name]
        client.typecheck(contract)

    def test_run_voting_power_contract(self, client, session):
        tokens_per_roll = int(constants.PARAMETERS['tokens_per_roll'])
        name = 'voting_power_contract.tz'
        contract = session[name]
        storage = 'Unit'
        for num in range(1, 5):
            alias = f'bootstrap{num}'
            balance = client.get_mutez_balance(f'{alias}')
            rolls = int(balance) // tokens_per_roll
            pubkey = constants.IDENTITIES[alias]['public']
            param = f'Pair "{pubkey}" {rolls}'
            client.run_script(contract, storage, param)
