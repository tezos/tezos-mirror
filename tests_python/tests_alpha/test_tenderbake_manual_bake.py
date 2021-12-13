import copy
import time
import pytest
from tools import utils, constants
from launchers.sandbox import Sandbox
from . import protocol


MINIMAL_BLOCK_DELAY = 1
TRANSFER_AMOUNT = 500
INITIAL_BALANCE = 4000000.0
FEE = 0.1
ALL_ACOUNTS = [
    'bootstrap1',
    'bootstrap2',
    'bootstrap3',
    'bootstrap4',
    'bootstrap5',
]


def find_account(identity):
    for account, keys in constants.IDENTITIES.items():
        if keys['identity'] == identity:
            return account
    return None


def baker_at_round_0(client, level='head'):
    if level == 'head':
        arg = ''
    else:
        arg = f'?level={level}'
    res = client.rpc(
        'get', f'/chains/main/blocks/head/helpers/baking_rights{arg}'
    )
    delegate_id = res[0]['delegate']
    return find_account(delegate_id)


@pytest.mark.slow
@pytest.mark.tenderbake
@pytest.mark.incremental
@pytest.mark.tenderbake
class TestManualBake:
    """Run a number of nodes, and bake, preendorse, and endorse using the
    client commands"""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(0, params=constants.NODE_PARAMS)

        proto_params = dict(protocol.TENDERBAKE_PARAMETERS)
        parameters = copy.deepcopy(proto_params)
        parameters['minimal_block_delay'] = str(MINIMAL_BLOCK_DELAY)
        parameters['delay_increment_per_round'] = '1'
        protocol.activate(
            sandbox.client(0),
            parameters=proto_params,
            activate_in_the_past=True,
        )

    def test_choose_players(self, sandbox: Sandbox, session: dict):
        # we will make a transfer to someone who is not baking, to simplify
        # the computation of the expected balance
        client = sandbox.client(0)
        session['level2_baker'] = baker_at_round_0(client)
        session['level3_baker'] = baker_at_round_0(client, str(3))
        # recepient should also be different from bootstrap1 because
        # bootstrap1 makes the transfer and we don't want a self-transfer
        for account in ALL_ACOUNTS[1:]:
            if account not in [
                session['level3_baker'],
                session['level2_baker'],
            ]:
                session['recepient'] = account
                break

    def test_bake(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        client.propose([session['level2_baker']], ['--minimal-timestamp'])

    def test_deposit(self, sandbox: Sandbox, session: dict):
        """Test balance of the receipient takes into account deposits taken at
        level 2"""
        client = sandbox.client(0)
        balance = client.get_mutez_balance(session['recepient'])
        recepient_pkh = constants.IDENTITIES[session['recepient']]['identity']
        deposit_mutez = client.frozen_deposits(recepient_pkh)
        assert balance == utils.mutez_of_tez(INITIAL_BALANCE) - deposit_mutez

    def test_level(self, sandbox: Sandbox):
        client = sandbox.client(0)
        head = client.get_head()
        assert head['header']['level'] == 2

    def test_preendorse(self, sandbox: Sandbox):
        client = sandbox.client(0)
        client.run(['preendorse', 'for', 'bootstrap1', '--force'])
        client.run(['preendorse', 'for', 'bootstrap2', '--force'])
        client.run(['preendorse', 'for', 'bootstrap3', '--force'])
        client.run(['preendorse', 'for', 'bootstrap4', '--force'])

    def test_transfer(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        transfer = client.transfer(
            TRANSFER_AMOUNT,
            'bootstrap1',
            session['recepient'],
            ['--fee', str(FEE), '--force-low-fee'],
        )
        session["transfer_hash"] = transfer.operation_hash

    def test_endorse(self, sandbox: Sandbox):
        client = sandbox.client(0)
        client.run(['endorse', 'for', 'bootstrap1', '--force'])
        client.run(['endorse', 'for', 'bootstrap2', '--force'])
        client.run(['endorse', 'for', 'bootstrap3', '--force'])
        client.run(['endorse', 'for', 'bootstrap4', '--force'])

    def test_transfer_is_pending(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        pending_ops = client.rpc(
            'get', '/chains/main/mempool/pending_operations'
        )
        # Checking the transfer hash is in pending_operaionts
        assert any(
            map(
                lambda op: op["hash"] == session["transfer_hash"],
                pending_ops["applied"],
            )
        )

    def test_bake_again(self, sandbox: Sandbox, session: dict):
        print("Sleeping")
        time.sleep(2 * MINIMAL_BLOCK_DELAY)
        client = sandbox.client(0)
        client.propose([session['level3_baker']], ['--minimal-timestamp'])
        client.get_head()

    def test_level_again(self, sandbox: Sandbox):
        client = sandbox.client(0)
        head = client.get_head()
        assert head['header']['level'] == 3

    def test_balance(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        recepient_pkh = constants.IDENTITIES[session['recepient']]['identity']
        deposit_mutez = client.frozen_deposits(recepient_pkh)
        balance = client.get_mutez_balance(session['recepient'])
        assert (
            balance
            == utils.mutez_of_tez(INITIAL_BALANCE + TRANSFER_AMOUNT)
            - deposit_mutez
        )

    def test_bake_fail(self, sandbox: Sandbox):
        """Baking with not enough endorsing power should fail in Tenderbake.

        This is the case in sandboxed mode since a lone bootstrap account does
        not have enough slots in the default settings.

        """
        client = sandbox.client(0)
        error_pattern = 'Delegates do not have enough voting power.'
        with utils.assert_run_failure(error_pattern):
            utils.bake(client, bake_for='bootstrap1')


@pytest.mark.incremental
@pytest.mark.tenderbake
class TestManualBakeNullThreshold:
    """Run a number of nodes, and bake when no endorsements are expected"""

    def test_init(self, sandbox: Sandbox):
        sandbox.add_node(0, params=constants.NODE_PARAMS)
        proto_params = protocol.TENDERBAKE_PARAMETERS
        parameters = copy.deepcopy(proto_params)
        parameters['consensus_threshold'] = 0

        protocol.activate(
            sandbox.client(0), parameters=parameters, activate_in_the_past=True
        )

    def test_choose_players(self, sandbox: Sandbox, session: dict):
        # we will make a transfer to someone who is not baking, to simplify
        # the computation of the expected balance
        client = sandbox.client(0)
        session['level2_baker'] = baker_at_round_0(client)
        session['level3_baker'] = baker_at_round_0(client, str(3))
        # recepient should also be different from bootstrap1 because
        # bootstrap1 makes the transfer and we don't want a self-transfer
        for account in ALL_ACOUNTS[1:]:
            if account not in [
                session['level3_baker'],
                session['level2_baker'],
            ]:
                session['recepient'] = account
                break

    def test_bake(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        client.propose([session['level2_baker']], ['--minimal-timestamp'])

    def test_level(self, sandbox: Sandbox):
        client = sandbox.client(0)
        level = client.get_level()
        assert level == 2

    def test_transfer(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        client.transfer(TRANSFER_AMOUNT, 'bootstrap1', session['recepient'])

    def test_bake_again(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        client.propose([session['level3_baker']], ['--minimal-timestamp'])

    def test_level_again(self, sandbox: Sandbox):
        client = sandbox.client(0)
        head = client.get_head()
        assert head['header']['level'] == 3

    def test_balance(self, sandbox: Sandbox, session: dict):
        client = sandbox.client(0)
        balance = client.get_mutez_balance(session['recepient'])
        recepient_pkh = constants.IDENTITIES[session['recepient']]['identity']
        deposit_mutez = client.frozen_deposits(recepient_pkh)
        assert (
            balance
            == utils.mutez_of_tez(INITIAL_BALANCE + TRANSFER_AMOUNT)
            - deposit_mutez
        )
