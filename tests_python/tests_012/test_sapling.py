import json
import re
from os import path
import pytest
from tools import utils, paths, constants
from tools.utils import assert_run_failure
from . import contract_paths
from . import protocol

CONTRACT_PATH = path.join(
    paths.TEZOS_HOME,
    'src',
    protocol.FOLDER,
    'lib_protocol',
    'test',
    'integration',
    'michelson',
)
TX_AMOUNT = 100.0


# TODO: Use a random valid memo size for shielded-tez and others
@pytest.fixture
def contract_path():
    return CONTRACT_PATH


@pytest.fixture(scope="class")
def sandbox(sandbox):
    sandbox.add_node(0, params=constants.NODE_PARAMS)
    parameters = protocol.get_parameters()
    parameters['consensus_threshold'] = 0
    protocol.activate(
        sandbox.client(0), parameters=parameters, activate_in_the_past=True
    )
    return sandbox


@pytest.fixture(scope="class")
def node(sandbox):
    return sandbox.node(0)


@pytest.fixture(scope="class")
def client(sandbox, node):
    client = sandbox.get_new_client(node)
    return client


@pytest.fixture
def mnemonic():
    return [
        "morning",
        "dinosaur",
        "estate",
        "youth",
        "sausage",
        "feature",
        "apology",
        "bullet",
        "square",
        "type",
        "zoo",
        "coyote",
        "extra",
        "fabric",
        "grain",
        "phone",
        "pipe",
        "despair",
        "razor",
        "ranch",
        "blouse",
        "debris",
        "urge",
        "evidence",
    ]


@pytest.fixture
def non_originated_contract_address():
    return "KT1MXuZJJFg4EVpLQeLeuHvznTRiNefh3yCs"


@pytest.fixture
def non_originated_contract_name():
    return "fake-contract"


@pytest.fixture
def key_name():
    return "test_key_name"


# here baker 'account' has baked a block and has sent 'tx_amount'
def check_baker_balance(client, account, tx_amount):
    parameters = dict(protocol.PARAMETERS)
    initial_amount = float(parameters["bootstrap_accounts"][0][1])
    identity = constants.IDENTITIES[account]['identity']
    all_deposits = client.frozen_deposits(identity)
    # sender's balance without fees
    expected_baker_balance = (
        initial_amount / 1000000 - all_deposits / 1000000 - tx_amount
    )
    baker_balance = client.get_balance(account)
    # the fees are assumed to be at most 1 tez
    fees_upper_bound = 3
    assert expected_baker_balance - fees_upper_bound <= baker_balance
    assert baker_balance <= expected_baker_balance


@pytest.mark.client
class TestSaplingWalletImportKey:
    @pytest.fixture
    def client(
        self,
        sandbox,
        node,
        non_originated_contract_name,
        non_originated_contract_address,
    ):
        """
        A client with a pre-registered contract to link the wallet with.
        """
        client = sandbox.get_new_client(node)
        client.remember_contract(
            non_originated_contract_name, non_originated_contract_address
        )
        return client

    def test_import_key_no_force(self, client, mnemonic, key_name):
        """
        Import key without forcing and without pre-existing alias
        """
        client.sapling_import_key(key_name, mnemonic, force=False)

    def test_import_key_force_and_non_previously_saved(
        self,
        client,
        mnemonic,
        key_name,
    ):
        """
        Import key with forcing and without pre-existing alias
        """
        client.sapling_import_key(key_name, mnemonic, force=True)

    def test_import_key_force_and_previously_saved(
        self,
        client,
        mnemonic,
        key_name,
    ):
        """
        Import key with forcing and with pre-existing alias
        """
        client.sapling_import_key(key_name, mnemonic, force=False)
        client.sapling_import_key(key_name, mnemonic, force=True)


class TestSaplingWalletAddressGeneration:
    @pytest.fixture
    def client(self, sandbox, node, key_name, mnemonic):
        """
        A client with a sapling wallet
        """
        client = sandbox.get_new_client(node)
        client.sapling_import_key(key_name, mnemonic, force=False)
        return client

    @pytest.mark.parametrize(
        "expected_address,expected_index",
        [
            (
                "zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGb"
                "HgFLgDrT6J6FYJoHGL3",
                0,
            )
        ],
    )
    def test_generate_first_address_of_key(
        self, client, key_name, expected_address, expected_index
    ):
        result = client.sapling_gen_address(key_name)
        assert result.index == expected_index
        assert result.address == expected_address

    @pytest.mark.parametrize(
        "requested_index,expected_address,expected_index",
        [
            (
                0,
                "zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGb"
                "HgFLgDrT6J6FYJoHGL3",
                0,
            ),
            (
                1,
                "zet13mN26QV67FgPzMSzrigXjKZNMMtCubhi9L3kUePnFYdXqEj"
                "c8pmjw1h2wC6NLZf5F",
                1,
            ),
            (
                2,
                "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
                "WyTrAEqBXmEdHp9woU",
                4,
            ),
            (
                3,
                "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
                "WyTrAEqBXmEdHp9woU",
                4,
            ),
            (
                4,
                "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
                "WyTrAEqBXmEdHp9woU",
                4,
            ),
            (
                100,
                "zet14LmgdAzVrTtQKpeuD3f2y7wFSjXTMEexNNuiEWhGimm25en"
                "xkqmwmbdFsC4y6YmYx",
                100,
            ),
            (
                143534,
                "zet14EWNxZYoHJASHFpcCYSfTfQokWSMzdJeV5SfaGEPDtiYiDC"
                "X5jz8QkMF5jZaK5F4k",
                143536,
            ),
            (
                42,
                "zet143WVQUmNodhSe4ytHL6gvtdXYhRp7bywDWASUFYUCMGAS71"
                "juXT6AyWY89fjg3eZn",
                42,
            ),
            (
                90870987456348,
                "zet13CiUqFsVEr2LdMnyyUQNL3Nh74sa4LdU6V3oD3YfcizbwuF"
                "tftPRYvRrB2zsVaEw1",
                90870987456348,
            ),
        ],
    )
    def test_generate_address_with_address_index(
        self,
        client,
        key_name,
        expected_address,
        expected_index,
        requested_index,
    ):
        result = client.sapling_gen_address(key_name, index=requested_index)
        assert result.index == expected_index
        assert result.address == expected_address


@pytest.mark.client
@pytest.mark.contract
@pytest.mark.incremental
class TestSaplingShieldedTez:
    """
    Tests involving sapling key management and shielded transactions using
    the shielded tez example contract.
    """

    @pytest.fixture
    def contract_path(self):
        return path.join(CONTRACT_PATH, 'contracts', 'sapling_contract.tz')

    @pytest.fixture
    def contract_name(self):
        return "sapling"

    @pytest.fixture(scope="session")
    def tmpdir(self, tmpdir_factory):
        """
        Temporary directory. Forged transactions will be saved
        in this directory.
        FIXME/IMPROVEME: tmpdir_factory is a fixture provided by pytest. It is
        session-scoped, then the fixture tmpdir must be session-scoped.
        Would be nice to have a class-scoped fixture.
        """
        tmpdir = tmpdir_factory.mktemp("sapling_transactions_shielded_tez")
        return tmpdir

    def test_originate_sapling_contract(
        self, contract_path, client, session, contract_name
    ):
        sender = "bootstrap1"
        origination = client.originate(
            contract_name=contract_name,
            amount=0,
            sender=sender,
            contract=contract_path,
            args=["--init", "{ }", "--burn-cap", "3.0"],
        )
        session["contract_address"] = origination.contract
        utils.bake(client, bake_for=sender)
        assert utils.check_block_contains_operations(
            client,
            [origination.operation_hash],
        )

    def test_generate_bob(self, client, session, contract_name):
        key_name = "bob"
        result = client.sapling_gen_key(key_name=key_name)
        client.sapling_use_key_for_contract(
            key_name, contract_name, memo_size=8
        )
        session['bob_mnemonic'] = result.mnemonic

    def test_list_keys_bob(self, client):
        keys = client.sapling_list_keys()
        assert keys == ["bob"]

    def test_list_keys_with_alice_and_bob(self, client, contract_name):
        """
        NB: another key (ali) is generated in the test, but the mnemonic
        is not saved.
        We add this test to verify the list keys command orders alphabetically
        """
        key_name = "ali"
        client.sapling_gen_key(key_name=key_name)
        client.sapling_use_key_for_contract(
            key_name=key_name, contract_name=contract_name
        )
        keys = client.sapling_list_keys()
        assert keys == ["ali", "bob"]

    def test_generate_bob_address_0(self, client, session):
        result = client.sapling_gen_address(
            key_name="bob",
        )
        session['last_address_index'] = result.index
        session['bob_address_0'] = result.address

    def test_generate_bob_address_1(self, client, session):
        result = client.sapling_gen_address(
            key_name="bob",
        )
        assert result.index > session['last_address_index']
        session['bob_address_1'] = result.address

    def test_check_bob_balance(self, client, contract_name):
        result = client.sapling_get_balance(
            key_name="bob",
            contract_name=contract_name,
        )
        assert result.balance == 0

    def test_shield_bob_address_0(self, client, session, contract_name):
        client.sapling_shield(
            amount=TX_AMOUNT,
            src="bootstrap2",
            dest=session['bob_address_0'],
            contract=contract_name,
            args=["--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap1")
        check_baker_balance(client, "bootstrap2", TX_AMOUNT)
        bob_balance = client.sapling_get_balance(
            key_name="bob", contract_name=contract_name
        ).balance
        assert bob_balance == TX_AMOUNT

    def test_check_contract_balance_after_shielding(
        self, client, contract_name
    ):
        assert client.get_balance(contract_name) == TX_AMOUNT

    def test_regenerate_bob_from_mnemonic(self, client, session):
        # Overwrite the old 'bob' key with one restored from the mnemonic.
        key_name = "bob"
        client.sapling_import_key(
            key_name=key_name,
            mnemonic=session['bob_mnemonic'],
            force=True,
        )

    def test_derive_alice(self, client, contract_name):
        result = client.sapling_derive_key(
            source_key_name='bob',
            target_key_name='alice',
            contract_name=contract_name,
            index='0',
        )
        assert result.path == '0/0'

    def test_derive_yves(self, client, contract_name):
        result = client.sapling_derive_key(
            source_key_name='bob',
            target_key_name='yves',
            contract_name=contract_name,
            index='1',
        )
        assert result.path == '0/1'

    def test_generate_alice_address_0(self, client, session):
        result = client.sapling_gen_address(
            key_name="alice",
        )
        session['alice_address_0'] = result.address

    def test_alice_shields_money_insufficient_funds(
        self, client, session, contract_name
    ):
        bootstrap3 = constants.IDENTITIES['bootstrap3']['identity']
        alice_balance = int(client.get_balance('bootstrap3'))
        amount = 2 * alice_balance
        with assert_run_failure(
            r"Balance of contract {} too low \({}\) to spend {}".format(
                bootstrap3, alice_balance, amount
            )
        ):
            client.sapling_shield(
                amount=amount,
                src="bootstrap3",
                dest=session['alice_address_0'],
                contract=contract_name,
                args=["--burn-cap", "3.0"],
            )

    def test_alice_shields_money(self, client, session, contract_name):
        client.sapling_shield(
            amount=TX_AMOUNT,
            src="bootstrap3",
            dest=session['alice_address_0'],
            contract=contract_name,
            args=[
                "--burn-cap",
                "3.0",
            ],
        )
        utils.bake(client, bake_for="bootstrap1")
        check_baker_balance(client, "bootstrap3", TX_AMOUNT)
        alice_balance = client.sapling_get_balance(
            key_name="alice", contract_name=contract_name
        ).balance
        assert alice_balance == TX_AMOUNT

    @pytest.mark.parametrize(
        "transaction_file,use_json",
        [
            ("sapling_transaction.bin", False),
            ("sapling_transaction.json", True),
        ],
    )
    def test_forge_alice_to_bob_insufficient_funds(
        self,
        client,
        tmpdir,
        contract_name,
        session,
        transaction_file,
        use_json,
    ):
        transaction_file = f'{tmpdir}/{transaction_file}'
        amount = 2100000000.0
        account = 'alice'
        additional_args = []
        if use_json:
            additional_args += ["--json"]

        with assert_run_failure(
            r"Balance too low \({}\) to spend {}".format(100, int(amount))
        ):
            client.sapling_forge_transaction(
                amount=amount,
                src=account,
                dest=session['bob_address_1'],
                contract=contract_name,
                file=transaction_file,
                args=additional_args,
            )

    def test_forge_alice_to_bob_address_0(
        self, tmpdir, session, client, contract_name
    ):
        transaction_file = f'{tmpdir}/sapling_transaction0.bin'
        client.sapling_forge_transaction(
            amount=TX_AMOUNT,
            src='alice',
            dest=session['bob_address_0'],
            contract=contract_name,
            file=transaction_file,
        )

    def test_forge_alice_to_bob_address_1_binary_format(
        self, tmpdir, session, client, contract_name
    ):
        transaction_file = f'{tmpdir}/sapling_transaction1.bin'
        client.sapling_forge_transaction(
            amount=50.0,
            src='alice',
            dest=session['bob_address_1'],
            contract=contract_name,
            file=transaction_file,
        )

    @pytest.mark.parametrize(
        "key_name,expected_balance", [("alice", TX_AMOUNT), ("bob", TX_AMOUNT)]
    )
    def test_check_sapling_balances_post_forge_binary_format(
        self, client, contract_name, key_name, expected_balance
    ):
        result = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name,
        )
        assert result.balance == expected_balance

    def test_submit_alice_to_bob_address_1_binary_format(
        self, client, tmpdir, contract_name
    ):
        transaction_file = f'{tmpdir}/sapling_transaction1.bin'
        additional_args = ["--burn-cap", "3.0"]
        client.sapling_submit(
            file=transaction_file,
            fee_payer='bootstrap2',
            contract=contract_name,
            args=additional_args,
        )
        utils.bake(client, bake_for="bootstrap2")

    @pytest.mark.parametrize(
        "key_name,expected_balance", [("alice", 50.0), ("bob", 150.0)]
    )
    def test_check_sapling_balances_after_successfull_transaction_in_binary(
        self, client, contract_name, key_name, expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name, contract_name=contract_name
        ).balance
        assert balance == expected_balance

    def test_forge_alice_to_bob_address_1_json_format(
        self, tmpdir, session, client, contract_name
    ):
        transaction_file = f'{tmpdir}/sapling_transaction1.json'
        client.sapling_forge_transaction(
            amount=50.0,
            src='alice',
            dest=session['bob_address_1'],
            contract=contract_name,
            file=transaction_file,
            args=['--json'],
        )
        # Try to load the file as JSON. Must not fail.
        with open(transaction_file, "r") as file_descriptor:
            json.load(file_descriptor)

    @pytest.mark.parametrize(
        "key_name,expected_balance", [("alice", 50.0), ("bob", 150.0)]
    )
    def test_check_sapling_balances_post_forge_json_format(
        self, client, contract_name, key_name, expected_balance
    ):
        result = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name,
        )
        assert result.balance == expected_balance

    def test_submit_alice_to_bob_address_1_json_format(
        self, client, tmpdir, contract_name
    ):
        transaction_file = f'{tmpdir}/sapling_transaction1.json'
        additional_args = ["--burn-cap", "3.0", "--json"]
        client.sapling_submit(
            file=transaction_file,
            fee_payer='bootstrap2',
            contract=contract_name,
            args=additional_args,
        )
        utils.bake(client, bake_for="bootstrap2")

    @pytest.mark.parametrize(
        "key_name,expected_balance", [("alice", 0.0), ("bob", 200.0)]
    )
    def test_check_sapling_balances_after_successfull_transaction_in_json(
        self, client, contract_name, key_name, expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name, contract_name=contract_name
        ).balance
        assert balance == expected_balance

    @pytest.mark.parametrize(
        "transaction_file,use_json",
        [
            ("sapling_transaction0.bin", False),
            # ("sapling_transaction0.json", True),
        ],
    )
    def test_submit_alice_to_bob0(
        self, client, transaction_file, use_json, tmpdir, contract_name
    ):
        transaction_file = f'{tmpdir}/{transaction_file}'
        additional_args = ["--burn-cap", "3.0"]
        if use_json:
            additional_args.append("--json")

        with assert_run_failure(r'transfer simulation failed'):
            client.sapling_submit(
                file=transaction_file,
                fee_payer='bootstrap2',
                contract=contract_name,
                args=additional_args,
            )

    @pytest.mark.parametrize(
        "requested_token,real_balance,key_name",
        [
            (2100000000, 200, "bob"),
            (300, 200, "bob"),
            (2100000000, 0, "alice"),
            (100, 0, "alice"),
        ],
    )
    def test_unshields_money_insufficient_funds(
        self, client, contract_name, requested_token, real_balance, key_name
    ):
        with assert_run_failure(
            r'Balance too low \({}\) to spend {}'.format(
                real_balance, requested_token
            )
        ):
            client.sapling_unshield(
                amount=requested_token,
                src=key_name,
                dest="bootstrap4",
                contract=contract_name,
                args=["--burn-cap", "3.0"],
            )

    def test_bob_unshields_money(self, client, contract_name):
        bootstrap4_prev_balance = client.get_balance('bootstrap4')
        amount = 90.0
        client.sapling_unshield(
            amount=amount,
            src="bob",
            dest="bootstrap4",
            contract=contract_name,
            args=["--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap2")
        bob_balance = client.sapling_get_balance(
            key_name="bob", contract_name=contract_name
        ).balance
        assert bob_balance == 200.0 - amount
        bootstrap4_balance = client.get_balance("bootstrap4")
        # The receiver pays fees by default so it will not get the full amount,
        # but still, it should have more than before
        assert bootstrap4_balance >= bootstrap4_prev_balance
        assert bootstrap4_balance <= bootstrap4_prev_balance + amount

    def test_check_state_with_another_client(
        self, sandbox, node, contract_name, session
    ):
        client = sandbox.get_new_client(node)
        client.remember_contract(contract_name, session["contract_address"])
        key_name = "bob"
        # Restore bob's key from mnemonic:
        client.sapling_import_key(
            key_name=key_name,
            mnemonic=session['bob_mnemonic'],
        )
        client.sapling_use_key_for_contract(
            key_name, contract_name, memo_size=8
        )
        # Check Bob's balance again, it should be the same:
        bob_balance = client.sapling_get_balance(
            key_name=key_name, contract_name=contract_name
        ).balance
        assert bob_balance == 110.0

    @pytest.mark.parametrize(
        "transparent_signer,baker", [("bootstrap4", "bootstrap2")]
    )
    def test_shielded_transfer_using_non_sapling_transfer_method(
        self, client, contract_name, session, transparent_signer, baker, tmpdir
    ):
        transaction_filename = f'{tmpdir}/sapling_transaction_2.bin'
        client.sapling_forge_transaction(
            amount=10.0,
            src='bob',
            dest=session['bob_address_1'],
            contract=contract_name,
            file=transaction_filename,
            args=[],
        )
        with open(transaction_filename, "r") as file_descriptor:
            content = re.sub(r'\s+', ' ', file_descriptor.read())
            client.transfer(
                0,
                giver=transparent_signer,
                receiver=contract_name,
                args=[
                    "--arg",
                    '{Pair %s None }' % content,
                    "--burn-cap",
                    "3.0",
                ],
            )
        utils.bake(client, bake_for=baker)

    @pytest.mark.parametrize(
        "key_name,expected_balance", [("alice", 0.0), ("bob", 110.0)]
    )
    def test_check_sapling_balances_after_calling_smart_contract(
        self, client, contract_name, key_name, expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name, contract_name=contract_name
        ).balance
        assert balance == expected_balance


class TestSaplingMemoSize:
    @pytest.fixture(scope="class")
    def contract_name(self):
        return "sapling_memo_size"

    @pytest.fixture(scope="class")
    def contract_generator(self):
        def generator(memo_size):
            return '''
parameter unit;
storage (sapling_state %s);
code {
      DROP;
      SAPLING_EMPTY_STATE %s;
      NIL operation;
      PAIR;
     }
''' % (
                memo_size,
                memo_size,
            )

        return generator

    @pytest.mark.parametrize("memo_size", [0, 1, 10, 42, 100, 65535])
    def test_originate_with_valid_size_and_update_with_valid_size(
        self, client, memo_size, contract_name, tmpdir, contract_generator
    ):
        contract_path = tmpdir.join("c.tz")
        contract_path.write(contract_generator(memo_size))
        sender = "bootstrap1"
        client.originate(
            contract_name=contract_name,
            amount=0,
            sender=sender,
            contract=str(contract_path),
            args=["--init", '{ }', "--burn-cap", "3.0", "--force"],
        )
        utils.bake(client, bake_for="bootstrap1")
        client.transfer(
            0,
            giver="bootstrap1",
            receiver=contract_name,
            args=["--arg", "Unit", "--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap1")

    @pytest.mark.parametrize("memo_size", [-1, 65536, 65598909, 908923434])
    def test_originate_with_invalid_size(
        self,
        client,
        memo_size,
        contract_path,
        contract_name,
        contract_generator,
        tmpdir,
    ):
        contract_path = tmpdir.join("c.tz")
        contract_path.write(contract_generator(memo_size))
        sender = "bootstrap1"
        err = r"expected a positive 16-bit integer"
        with assert_run_failure(err):
            client.originate(
                contract_name=contract_name,
                amount=0,
                sender=sender,
                contract=str(contract_path),
                args=["--init", "{ }", "--burn-cap", "3.0", "--force"],
            )


@pytest.mark.incremental
class TestSaplingStateCorruption:
    @pytest.fixture(scope="session")
    def tmpdir(self, tmpdir_factory):
        """
        Temporary directory. Forged transactions will be saved
        in this directory.
        FIXME/IMPROVEME: tmpdir_factory is a fixture provided by pytest. It is
        session-scoped, then the fixture tmpdir must be session-scoped.
        Would be nice to have a class-scoped fixture.
        """
        tmpdir = tmpdir_factory.mktemp("sapling_transactions_shielded_tez")
        return tmpdir

    def test_push_sapling_state_with_id_is_forbidden(
        self, client, contract_path
    ):
        contract_name = (
            f"{contract_path}/contracts/sapling_push_sapling_state.tz"
        )
        sender = "bootstrap1"
        msg = r"big_map or sapling_state type not expected here"
        with assert_run_failure(msg):
            client.originate(
                contract_name="push_sapling_state",
                amount=0,
                sender=sender,
                contract=contract_name,
                args=["--init", "Unit", "--burn-cap", "3.0"],
            )

    def test_originate_with_empty(self, client):
        """
        Makes sure sapling state with id 0 exists
        """
        contract = path.join(
            contract_paths.OPCODES_CONTRACT_PATH, "sapling_empty_state.tz"
        )
        client.originate(
            amount=0,
            sender="bootstrap1",
            contract=contract,
            contract_name="sapling_empty_state",
            args=["--init", "{}", "--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap1")

    def test_originate_with_id_is_forbidden(self, client):
        contract = path.join(
            contract_paths.OPCODES_CONTRACT_PATH, "sapling_empty_state.tz"
        )
        with assert_run_failure(r'Unexpected forged value'):
            client.originate(
                amount=0,
                sender="bootstrap1",
                contract=contract,
                contract_name="sapling_empty_state2",
                args=["--init", "0", "--burn-cap", "3.0"],
            )


class TestSaplingDifferentMemosize:
    """
    Deploy a sapling contract using a sapling state with a memo size N and
    create transactions with a memo size of M
    """

    @pytest.fixture
    def contract_path(self):
        return f'{CONTRACT_PATH}/contracts/sapling_contract.tz'

    def test_shield_with_different_memo_size(self, contract_path, client):
        contract_name = "sapling_memo_size_different"
        implicit_account = "bootstrap1"
        contract_address = client.originate(
            contract_name=contract_name,
            amount=0,
            sender=implicit_account,
            contract=contract_path,
            args=["--init", "{ }", "--burn-cap", "3.0"],
        ).contract
        utils.bake(client, bake_for=implicit_account)
        client.sapling_gen_key(key_name='alice')
        client.sapling_use_key_for_contract(
            'alice', contract_name, memo_size=16
        )
        address = client.sapling_gen_address(key_name='alice').address
        # Key was registered with a memo_size of 16, it should fail
        with assert_run_failure(r"Memo sizes of two sapling states"):
            client.sapling_shield(
                amount=TX_AMOUNT,
                src=implicit_account,
                dest=address,
                contract=contract_address,
                args=["--burn-cap", "3.0"],
            )


class TestSaplingRightMemosize:
    """
    Deploy a sapling contract using a sapling state with a memo size N and
    create transactions with a memo size of N and diverse messages
    """

    @pytest.fixture
    def contract_path(self):
        return f'{CONTRACT_PATH}/contracts/sapling_contract.tz'

    def test_shield_with_same_memo_size(self, contract_path, client):
        contract_name = "sapling_memo_size_same"
        implicit_account = "bootstrap1"
        contract_address = client.originate(
            contract_name=contract_name,
            amount=0,
            sender=implicit_account,
            contract=contract_path,
            args=["--init", "{ }", "--burn-cap", "3.0"],
        ).contract
        utils.bake(client, bake_for=implicit_account)
        client.sapling_gen_key(key_name='alice')
        client.sapling_use_key_for_contract('alice', contract_name, memo_size=8)
        address = client.sapling_gen_address(key_name='alice').address
        # Should pass since memo-sizes are equal and message is
        # filled with 0's
        client.sapling_shield(
            amount=TX_AMOUNT,
            src=implicit_account,
            dest=address,
            contract=contract_address,
            args=["--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap2")
        # Deriving a new key should work as well since
        # the memo-size is kept
        client.sapling_derive_key(
            source_key_name='alice',
            target_key_name='bob',
            contract_name=contract_name,
            index=10,
        )
        address_derived = client.sapling_gen_address(key_name='bob').address
        client.sapling_shield(
            amount=TX_AMOUNT,
            src=implicit_account,
            dest=address_derived,
            contract=contract_address,
            args=["--burn-cap", "3.0"],
        )
        utils.bake(client, bake_for="bootstrap2")
        # Now with a too short message
        client.sapling_shield(
            amount=TX_AMOUNT,
            src=implicit_account,
            dest=address,
            contract=contract_address,
            args=["--burn-cap", "3.0", "--message", "aB"],
        )
        utils.bake(client, bake_for="bootstrap2")
        # Now with a right length message
        client.sapling_shield(
            amount=TX_AMOUNT,
            src=implicit_account,
            dest=address,
            contract=contract_address,
            args=["--burn-cap", "3.0", "--message", "aBbf19F00a"],
        )
        utils.bake(client, bake_for="bootstrap2")
        # Now with a too long message
        client.sapling_shield(
            amount=TX_AMOUNT,
            src=implicit_account,
            dest=address,
            contract=contract_address,
            args=["--burn-cap", "3.0", "--message", "aBbf19F00aaBbf19F00aC"],
        )
        utils.bake(client, bake_for="bootstrap2")
