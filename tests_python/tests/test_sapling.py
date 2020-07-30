import json
import re
import pytest
from tools import utils, paths
from tools.utils import assert_run_failure


@pytest.fixture(scope="class")
def sandbox(sandbox):
    sandbox.add_node(0, params=['--connections', '1'])
    utils.activate_alpha(sandbox.client(0))
    return sandbox


@pytest.fixture(scope="class")
def node(sandbox):
    return sandbox.node(0)


@pytest.fixture
def mnemonic():
    return [
        "morning", "dinosaur", "estate", "youth", "sausage", "feature",
        "apology", "bullet", "square", "type", "zoo", "coyote", "extra",
        "fabric", "grain", "phone", "pipe", "despair", "razor", "ranch",
        "blouse", "debris", "urge", "evidence"
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


@pytest.mark.client
class TestSaplingWalletImportKey:
    @pytest.fixture
    def client(self, sandbox, node, non_originated_contract_name,
               non_originated_contract_address):
        """
        A client with a pre-registered contract to link the wallet with.
        """
        client = sandbox.get_new_client(node)
        client.remember_contract(non_originated_contract_name,
                                 non_originated_contract_address)
        return client

    def test_import_key_no_force(self, client, mnemonic, key_name):
        """
        Import key without forcing and without pre-existing alias
        """
        client.sapling_import_key(key_name,
                                  mnemonic,
                                  force=False)

    def test_import_key_force_and_non_previously_saved(
            self, client, mnemonic, key_name,
    ):
        """
        Import key with forcing and without pre-existing alias
        """
        client.sapling_import_key(key_name,
                                  mnemonic,
                                  force=True)

    def test_import_key_force_and_previously_saved(
            self, client, mnemonic, key_name,
    ):
        """
        Import key with forcing and with pre-existing alias
        """
        client.sapling_import_key(key_name,
                                  mnemonic,
                                  force=False)
        client.sapling_import_key(key_name,
                                  mnemonic,
                                  force=True)


class TestSaplingWalletAddressGeneration:
    @pytest.fixture
    def client(self, sandbox, node, key_name,
               mnemonic):
        """
        A client with a sapling wallet
        """
        client = sandbox.get_new_client(node)
        client.sapling_import_key(key_name,
                                  mnemonic,
                                  force=False)
        return client

    @pytest.mark.parametrize(
        "expected_address,expected_index",
        [("zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGb"
          "HgFLgDrT6J6FYJoHGL3", 0)]
    )
    def test_generate_first_address_of_key(self, client, key_name,
                                           expected_address,
                                           expected_index):
        result = client.sapling_gen_address(key_name)
        assert result.index == expected_index
        assert result.address == expected_address

    @pytest.mark.parametrize(
        "requested_index,expected_address,expected_index",
        [
            (0,
             "zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGb"
             "HgFLgDrT6J6FYJoHGL3",
             0),
            (1,
             "zet13mN26QV67FgPzMSzrigXjKZNMMtCubhi9L3kUePnFYdXqEj"
             "c8pmjw1h2wC6NLZf5F",
             1),
            (2,
             "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
             "WyTrAEqBXmEdHp9woU",
             4),
            (3,
             "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
             "WyTrAEqBXmEdHp9woU",
             4),
            (4,
             "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7"
             "WyTrAEqBXmEdHp9woU",
             4),
            (100,
             "zet14LmgdAzVrTtQKpeuD3f2y7wFSjXTMEexNNuiEWhGimm25en"
             "xkqmwmbdFsC4y6YmYx",
             100),
            (143534,
             "zet14EWNxZYoHJASHFpcCYSfTfQokWSMzdJeV5SfaGEPDtiYiDC"
             "X5jz8QkMF5jZaK5F4k",
             143536),
            (42,
             "zet143WVQUmNodhSe4ytHL6gvtdXYhRp7bywDWASUFYUCMGAS71"
             "juXT6AyWY89fjg3eZn",
             42),
            (90870987456348,
             "zet13CiUqFsVEr2LdMnyyUQNL3Nh74sa4LdU6V3oD3YfcizbwuF"
             "tftPRYvRrB2zsVaEw1",
             90870987456348),
        ])
    def test_generate_address_with_address_index(
            self, client, key_name, expected_address, expected_index,
            requested_index):
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
        return f'{paths.TEZOS_HOME}/src/lib_sapling/test/sapling_contract.tz'

    @pytest.fixture(scope="class")
    def client(self, sandbox, node):
        client = sandbox.get_new_client(node)
        utils.remember_baker_contracts(client)
        return client

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

    def test_originate_sapling_contract(self, contract_path, client, session,
                                        contract_name):
        sender = "bootstrap1"
        origination = client.originate(
            contract_name=contract_name,
            amount=0,
            sender=sender,
            contract=contract_path,
            args=["--init", "{}", "--burn-cap", "3.0"]
        )
        session["contract_address"] = origination.contract
        client.bake("baker1", ["--minimal-timestamp"])
        assert utils.check_block_contains_operations(
            client,
            [origination.operation_hash],
        )

    def test_generate_bob(self, client, session, contract_name):
        key_name = "bob"
        result = client.sapling_gen_key(key_name=key_name)
        client.sapling_use_key_for_contract(key_name, contract_name)
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
        client.sapling_use_key_for_contract(key_name=key_name,
                                            contract_name=contract_name)
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
            amount=100.0,
            src="bootstrap2",
            dest=session['bob_address_0'],
            contract=contract_name,
            args=["--burn-cap", "3.0"],
        )
        client.bake("baker2", ["--minimal-timestamp"])
        bootstrap2_balance = client.get_balance("bootstrap2")
        # Fees
        assert 3999898 <= bootstrap2_balance <= 3999899
        bob_balance = client.sapling_get_balance(
            key_name="bob",
            contract_name=contract_name).balance
        assert bob_balance == 100

    def test_check_contract_balance_after_shielding(self, client,
                                                    contract_name):
        assert client.get_balance(contract_name) == 100.0

    def test_regenerate_bob_from_mnemonic(self, client,
                                          session):
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
            index='0'
        )
        assert result.path == '0/0'

    def test_derive_yves(self, client, contract_name):
        result = client.sapling_derive_key(
            source_key_name='bob',
            target_key_name='yves',
            contract_name=contract_name,
            index='1'
        )
        assert result.path == '0/1'

    def test_generate_alice_address_0(self, client, session):
        result = client.sapling_gen_address(
            key_name="alice",
        )
        session['alice_address_0'] = result.address

    def test_alice_shields_money_insufficient_funds(self, client, session,
                                                    contract_name):
        with assert_run_failure(r'too low \(4000000\) to spend 2100000000'):
            client.sapling_shield(
                amount=2100000000.0,
                src="bootstrap3",
                dest=session['alice_address_0'],
                contract=contract_name,
                args=["--burn-cap", "3.0"],
            )

    def test_alice_shields_money(self, client, session, contract_name):
        client.sapling_shield(
            amount=100.0,
            src="bootstrap3",
            dest=session['alice_address_0'],
            contract=contract_name,
            args=["--burn-cap", "3.0"],
        )
        client.bake("baker3", ["--minimal-timestamp"])
        bootstrap3_balance = client.get_balance("bootstrap3")
        assert bootstrap3_balance >= 3999899
        assert bootstrap3_balance <= 3999900
        alice_balance = client.sapling_get_balance(
            key_name="alice",
            contract_name=contract_name).balance
        assert alice_balance == 100.0

    @pytest.mark.parametrize("transaction_file,use_json", [
        ("sapling_transaction.bin", False),
        ("sapling_transaction.json", True),
    ])
    def test_forge_alice_to_bob_insufficient_funds(self,
                                                   client,
                                                   tmpdir, contract_name,
                                                   session,
                                                   transaction_file,
                                                   use_json):
        transaction_file = f'{tmpdir}/{transaction_file}'
        amount = 2100000000.0
        account = 'alice'
        additional_args = ["--json"] if use_json else None

        with assert_run_failure(
                r"Balance too low \({}\) to spend {}".
                format(100, int(amount))):
            client.sapling_forge_transaction(
                amount=amount,
                src=account,
                dest=session['bob_address_1'],
                contract=contract_name,
                file=transaction_file,
                args=additional_args,
            )

    def test_forge_alice_to_bob_address_0(self, tmpdir, session,
                                          client,
                                          contract_name):
        transaction_file = f'{tmpdir}/sapling_transaction0.bin'
        client.sapling_forge_transaction(
            amount=100.0,
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

    @pytest.mark.parametrize("key_name,expected_balance", [
        ("alice", 100.0),
        ("bob", 100.0)
    ])
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
        client.bake("baker2", ["--minimal-timestamp"])

    @pytest.mark.parametrize("key_name,expected_balance", [
        ("alice", 50.0),
        ("bob", 150.0)
    ])
    def test_check_sapling_balances_after_successfull_transaction_in_binary(
            self,
            client,
            contract_name,
            key_name,
            expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name).balance
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
            args=["--json"],
        )
        # Try to load the file as JSON. Must not fail.
        with open(transaction_file, "r") as file_descriptor:
            json.load(file_descriptor)

    @pytest.mark.parametrize("key_name,expected_balance", [
        ("alice", 50.0),
        ("bob", 150.0)
    ])
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
        client.bake("baker2", ["--minimal-timestamp"])

    @pytest.mark.parametrize("key_name,expected_balance", [
        ("alice", 0.0),
        ("bob", 200.0)
    ])
    def test_check_sapling_balances_after_successfull_transaction_in_json(
            self,
            client,
            contract_name,
            key_name,
            expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name).balance
        assert balance == expected_balance

    @pytest.mark.parametrize("transaction_file,use_json", [
        ("sapling_transaction0.bin", False),
        # ("sapling_transaction0.json", True),
    ])
    def test_submit_alice_to_bob0(self, client, transaction_file,
                                  use_json, tmpdir, contract_name):
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

    @pytest.mark.parametrize("requested_token,real_balance,key_name", [
        (2100000000, 200, "bob"),
        (300, 200, "bob"),
        (2100000000, 0, "alice"),
        (100, 0, "alice"),
    ])
    def test_unshields_money_insufficient_funds(self, client,
                                                contract_name,
                                                requested_token,
                                                real_balance,
                                                key_name):
        with assert_run_failure(
                r'Balance too low \({}\) to spend {}'.
                format(real_balance, requested_token)):
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
        client.bake("baker2", ['--minimal-timestamp'])
        bob_balance = client.sapling_get_balance(
            key_name="bob",
            contract_name=contract_name).balance
        assert bob_balance == 200.0 - amount
        bootstrap4_balance = client.get_balance("bootstrap4")
        # The receiver pays fees by default so it will not get the full amount,
        # but still, it should have more than before
        assert bootstrap4_balance >= bootstrap4_prev_balance
        assert bootstrap4_balance <= bootstrap4_prev_balance + amount

    def test_check_state_with_another_client(self, sandbox, node,
                                             contract_name, session):
        client = sandbox.get_new_client(node)
        client.remember_contract(contract_name, session["contract_address"])
        key_name = "bob"
        # Restore bob's key from mnemonic:
        client.sapling_import_key(
            key_name=key_name,
            mnemonic=session['bob_mnemonic'],
        )
        client.sapling_use_key_for_contract(key_name, contract_name)
        # Check Bob's balance again, it should be the same:
        bob_balance = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name).balance
        assert bob_balance == 110.0

    @pytest.mark.parametrize("transparent_signer,baker", [
        ("bootstrap4", "baker2")
    ])
    def test_shielded_transfer_using_non_sapling_transfer_method(
            self, client, contract_name, session, transparent_signer,
            baker, tmpdir
    ):
        transaction_filename = f'{tmpdir}/sapling_transaction_2.bin'
        client.sapling_forge_transaction(
            amount=10.0,
            src='bob',
            dest=session['bob_address_1'],
            contract=contract_name,
            file=transaction_filename,
        )
        with open(transaction_filename, "r") as file_descriptor:
            content = re.sub(r'\s+', ' ', file_descriptor.read())
            client.transfer(
                0,
                giver=transparent_signer,
                receiver=contract_name,
                args=["--arg", '(Pair %s None)' % content, "--burn-cap", "3.0"]
            )
        client.bake(baker, ["--minimal-timestamp"])

    @pytest.mark.parametrize("key_name,expected_balance", [
        ("alice", 0.0),
        ("bob", 110.0)
    ])
    def test_check_sapling_balances_after_calling_smart_contract(
            self,
            client,
            contract_name,
            key_name,
            expected_balance
    ):
        balance = client.sapling_get_balance(
            key_name=key_name,
            contract_name=contract_name).balance
        assert balance == expected_balance
