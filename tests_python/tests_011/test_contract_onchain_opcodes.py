from os import path

import pytest
from tools.client_regression import ClientRegression
from tools import paths
from tools.utils import (
    assert_run_failure,
    assert_storage_contains,
    bake,
    init_with_transfer,
    assert_balance,
)
from tools.constants import IDENTITIES
from .contract_paths import OPCODES_CONTRACT_PATH, MINI_SCENARIOS_CONTRACT_PATH
from . import protocol

KEY1 = 'foo'
KEY2 = 'bar'


@pytest.mark.incremental
@pytest.mark.slow
@pytest.mark.contract
@pytest.mark.regression
class TestContractOnchainOpcodes:
    """Tests for individual opcodes that requires origination."""

    def test_gen_keys(self, client_regtest_scrubbed: ClientRegression):
        """Add keys used by later tests."""
        client = client_regtest_scrubbed
        client.gen_key(KEY1)
        client.gen_key(KEY2)

    def test_store_input(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        client.transfer(1000, "bootstrap1", KEY1, ['--burn-cap', '0.257'])
        bake(client)

        client.transfer(2000, "bootstrap1", KEY2, ['--burn-cap', '0.257'])
        bake(client)

        assert_balance(client, KEY1, 1000)
        assert_balance(client, KEY2, 2000)

        # Create a contract and transfer 100 ꜩ to it
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'store_input.tz'),
            '""',
            100,
            'bootstrap1',
        )

        client.transfer(
            100,
            "bootstrap1",
            "store_input",
            ["-arg", '"abcdefg"', '--burn-cap', '10'],
        )
        bake(client)

        assert_balance(client, "store_input", 200)

        assert_storage_contains(client, "store_input", '"abcdefg"')

        client.transfer(
            100,
            "bootstrap1",
            "store_input",
            ["-arg", '"xyz"', '--burn-cap', '10'],
        )
        bake(client)

        assert_storage_contains(client, "store_input", '"xyz"')

    def test_transfer_amount(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'transfer_amount.tz'),
            '0',
            100,
            'bootstrap1',
        )

        client.transfer(
            500,
            "bootstrap1",
            'transfer_amount',
            ['-arg', 'Unit', '--burn-cap', '10'],
        )

        bake(client)

        assert_storage_contains(client, "transfer_amount", '500000000')

    def test_now(self, client_regtest_scrubbed: ClientRegression):
        # Regtest is disabled for this test, since one would need to
        # scrub storage for this one as it changes (the timestamp)
        # on every run.
        client = client_regtest_scrubbed
        client.set_regtest(None)

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'store_now.tz'),
            '"2017-07-13T09:19:01Z"',
            100,
            'bootstrap1',
        )

        client.transfer(
            500, "bootstrap1", 'store_now', ['-arg', 'Unit', '--burn-cap', '10']
        )
        bake(client)

        assert_storage_contains(
            client, 'store_now', f'"{protocol.get_now(client)}"'
        )

    def test_transfer_tokens(self, client_regtest_scrubbed: ClientRegression):
        """Tests TRANSFER_TOKENS."""
        client = client_regtest_scrubbed
        client.originate(
            'test_transfer_account1',
            100,
            'bootstrap1',
            path.join(OPCODES_CONTRACT_PATH, 'noop.tz'),
            ['--burn-cap', '10'],
        )
        bake(client)

        client.originate(
            'test_transfer_account2',
            20,
            'bootstrap1',
            path.join(OPCODES_CONTRACT_PATH, 'noop.tz'),
            ['--burn-cap', '10'],
        )
        bake(client)

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'transfer_tokens.tz'),
            'Unit',
            1000,
            'bootstrap1',
        )

        assert_balance(client, 'test_transfer_account1', 100)

        account1_addr = client.get_contract_address('test_transfer_account1')
        client.transfer(
            100,
            'bootstrap1',
            'transfer_tokens',
            ['-arg', f'"{account1_addr}"', '--burn-cap', '10'],
        )
        bake(client)

        # Why isn't this 200 ꜩ? Baking fee?
        assert_balance(client, 'test_transfer_account1', 200)

        account2_addr = client.get_contract_address('test_transfer_account2')
        client.transfer(
            100,
            'bootstrap1',
            'transfer_tokens',
            ['-arg', f'"{account2_addr}"', '--burn-cap', '10'],
        )
        bake(client)

        assert_balance(client, 'test_transfer_account2', 120)

    def test_self(self, client_regtest_scrubbed: ClientRegression):
        # Regtest is disabled for this test, since one would need to
        # scrub storage for this one as it changes (the contract
        # address) on every run.
        client = client_regtest_scrubbed
        client.set_regtest(None)

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'self.tz'),
            '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"',
            1000,
            'bootstrap1',
        )

        client.transfer(0, 'bootstrap1', 'self', ['--burn-cap', '10'])
        bake(client)

        self_addr = client.get_contract_address('self')
        assert_storage_contains(client, 'self', f'"{self_addr}"')

    def test_contract_fails(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        client.set_regtest(None)

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'contract.tz'),
            'Unit',
            1000,
            'bootstrap1',
        )

        client.transfer(0, 'bootstrap1', 'self', ['--burn-cap', '10'])
        bake(client)
        addr = client.get_contract_address('contract')

        with assert_run_failure(r'script reached FAILWITH instruction'):
            client.transfer(
                0,
                'bootstrap1',
                'contract',
                ['-arg', f'"{addr}"', '--burn-cap', '10'],
            )

    def test_init_proxy(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'proxy.tz'),
            'Unit',
            1000,
            'bootstrap1',
        )

    def test_source(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_store = IDENTITIES['bootstrap4']['identity']
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'source.tz'),
            f'"{init_store}"',
            1000,
            'bootstrap1',
        )

        # direct transfer to the contract
        client.transfer(0, 'bootstrap2', 'source', ['--burn-cap', '10'])
        bake(client)

        source_addr = IDENTITIES['bootstrap2']['identity']
        assert_storage_contains(client, 'source', f'"{source_addr}"')

        # indirect transfer to the contract through proxy
        contract_addr = client.get_contract_address('source')
        client.transfer(
            0,
            'bootstrap2',
            'proxy',
            ['--burn-cap', '10', '--arg', f'"{contract_addr}"'],
        )
        bake(client)
        assert_storage_contains(client, 'source', f'"{source_addr}"')

    def test_sender(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        client.set_regtest(None)

        init_store = IDENTITIES['bootstrap4']['identity']
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'sender.tz'),
            f'"{init_store}"',
            1000,
            'bootstrap1',
        )

        # direct transfer to the contract
        client.transfer(0, 'bootstrap2', 'sender', ['--burn-cap', '10'])
        bake(client)

        sender_addr = IDENTITIES['bootstrap2']['identity']
        assert_storage_contains(client, 'sender', f'"{sender_addr}"')

        # indirect transfer to the contract through proxy
        contract_addr = client.get_contract_address('sender')
        proxy_addr = client.get_contract_address('proxy')
        client.transfer(
            0,
            'bootstrap2',
            'proxy',
            ['--burn-cap', '10', '--arg', f'"{contract_addr}"'],
        )
        bake(client)
        assert_storage_contains(client, 'sender', f'"{proxy_addr}"')

    def test_slice(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'slices.tz'),
            '"sppk7dBPqMPjDjXgKbb5f7V3PuKUrA4Zuwc3c3H7XqQerqPUWbK7Hna"',
            1000,
            'bootstrap1',
        )

    @pytest.mark.parametrize(
        'contract_arg',
        [
            line.rstrip('\n')
            for line in open(
                f'{paths.TEZOS_HOME}'
                + '/tests_python/tests_011/'
                + 'test_slice_fails_params.txt'
            )
        ],
    )
    def test_slice_fails(
        self, client_regtest_scrubbed: ClientRegression, contract_arg: str
    ):
        client = client_regtest_scrubbed

        with assert_run_failure(r'script reached FAILWITH instruction'):
            client.transfer(
                0,
                'bootstrap1',
                'slices',
                ['-arg', contract_arg, '--burn-cap', '10'],
            )
        # bake(client)

    @pytest.mark.parametrize(
        'contract_arg',
        [
            line.rstrip('\n')
            for line in open(
                f'{paths.TEZOS_HOME}'
                + '/tests_python/tests_011/'
                + 'test_slice_success_params.txt'
            )
        ],
    )
    def test_slice_success(
        self, client_regtest_scrubbed: ClientRegression, contract_arg: str
    ):
        client = client_regtest_scrubbed
        client.transfer(
            0,
            'bootstrap1',
            'slices',
            ['-arg', contract_arg, '--burn-cap', '10'],
        )
        bake(client)

    def test_split_string(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'split_string.tz'),
            '{}',
            1000,
            'bootstrap1',
        )

        client.transfer(
            0,
            'bootstrap1',
            'split_string',
            ['-arg', '"abc"', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(client, 'split_string', '{ "a" ; "b" ; "c" }')

        client.transfer(
            0,
            'bootstrap1',
            'split_string',
            ['-arg', '"def"', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(
            client, 'split_string', '{ "a" ; "b" ; "c" ; "d" ; "e" ; "f" }'
        )

    def test_split_bytes(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'split_bytes.tz'),
            '{}',
            1000,
            'bootstrap1',
        )

        client.transfer(
            0,
            'bootstrap1',
            'split_bytes',
            ['-arg', '0xaabbcc', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(client, 'split_bytes', '{ 0xaa ; 0xbb ; 0xcc }')

        client.transfer(
            0,
            'bootstrap1',
            'split_bytes',
            ['-arg', '0xddeeff', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(
            client, 'split_bytes', '{ 0xaa ; 0xbb ; 0xcc ; 0xdd ; 0xee ; 0xff }'
        )

    def test_set_delegate(self, client_regtest_scrubbed: ClientRegression):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'set_delegate.tz'),
            'Unit',
            1000,
            'bootstrap1',
        )
        bake(client)

        assert client.get_delegate('set_delegate').delegate is None

        addr = IDENTITIES['bootstrap5']['identity']
        client.transfer(
            0, 'bootstrap1', 'set_delegate', ['-arg', f'(Some "{addr}")']
        )
        bake(client)

        assert client.get_delegate('set_delegate').delegate == addr

        client.transfer(0, 'bootstrap1', 'set_delegate', ['-arg', 'None'])
        bake(client)

        assert client.get_delegate('set_delegate').delegate is None

    @pytest.mark.parametrize(
        'contract',
        [
            'compare_big_type.tz',
            'compare_big_type2.tz',
        ],
    )
    def test_trace_origination(self, client_regtest_scrubbed, contract):
        client = client_regtest_scrubbed
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, contract),
            'Unit',
            1000,
            'bootstrap1',
        )
        bake(client)


@pytest.mark.incremental
class TestTickets:
    """Tests for tickets."""

    def test_ticket_user_forge(self, client):
        bake(client)
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_store-2.tz'),
            'None',
            100,
            'bootstrap1',
            'storer',
        )

        # Create parameter by hand with a ticket type but no ticket in it
        client.transfer(
            100, 'bootstrap1', 'storer', ['-arg', 'None', '--burn-cap', '10']
        )

        with assert_run_failure(r'Unexpected forged value'):
            # Create parameter by hand with a ticket in it
            client.transfer(
                100,
                'bootstrap1',
                'storer',
                ['-arg', 'Some 1', '--burn-cap', '10'],
            )

        with assert_run_failure(r'Unexpected forged value'):
            # Create storage by hand with a ticket in it
            init_with_transfer(
                client,
                path.join(OPCODES_CONTRACT_PATH, 'ticket_bad.tz'),
                '1',
                100,
                'bootstrap1',
                'ticket_bad',
            )

    def test_ticket_user_big_forge(self, client):
        bake(client)
        contract_name = 'big_storer'
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_big_store.tz'),
            '{}',
            100,
            'bootstrap1',
            contract_name,
        )
        bake(client)
        client.transfer(
            100, 'bootstrap1', contract_name, ['-arg', '42', '--burn-cap', '10']
        )
        bake(client)
        storage = client.get_storage(contract_name)

        with assert_run_failure(r'Unexpected forged value'):
            # Create a storage with the ID of a big map that has tickets in it
            init_with_transfer(
                client,
                path.join(OPCODES_CONTRACT_PATH, 'ticket_big_store.tz'),
                storage,
                100,
                'bootstrap1',
                'thief',
            )

        with assert_run_failure(r'Unexpected forged value'):
            # Create a storage with the ID of a big map that has tickets in it
            init_with_transfer(
                client,
                path.join(OPCODES_CONTRACT_PATH, 'ticket_big_store.tz'),
                '(Pair ' + storage + ' {})',
                100,
                'bootstrap1',
                'thief',
            )

    def test_ticket_read(self, client):
        """Test TICKETS"""
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticketer.tz'),
            '42',
            100,
            'bootstrap1',
            'ticketer_read',
        )
        bake(client)
        ticketer_addr = client.get_contract_address('ticketer_read')
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_read.tz'),
            '"' + ticketer_addr + '"',
            100,
            'bootstrap1',
            'reader',
        )
        bake(client)
        reader_addr = client.get_contract_address('reader')
        client.transfer(
            100,
            'bootstrap1',
            'ticketer_read',
            ['-arg', '"' + reader_addr + '"', '--burn-cap', '10'],
        )
        bake(client)
        assert_storage_contains(client, "reader", '"' + ticketer_addr + '"')

    def test_bad_ticket(self, client):
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticketer.tz'),
            '42',
            100,
            'bootstrap1',
            'ticketer_bad',
        )
        bake(client)
        ticketer_addr = client.get_contract_address('ticketer_bad')
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_read.tz'),
            '"' + ticketer_addr + '"',
            100,
            'bootstrap1',
            'reader_bad',
        )
        bake(client)
        with assert_run_failure(r'Unexpected forged value'):
            client.transfer(
                100,
                'bootstrap1',
                'reader_bad',
                ['-arg', '1', '--burn-cap', '10'],
            )
            bake(client)

    def test_ticket_utxo(self, client):
        """Test UTXOs"""
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'utxor.tz'),
            '42',
            100,
            'bootstrap1',
        )
        bake(client)
        utxor_addr = client.get_contract_address('utxor')
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'utxo_read.tz'),
            '"' + utxor_addr + '"',
            100,
            'bootstrap1',
            "reader_a",
        )
        bake(client)
        reader_a_addr = client.get_contract_address('reader_a')
        utxor_addr = client.get_contract_address('utxor')
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'utxo_read.tz'),
            '"' + utxor_addr + '"',
            100,
            'bootstrap1',
            "reader_b",
        )
        bake(client)
        reader_b_addr = client.get_contract_address('reader_b')
        client.transfer(
            100,
            'bootstrap1',
            'utxor',
            [
                '-arg',
                '(Pair "' + reader_a_addr + '" "' + reader_b_addr + '")',
                '--burn-cap',
                '10',
            ],
        )
        bake(client)

    def test_ticket_split(self, client):
        def ticket(target_addr, param, utxo_amount):
            param = (
                '(Pair (Pair "'
                + target_addr
                + '" '
                + str(param)
                + ') '
                + str(utxo_amount)
                + ')'
            )
            client.transfer(
                100,
                'bootstrap1',
                'ticketer',
                ['-arg', param, '--burn-cap', '10'],
            )

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticketer-2.tz'),
            'Unit',
            100,
            'bootstrap1',
            'ticketer',
        )
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_split.tz'),
            'Unit',
            100,
            'bootstrap1',
            'splitter',
        )
        bake(client)
        splitter_addr = client.get_contract_address('splitter')
        ticket(splitter_addr, 42, 3)
        with assert_run_failure(r'script reached FAILWITH instruction'):
            # Wrong Split Amount
            ticket(splitter_addr, 42, 4)
        bake(client)

    def test_ticket_join(self, client):
        """Test JOIN"""

        def params(target_addr, utxo_amount, param):
            return (
                '(Pair (Pair "'
                + target_addr
                + '" '
                + str(param)
                + ') '
                + str(utxo_amount)
                + ')'
            )

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticketer-2.tz'),
            'Unit',
            100,
            'bootstrap1',
            'ticketer_a',
        )
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticketer-2.tz'),
            'Unit',
            100,
            'bootstrap1',
            'ticketer_b',
        )
        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'ticket_join.tz'),
            'None',
            100,
            'bootstrap1',
            'joiner',
        )
        bake(client)
        joiner_addr = client.get_contract_address('joiner')
        client.transfer(
            100,
            'bootstrap1',
            'ticketer_a',
            ['-arg', params(joiner_addr, 42, 1), '--burn-cap', '10'],
        )
        bake(client)
        client.transfer(
            100,
            'bootstrap1',
            'ticketer_a',
            ['-arg', params(joiner_addr, 144, 1), '--burn-cap', '10'],
        )
        bake(client)
        with assert_run_failure(r'script reached FAILWITH instruction'):
            # Different Ticketer
            client.transfer(
                100,
                'bootstrap1',
                'ticketer_b',
                ['-arg', params(joiner_addr, 23, 1), '--burn-cap', '10'],
            )
        with assert_run_failure(r'script reached FAILWITH instruction'):
            # Different Content
            client.transfer(
                100,
                'bootstrap1',
                'ticketer_a',
                ['-arg', params(joiner_addr, 21, 23), '--burn-cap', '10'],
            )

    def test_ticket_fungible_originations(self, client, session):
        """Test the origination of builder and wallet contracts for fungible
        tokens implemented using tickets."""

        builder_path = path.join(
            MINI_SCENARIOS_CONTRACT_PATH, 'ticket_builder_fungible.tz'
        )

        wallet_path = path.join(
            MINI_SCENARIOS_CONTRACT_PATH, 'ticket_wallet_fungible.tz'
        )

        manager_address = IDENTITIES['bootstrap1']['identity']

        builders = {}
        wallets = {}

        # Helper functions
        def originate_builder(name):
            """Create a fungible token contract managed by bootstrap1."""
            origination = client.originate(
                contract_name=f'builder_{name}',
                amount="0",
                sender='bootstrap1',
                contract=builder_path,
                args=['--init', f'"{manager_address}"', '--burn-cap', "10"],
            )
            builders[name] = origination.contract
            bake(client)

        def originate_wallet(name):
            """Create a fungible token wallet managed by bootstrap1."""
            origination = client.originate(
                contract_name=f'wallet_{name}',
                amount="0",
                sender='bootstrap1',
                contract=wallet_path,
                args=[
                    '--init',
                    f'Pair "{manager_address}" {{}}',
                    '--burn-cap',
                    "10",
                ],
            )
            wallets[name] = origination.contract
            bake(client)

        # Create 3 token contracts "A", "B", and "C".
        originate_builder("A")
        originate_builder("B")
        originate_builder("C")

        # Create 2 wallets "Alice" and "Bob".
        originate_wallet("Alice")
        originate_wallet("Bob")

        session['fungible_builders'] = builders
        session['fungible_wallets'] = wallets

    def test_ticket_fungible_transfers(self, client, session):
        """Test the life cycle of fungible tokens implemented using tickets."""

        manager_address = IDENTITIES['bootstrap1']['identity']

        builders = session['fungible_builders']
        wallets = session['fungible_wallets']

        def mint(builder, wallet, amount):
            """Mint fungible tokens."""
            wallet_address = wallets[wallet]
            parameter = f'(Pair "{wallet_address}%receive" {amount})'
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=f'builder_{builder}',
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'mint',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        def burn(builder, wallet, amount):
            """Burn fungible tokens."""
            builder_addr = builders[builder]
            parameter = f'Pair "{builder_addr}%burn" {amount} "{builder_addr}"'
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=f'wallet_{wallet}',
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'send',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        def transfer(builder, source_wallet, destination_wallet, amount):
            """Transfer fungible tokens."""
            builder_addr = builders[builder]
            dest_addr = wallets[destination_wallet]
            parameter = f'Pair "{dest_addr}%receive" {amount} "{builder_addr}"'
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=f'wallet_{source_wallet}',
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'send',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        # 100A --> Alice
        mint(builder="A", wallet="Alice", amount=100)
        # 100B --> Alice
        mint(builder="B", wallet="Alice", amount=100)

        # Fail: Alice --1C--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="C",
                source_wallet="Alice",
                destination_wallet="Bob",
                amount=1,
            )

        # Fail: Alice --0C--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="C",
                source_wallet="Alice",
                destination_wallet="Bob",
                amount=0,
            )

        # Fail: Alice --150A--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Alice",
                destination_wallet="Bob",
                amount=150,
            )

        # Fail: Bob --50A--> Alice
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Bob",
                destination_wallet="Alice",
                amount=50,
            )

        # Alice --50A--> Bob
        transfer(
            builder="A",
            source_wallet="Alice",
            destination_wallet="Bob",
            amount=50,
        )

        # Alice --50A--> Bob
        transfer(
            builder="A",
            source_wallet="Alice",
            destination_wallet="Bob",
            amount=50,
        )

        # Alice --0A--> Bob
        transfer(
            builder="A",
            source_wallet="Alice",
            destination_wallet="Bob",
            amount=0,
        )

        # Fail: Alice --1A--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Alice",
                destination_wallet="Bob",
                amount=1,
            )

        # Bob --100A--> Bob
        transfer(
            builder="A",
            source_wallet="Bob",
            destination_wallet="Bob",
            amount=100,
        )

        # Fail: Bob --150A--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Bob",
                destination_wallet="Bob",
                amount=150,
            )

        # Bob --100A-->
        burn(builder="A", wallet="Bob", amount=100)

        # Bob --0A-->
        burn(builder="A", wallet="Bob", amount=0)

        # Fail: Bob --1A-->
        with assert_run_failure(r'script reached FAILWITH instruction'):
            burn(builder="A", wallet="Bob", amount=1)

    def test_ticket_non_fungible_originations(self, client, session):
        """Test the origination of builder and wallet contracts for
        non-fungible tokens implemented using tickets."""

        builder_path = path.join(
            MINI_SCENARIOS_CONTRACT_PATH, 'ticket_builder_non_fungible.tz'
        )

        wallet_path = path.join(
            MINI_SCENARIOS_CONTRACT_PATH, 'ticket_wallet_non_fungible.tz'
        )

        manager_address = IDENTITIES['bootstrap1']['identity']

        builders = {}
        wallets = {}

        # Helper functions
        def originate_builder(name):
            """Create a non-fungible token contract managed by bootstrap1."""
            storage = f'(Pair "{manager_address}" 0)'
            origination = client.originate(
                contract_name=f'nft_builder_{name}',
                amount="0",
                sender='bootstrap1',
                contract=builder_path,
                args=['--init', storage, '--burn-cap', "10"],
            )
            builders[name] = origination.contract
            bake(client)

        def originate_wallet(name):
            """Create a non-fungible token wallet managed by bootstrap1."""
            origination = client.originate(
                contract_name=f'nft_wallet_{name}',
                amount="0",
                sender='bootstrap1',
                contract=wallet_path,
                args=[
                    '--init',
                    f'Pair "{manager_address}" {{}}',
                    '--burn-cap',
                    "10",
                ],
            )
            wallets[name] = origination.contract
            bake(client)

        # Create 3 token contracts "A", "B", and "C".
        originate_builder("A")
        originate_builder("B")
        originate_builder("C")

        # Create 2 wallets "Alice" and "Bob".
        originate_wallet("Alice")
        originate_wallet("Bob")

        session['non_fungible_builders'] = builders
        session['non_fungible_wallets'] = wallets

    def test_ticket_non_fungible_transfers(self, client, session):
        """Test the life cycle of non-fungible tokens implemented using
        tickets."""

        manager_address = IDENTITIES['bootstrap1']['identity']

        builders = session['non_fungible_builders']
        wallets = session['non_fungible_wallets']

        def mint(builder, wallet, token_id):
            """Mint a non-fungible token and assert that it has the expected
            id."""
            builder_alias = f'nft_builder_{builder}'
            expected_builder_storage = f'Pair "{manager_address}" {token_id}'
            actual_builder_storage = client.get_storage(builder_alias)
            assert expected_builder_storage == actual_builder_storage

            wallet_address = wallets[wallet]
            parameter = f'"{wallet_address}%receive"'
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=builder_alias,
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'mint_destination',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        def burn(builder, wallet, token_id):
            """Burn a non-fungible token."""
            builder_addr = builders[builder]
            parameter = (
                f'Pair "{builder_addr}%burn" "{builder_addr}" {token_id}'
            )
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=f'nft_wallet_{wallet}',
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'send',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        def transfer(builder, source_wallet, destination_wallet, token_id):
            """Transfer fungible tokens."""
            builder_addr = builders[builder]
            dest_addr = wallets[destination_wallet]
            parameter = (
                f'Pair "{dest_addr}%receive" "{builder_addr}" {token_id}'
            )
            client.transfer(
                amount=0,
                giver=manager_address,
                receiver=f'nft_wallet_{source_wallet}',
                args=[
                    '--burn-cap',
                    '2',
                    '--entrypoint',
                    'send',
                    '--arg',
                    parameter,
                ],
            )
            bake(client)

        # A0 --> Alice
        mint(builder="A", wallet="Alice", token_id=0)
        # A1 --> Alice
        mint(builder="A", wallet="Alice", token_id=1)
        # B0 --> Alice
        mint(builder="B", wallet="Alice", token_id=0)

        # Fail: Alice --C0--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="C",
                source_wallet="Alice",
                destination_wallet="Bob",
                token_id=0,
            )

        # Fail: Alice --A2--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Alice",
                destination_wallet="Bob",
                token_id=2,
            )

        # Fail: Bob --A0--> Alice
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Bob",
                destination_wallet="Alice",
                token_id=0,
            )

        # Fail: Bob --A1--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Bob",
                destination_wallet="Bob",
                token_id=1,
            )

        # Alice --A1--> Bob
        transfer(
            builder="A",
            source_wallet="Alice",
            destination_wallet="Bob",
            token_id=1,
        )

        # Alice --A0--> Bob
        transfer(
            builder="A",
            source_wallet="Alice",
            destination_wallet="Bob",
            token_id=0,
        )

        # Fail: Alice --A1--> Bob
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Alice",
                destination_wallet="Bob",
                token_id=1,
            )

        # Bob --A0--> Bob
        transfer(
            builder="A",
            source_wallet="Bob",
            destination_wallet="Bob",
            token_id=0,
        )

        # Bob --A0-->
        burn(builder="A", wallet="Bob", token_id=0)

        # Bob --A1-->
        burn(builder="A", wallet="Bob", token_id=1)

        # Fail: Bob --B0-->
        with assert_run_failure(r'script reached FAILWITH instruction'):
            burn(builder="B", wallet="Bob", token_id=0)

        # Alice --B0-->
        burn(builder="B", wallet="Alice", token_id=0)


ORIGINATE_BIG_MAP_FILE = path.join(
    OPCODES_CONTRACT_PATH, 'originate_big_map.tz'
)


@pytest.mark.incremental
@pytest.mark.contract
@pytest.mark.regression
class TestContractBigMapOrigination:
    def test_big_map_origination_literal(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        # originate a first version of the contract from a literal so
        # that a big_map with id 0 exists
        init_with_transfer(
            client,
            ORIGINATE_BIG_MAP_FILE,
            '{Elt 0 0}',
            1000,
            'bootstrap1',
            contract_name='originate_big_map_literal',
        )

    def test_big_map_origination_id(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        # originate again the same script from the big-map id 0
        with assert_run_failure(r'Unexpected forged value'):
            init_with_transfer(
                client,
                ORIGINATE_BIG_MAP_FILE,
                '0',
                1000,
                'bootstrap1',
                contract_name='originate_big_map_id',
            )

    def test_big_map_origination_diff(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed

        # originate again the same script from a big-diff
        with assert_run_failure(r'Unexpected forged value'):
            init_with_transfer(
                client,
                ORIGINATE_BIG_MAP_FILE,
                'Pair 0 {Elt 1 (Some 4)}',
                1000,
                'bootstrap1',
                contract_name='originate_big_map_diff',
            )

    def test_big_map_transfer_id(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        # call the first contract, passing an id as parameter
        with assert_run_failure(r'Unexpected forged value'):
            client.call(
                source='bootstrap1',
                destination='originate_big_map_literal',
                args=['--arg', '0'],
            )

    def test_big_map_transfer_diff(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        # call the first contract, passing a diff as parameter
        with assert_run_failure(r'Unexpected forged value'):
            client.call(
                source='bootstrap1',
                destination='originate_big_map_literal',
                args=['--arg', 'Pair 0 {Elt 1 (Some 4)}'],
            )


@pytest.mark.incremental
@pytest.mark.slow
@pytest.mark.contract
@pytest.mark.regression
class TestContractOnchainLevel:
    """Onchain tests for LEVEL."""

    # This test needs to be in a separate class to not depend on the number
    # of operations happening before

    def test_level(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed

        init_with_transfer(
            client,
            path.join(OPCODES_CONTRACT_PATH, 'level.tz'),
            '9999999',
            100,
            'bootstrap1',
        )
        bake(client)
        client.transfer(
            500, "bootstrap1", 'level', ['-arg', 'Unit', '--burn-cap', '10']
        )
        bake(client)
        level = client.get_level()
        slevel = str(level)
        assert_storage_contains(client, 'level', slevel)
        bake(client)
        bake(client)
        # checks the storage hasn't changed even though the current level has
        assert_storage_contains(client, 'level', slevel)
        # Run again to check the storage gets updated
        client.transfer(
            500, "bootstrap1", 'level', ['-arg', 'Unit', '--burn-cap', '10']
        )
        bake(client)
        assert_storage_contains(client, 'level', str(level + 3))
