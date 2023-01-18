import pytest
from tools.utils import (
    assert_run_failure,
    assert_storage_contains,
    bake,
)
from tools.constants import IDENTITIES
from .contract_paths import find_script, init_with_transfer


@pytest.mark.incremental
class TestTickets:
    """Tests for tickets."""

    def test_ticket_user_forge(self, client):
        bake(client)
        init_with_transfer(
            client,
            ['opcodes', 'ticket_store-2'],
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
                ['opcodes', 'ticket_bad'],
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
            ['opcodes', 'ticket_big_store'],
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
                ['opcodes', 'ticket_big_store'],
                storage,
                100,
                'bootstrap1',
                'thief',
            )

        with assert_run_failure(r'Unexpected forged value'):
            # Create a storage with the ID of a big map that has tickets in it
            init_with_transfer(
                client,
                ['opcodes', 'ticket_big_store'],
                '(Pair ' + storage + ' {})',
                100,
                'bootstrap1',
                'thief',
            )

    def test_ticket_read(self, client):
        """Test TICKETS"""
        init_with_transfer(
            client,
            ['opcodes', 'ticketer'],
            '42',
            100,
            'bootstrap1',
            'ticketer_read',
        )
        bake(client)
        ticketer_addr = client.get_contract_address('ticketer_read')
        init_with_transfer(
            client,
            ['opcodes', 'ticket_read'],
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
            ['opcodes', 'ticketer'],
            '42',
            100,
            'bootstrap1',
            'ticketer_bad',
        )
        bake(client)
        ticketer_addr = client.get_contract_address('ticketer_bad')
        init_with_transfer(
            client,
            ['opcodes', 'ticket_read'],
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
            ['opcodes', 'utxor'],
            '42',
            100,
            'bootstrap1',
        )
        bake(client)
        utxor_addr = client.get_contract_address('utxor')
        init_with_transfer(
            client,
            ['opcodes', 'utxo_read'],
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
            ['opcodes', 'utxo_read'],
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
            ['opcodes', 'ticketer-2'],
            'Unit',
            100,
            'bootstrap1',
            'ticketer',
        )
        init_with_transfer(
            client,
            ['opcodes', 'ticket_split'],
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
            ['opcodes', 'ticketer-2'],
            'Unit',
            100,
            'bootstrap1',
            'ticketer_a',
        )
        init_with_transfer(
            client,
            ['opcodes', 'ticketer-2'],
            'Unit',
            100,
            'bootstrap1',
            'ticketer_b',
        )
        init_with_transfer(
            client,
            ['opcodes', 'ticket_join'],
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

        builder_path = find_script(
            ['mini_scenarios', 'ticket_builder_fungible']
        )

        wallet_path = find_script(['mini_scenarios', 'ticket_wallet_fungible'])

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
        # This fails because we are not allowed to keep a ticket
        # with zero amount in big maps.
        # In the last transfer, Alice's wallet contract has depleted all
        # A-tokens.
        # Therefore, this contract call fails on A-token look-up.
        with assert_run_failure(r'script reached FAILWITH instruction'):
            transfer(
                builder="A",
                source_wallet="Alice",
                destination_wallet="Bob",
                amount=0,
            )

        # Fail: Alice --1A--> Bob
        # Similarly, this contract call fails because there is no
        # big map entry for A-tokens in Alice's wallet contract.
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
        # The last `burn` call depletes all A-tokens in Bob's wallet.
        # Since no ticket of amount zero is allowed in big map,
        # this call fails because there is no A-token entry in Bob's
        # wallet contract.
        with assert_run_failure(r'script reached FAILWITH instruction'):
            burn(builder="A", wallet="Bob", amount=0)

        # Fail: Bob --1A-->
        with assert_run_failure(r'script reached FAILWITH instruction'):
            burn(builder="A", wallet="Bob", amount=1)

    def test_ticket_non_fungible_originations(self, client, session):
        """Test the origination of builder and wallet contracts for
        non-fungible tokens implemented using tickets."""

        builder_path = find_script(
            ['mini_scenarios', 'ticket_builder_non_fungible']
        )

        wallet_path = find_script(
            ['mini_scenarios', 'ticket_wallet_non_fungible']
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
