import pytest

from tools import utils
from tools.constants import IDENTITIES

DEX = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"
TOK = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN"
LQT = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
BOOTSTRAP1 = IDENTITIES['bootstrap1']['identity']
TOK_ADMIN = BOOTSTRAP1
BOOTSTRAP2 = IDENTITIES['bootstrap2']['identity']
BOOTSTRAP3 = IDENTITIES['bootstrap3']['identity']
FUTURE = "2050-01-01T00:00:00Z"


@pytest.mark.contract
@pytest.mark.regression
@pytest.mark.incremental
class SetupMintAndApprove:
    """Test calling entrypoints of liquidity baking contracts"""

    def test_setup(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        assert DEX == client.rpc(
            'get',
            "/chains/main/blocks/head/context/liquidity_baking/cpmm_address",
        )
        dex_storage = client.get_storage(DEX).split()
        assert dex_storage[4].strip('"') == TOK
        assert dex_storage[5].strip('"') == LQT

    # mint some test TOK (1 tzBTC?) for ourselves
    def test_call_mint_or_burn(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            TOK_ADMIN,
            TOK,
            [
                '--entrypoint',
                'mintOrBurn',
                '--arg',
                f'(Pair 100000000 "{BOOTSTRAP1}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # pre-approve big allowances in TOK for DEX
    def test_call_approve1(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap1',
            TOK,
            [
                '--entrypoint',
                'approve',
                '--arg',
                f'(Pair "{DEX}" 1000000000)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    def test_call_approve2(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap2',
            TOK,
            [
                '--entrypoint',
                'approve',
                '--arg',
                f'(Pair "{DEX}" 1000000000)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    def test_call_approve3(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap3',
            TOK,
            [
                '--entrypoint',
                'approve',
                '--arg',
                f'(Pair "{DEX}" 1000000000)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')


@pytest.mark.contract
@pytest.mark.regression
@pytest.mark.incremental
class TestAddApproveTransferRemove(SetupMintAndApprove):
    """Test add/approve/transfer/remove liquidity"""

    # first add liquidity on DEX so that we have some LQT
    def test_add_liquidity(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.transfer(
            9001,
            'bootstrap1',
            DEX,
            [
                '--entrypoint',
                'addLiquidity',
                '--arg',
                f'(Pair "{BOOTSTRAP1}" 0 1000000000 "{FUTURE}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # test LQT approval
    def test_approval(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap1',
            LQT,
            [
                '--entrypoint',
                'approve',
                '--arg',
                f'(Pair "{BOOTSTRAP2}" 1000)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # transfer LQT to bootstrap2 (using approval)
    def test_approved_transfer(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap2',
            LQT,
            [
                '--entrypoint',
                'transfer',
                '--arg',
                f'(Pair "{BOOTSTRAP1}" "{BOOTSTRAP2}" 1000)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # now remove LQT for bootstrap2
    def test_remove_liquidity(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap2',
            DEX,
            [
                '--entrypoint',
                'removeLiquidity',
                '--arg',
                f'(Pair "{BOOTSTRAP2}" 1000 0 0 "{FUTURE}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # check DEX storage
    def test_dex_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(DEX)

    # check LQT storage
    def test_lqt_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(LQT)

    # check TOK storage
    def test_tok_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(TOK)


@pytest.mark.contract
@pytest.mark.regression
@pytest.mark.incremental
class TestTrades(SetupMintAndApprove):
    """Test trades"""

    # add liquidity
    def test_add_liquidity(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.transfer(
            9001,
            'bootstrap1',
            DEX,
            [
                '--entrypoint',
                'addLiquidity',
                '--arg',
                f'(Pair "{BOOTSTRAP1}" 0 1000000000 "{FUTURE}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # bootstrap2 buys some TOK
    def test_buy_tok(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.transfer(
            9001,
            'bootstrap2',
            DEX,
            [
                '--entrypoint',
                'xtzToToken',
                '--arg',
                f'(Pair "{BOOTSTRAP2}" 0 "{FUTURE}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # bootstrap2 transfers TOK to bootstrap3
    def test_transfer(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap2',
            TOK,
            [
                '--entrypoint',
                'transfer',
                '--arg',
                f'(Pair "{BOOTSTRAP2}" "{BOOTSTRAP3}" 100)',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # bootstrap3 sells TOK
    def test_sell_tok(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.call(
            'bootstrap3',
            DEX,
            [
                '--entrypoint',
                'tokenToXtz',
                '--arg',
                f'(Pair "{BOOTSTRAP3}" 100 0 "{FUTURE}")',
                '--burn-cap',
                '10',
            ],
        )
        utils.bake(client, 'bootstrap5')

    # check DEX storage
    def test_dex_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(DEX)

    # check LQT storage
    def test_lqt_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(LQT)

    # check TOK storage
    def test_tok_storage(self, client_regtest_scrubbed):
        client = client_regtest_scrubbed
        client.get_storage(TOK)
