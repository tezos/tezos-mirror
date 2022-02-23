from os import path
import random
from hashlib import blake2b
import pytest

from tools.bls12_381 import G1, G2, Fr, pairing_check
from tools.utils import assert_run_failure
from .contract_paths import MINI_SCENARIOS_CONTRACT_PATH, OPCODES_CONTRACT_PATH


def check_contract(client, contract_name, arg, expected_storage):
    contract_path = path.join(OPCODES_CONTRACT_PATH, f'{contract_name}.tz')
    result = client.run_script(contract_path, 'None', arg)
    assert result.storage == f'(Some {expected_storage})'


def check_contract_binop(client, contract_name, arg0, arg1, expected_storage):
    check_contract(
        client, contract_name, f'Pair {arg0} {arg1}', expected_storage
    )


# prefix a type name with 'bls12_381_'
def bls(tname):
    return f'bls12_381_{tname}'


# Store
def check_store(client, cls, arg):
    arg = cls.to_hex(arg)
    check_contract(client, f'store_{bls(cls.name)}', arg, arg)


# Add
def check_add(client, cls, xxx, yyy):
    check_contract_binop(
        client,
        f'add_{bls(cls.name)}',
        cls.to_hex(xxx),
        cls.to_hex(yyy),
        cls.to_hex(cls.add(xxx, yyy)),
    )


# Mul
def check_mul(client, cls, xxx, yyy):
    check_contract_binop(
        client,
        f'mul_{bls(cls.name)}',
        cls.to_hex(xxx),
        Fr.to_hex(yyy),
        cls.to_hex(cls.mul(xxx, yyy)),
    )


# Neg
def check_neg(client, cls, arg):
    res = cls.to_hex(cls.neg(arg))
    arg = cls.to_hex(arg)
    check_contract(client, f'neg_{bls(cls.name)}', arg, res)


# Pairing Check
def check_pairing_check(client, args):
    res = pairing_check(args)
    args = [(G1.to_hex(g1), G2.to_hex(g2)) for g1, g2 in args]
    args = [f'Pair {g1} {g2}' for g1, g2 in args]
    args = f'{{ {"; ".join(args)} }}'
    check_contract(client, 'pairing_check', args, res)


# Setting this higher makes things rather slow
RANDOM_ITERATIONS = range(10)


STORE_CLASSES = [G1, G2, Fr]
CURVES = [G1, G2]
ADD_CLASSES = [G1, G2, Fr]
MUL_CLASSES = [G1, G2, Fr]
NEG_CLASSES = [G1, G2, Fr]


@pytest.mark.incremental
@pytest.mark.contract
@pytest.mark.regression
class TestBls12_381:

    # Fix the random seed to ensure reproducibility
    h = blake2b()
    h.update(b'seed')
    gen = random.Random()
    gen.seed(bytes.fromhex(h.hexdigest()))

    # Store

    @pytest.mark.parametrize("cls", STORE_CLASSES)
    def test_store_zero(self, client_regtest, cls):
        check_store(client_regtest, cls, cls.zero)

    @pytest.mark.parametrize("cls", STORE_CLASSES)
    def test_store_one(self, client_regtest, cls):
        check_store(client_regtest, cls, cls.one)

    @pytest.mark.parametrize("cls", STORE_CLASSES)
    def test_store_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_store(client_regtest, cls, cls.random(self.gen))

    # Add
    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_zero_zero(self, client_regtest, cls):
        check_add(client_regtest, cls, cls.zero, cls.zero)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_zero_one(self, client_regtest, cls):
        check_add(client_regtest, cls, cls.zero, cls.one)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_zero_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_add(client_regtest, cls, cls.zero, cls.random(self.gen))

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_one_zero(self, client_regtest, cls):
        check_add(client_regtest, cls, cls.one, cls.zero)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_one_one(self, client_regtest, cls):
        check_add(client_regtest, cls, cls.one, cls.one)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_one_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_add(client_regtest, cls, cls.one, cls.random(self.gen))

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_random_zero(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_add(client_regtest, cls, cls.random(self.gen), cls.zero)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_random_one(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_add(client_regtest, cls, cls.random(self.gen), cls.one)

    @pytest.mark.parametrize("cls", ADD_CLASSES)
    def test_add_random_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_add(
                client_regtest, cls, cls.random(self.gen), cls.random(self.gen)
            )

    # Mul
    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_zero_zero(self, client_regtest, cls):
        check_mul(client_regtest, cls, cls.zero, Fr.zero)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_zero_one(self, client_regtest, cls):
        check_mul(client_regtest, cls, cls.zero, Fr.one)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_zero_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_mul(client_regtest, cls, cls.zero, Fr.random(self.gen))

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_one_zero(self, client_regtest, cls):
        check_mul(client_regtest, cls, cls.one, Fr.zero)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_one_one(self, client_regtest, cls):
        check_mul(client_regtest, cls, cls.one, Fr.one)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_one_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_mul(client_regtest, cls, cls.one, Fr.random(self.gen))

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_random_zero(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_mul(client_regtest, cls, cls.random(self.gen), Fr.zero)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_random_one(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_mul(client_regtest, cls, cls.random(self.gen), Fr.one)

    @pytest.mark.parametrize("cls", MUL_CLASSES)
    def test_mul_random_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_mul(
                client_regtest, cls, cls.random(self.gen), Fr.random(self.gen)
            )

    # Neg
    @pytest.mark.parametrize("cls", NEG_CLASSES)
    def test_neg_zero(self, client_regtest, cls):
        check_neg(client_regtest, cls, cls.zero)

    @pytest.mark.parametrize("cls", NEG_CLASSES)
    def test_neg_one(self, client_regtest, cls):
        check_neg(client_regtest, cls, cls.one)

    @pytest.mark.parametrize("cls", NEG_CLASSES)
    def test_neg_random(self, client_regtest, cls):
        for _ in RANDOM_ITERATIONS:
            check_neg(client_regtest, cls, cls.random(self.gen))

    # Pairing checks
    def test_pairing_nil(self, client_regtest):
        check_pairing_check(client_regtest, [])

    def test_pairing_zero_zero(self, client_regtest):
        args = [(G1.zero, G2.zero)]
        check_pairing_check(client_regtest, args)

    def test_pairing_zero_one(self, client_regtest):
        args = [(G1.zero, G2.one)]
        check_pairing_check(client_regtest, args)

    def test_pairing_zero_random(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            args = [(G1.zero, G2.random(self.gen))]
            check_pairing_check(client_regtest, args)

    def test_pairing_one_zero(self, client_regtest):
        args = [(G1.one, G2.zero)]
        check_pairing_check(client_regtest, args)

    def test_pairing_one_one(self, client_regtest):
        args = [(G1.one, G2.one)]
        check_pairing_check(client_regtest, args)

    def test_pairing_one_random(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            args = [(G1.one, G2.random(self.gen))]
            check_pairing_check(client_regtest, args)

    def test_pairing_random_zero(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            args = [(G1.random(self.gen), G2.zero)]
            check_pairing_check(client_regtest, args)

    def test_pairing_random_one(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            args = [(G1.random(self.gen), G2.one)]
            check_pairing_check(client_regtest, args)

    def test_pairing_random_random(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            args = [(G1.random(self.gen), G2.random(self.gen))]
            check_pairing_check(client_regtest, args)

    def test_pairing_neg_g1(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            g1_point = G1.random(self.gen)
            g2_point = G2.random(self.gen)
            args = [(g1_point, g2_point), (G1.neg(g1_point), g2_point)]
            check_pairing_check(client_regtest, args)

    def test_pairing_neg_g2(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            g1_point = G1.random(self.gen)
            g2_point = G2.random(self.gen)
            args = [(g1_point, g2_point), (g1_point, G2.neg(g2_point))]
            check_pairing_check(client_regtest, args)

    # Pairing Check test based on signature aggregation
    def test_signature_aggregation(self, client_regtest):
        for _ in RANDOM_ITERATIONS:
            sk0 = Fr.random(self.gen)  # secret key
            pk0 = G2.mul(G2.one, sk0)  # public key
            # we don't have hash-to-curve on g1, so compute a random point
            msg_hash = G1.random(self.gen)  # message hash
            sig0 = G1.mul(msg_hash, sk0)  # signature
            args0 = [(msg_hash, pk0), (G1.neg(sig0), G2.one)]
            check_pairing_check(client_regtest, args0)

            sk1 = Fr.random(self.gen)  # secret key
            pk1 = G2.mul(G2.one, sk1)  # public key
            # we don't have hash-to-curve on g1, so compute a random point
            sig1 = G1.mul(msg_hash, sk1)  # signature
            args1 = [
                (G1.add(msg_hash, msg_hash), G2.add(pk0, pk1)),
                (G1.neg(G1.add(sig0, sig1)), G2.add(G2.one, G2.one)),
            ]
            check_pairing_check(client_regtest, args1)

    def test_groth16(self, client_regtest):
        # pylint: disable=line-too-long
        # The verifying key, proof, and inputs are generated from
        # ZoKrates, modified to use BLS12-381.
        # The circuit proves knowledge of a square root of 113569.

        input_x = "0xa1bb010000000000000000000000000000000000000000000000000000000000"  # noqa
        input_y = "0x0100000000000000000000000000000000000000000000000000000000000000"  # noqa
        proof_a = "0x0a2841423326ab08f5f406409775e43fa0f9a0b97631fa85d2dd9242507d25059e9cf48b8b98f99a0008671423a148ec106d70637056972ef49fb6f62de2e89ba3682b9972292b6bb4e6f53799a75d2f8001ccfde280d8ac05fc209352236cbd"  # noqa
        proof_b = "0x0fced939fb1ad733f99669f50a383ef632f6d41dfbde434a6715afd5c7dfbb7bc5835e058ad8b590c7b38dd137d0bd0f0e1540f1b45d8aa626c360e2ea484a116243f7c802034de915db6b18d5303946f676e423cbd6046d37a82208d500625a11c7250ccb953a7ee49d704ad14de4b727733cff7cf06875d8b6444f3c0a8cbf0bd980e539c74bd5b37bb15fe816f23407d269193105fda71adf35fae9309d9d46729fcd4685699097a86f0460a2bc8b16293940cabfdcfe0f27e4107e74e90c"  # noqa
        proof_c = "0x0a1fb5a144ca3bdfe4ad0f183cf71dd7fdd28cbef4fcd47b5b419f65186703f62ecaaa1255fa21a6ebdd917ab1f9bd9707de7066865e2ff3875e22088619125a0d4088a622ab42224425ef89a5a149ce2db9c8292b62c7e7aaa7e87f3535304b"  # noqa

        inputs = f"Pair {input_x} {input_y}"
        proof = f"Pair (Pair {proof_a} {proof_b}) {proof_c}"
        arg = f"Pair ({inputs}) ({proof})"

        contract = path.join(MINI_SCENARIOS_CONTRACT_PATH, 'groth16.tz')
        client_regtest.run_script(contract, 'Unit', arg)

    def test_fr_bytes_parameters_more_than_32_bytes(self, client_regtest):
        random_bytes = (
            "0xf7ef66f95c90b2f953eb0555af65f22095d4f54b40ea8c6d"
            + "cc2014740e8662c16bb8786723"
        )
        contract = path.join(OPCODES_CONTRACT_PATH, 'bls12_381_fr_to_int.tz')
        with assert_run_failure(r'error running script'):
            client_regtest.run_script(contract, storage='0', inp=random_bytes)
