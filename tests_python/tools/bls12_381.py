"""Static classes for BLS-381 types, and utility functions for BLS12-381.
This file is intended to encompass and augment the functionality from py_ecc.
"""
from typing import Iterable, Tuple

from py_ecc import bls
from py_ecc import optimized_bls12_381 as bls12_381

from py_ecc.fields import (
    optimized_bls12_381_FQ as FQ,
    optimized_bls12_381_FQ2 as FQ2,
    optimized_bls12_381_FQ12 as FQ12,
)

G1Point = bls.typing.G1Uncompressed
G2Point = bls.typing.G2Uncompressed


class Fr:

    name = 'fr'

    modulus = 0x73EDA753299D7D483339D80809A1D80553BDA402FFFE5BFEFFFFFFFF00000001

    zero = 0

    one = 1

    @staticmethod
    def add(xxx, yyy):
        return (xxx + yyy) % Fr.modulus

    @staticmethod
    def mul(xxx, yyy):
        return (xxx * yyy) % Fr.modulus

    @staticmethod
    def neg(xxx):
        return (-xxx) % Fr.modulus

    @staticmethod
    def to_hex(point) -> str:
        return f'0x{point.to_bytes(32, byteorder="little").hex()}'

    @staticmethod
    def random(gen):
        return gen.randint(0, Fr.modulus)


class G1:

    name = 'g1'

    zero = bls12_381.Z1

    one = bls12_381.G1

    @staticmethod
    def add(xxx: G1Point, yyy: G1Point) -> G1Point:
        return bls12_381.add(xxx, yyy)

    @staticmethod
    def mul(xxx: G1Point, nnn: int) -> G1Point:
        return bls12_381.multiply(xxx, nnn)

    @staticmethod
    def neg(xxx: G1Point) -> G1Point:
        return bls12_381.neg(xxx)

    @staticmethod
    def equal(xxx: G1Point, yyy: G1Point) -> bool:
        return bls12_381.eq(xxx, yyy)

    @staticmethod
    def is_on_curve(xxx: G1Point) -> bool:
        return bls12_381.is_on_curve(xxx, bls12_381.b)

    @staticmethod
    def to_hex(g1_point: G1Point) -> str:
        if bls12_381.is_inf(g1_point):
            xxx, yyy = bls.constants.POW_2_382, 0
        else:
            xxx_pt, yyy_pt = bls12_381.normalize(g1_point)
            xxx, yyy = xxx_pt.n, yyy_pt.n
        xxx_bytes = xxx.to_bytes(48, byteorder='big')
        yyy_bytes = yyy.to_bytes(48, byteorder='big')
        return f'0x{(xxx_bytes + yyy_bytes).hex()}'

    @staticmethod
    def of_hex(hexstr: str) -> G1Point:
        xxx_bytes = bytes.fromhex(hexstr[2:98])
        yyy_bytes = bytes.fromhex(hexstr[98:])
        xxx = int.from_bytes(xxx_bytes, byteorder='big')
        yyy = int.from_bytes(yyy_bytes, byteorder='big')
        return (FQ(xxx), FQ(yyy), FQ(1))

    @staticmethod
    # Generate a random non-infinity point on G1
    def random(gen) -> G1Point:
        # Get a random point by multiplying a random Fr by the generator
        g1_point = G1.mul(G1.one, Fr.random(gen))
        assert G1.is_on_curve(g1_point)
        return g1_point


class G2:

    name = 'g2'

    zero = bls12_381.Z2

    one = bls12_381.G2

    @staticmethod
    def add(xxx: G2Point, yyy: G2Point) -> G2Point:
        return bls12_381.add(xxx, yyy)

    @staticmethod
    def mul(xxx: G2Point, nnn: int) -> G2Point:
        return bls12_381.multiply(xxx, nnn)

    @staticmethod
    def neg(xxx: G2Point) -> G2Point:
        return bls12_381.neg(xxx)

    @staticmethod
    def equal(xxx: G2Point, yyy: G2Point) -> bool:
        return bls12_381.eq(xxx, yyy)

    @staticmethod
    def is_on_curve(xxx: G2Point) -> bool:
        return bls12_381.is_on_curve(xxx, bls12_381.b2)

    @staticmethod
    def to_hex(g2_point: G2Point) -> str:
        if bls12_381.is_inf(g2_point):
            x_re, x_im, y_re, y_im = 0, bls.constants.POW_2_382, 0, 0
        else:
            xxx, yyy = bls12_381.normalize(g2_point)
            x_re, x_im = xxx.coeffs
            y_re, y_im = yyy.coeffs
        x_re_bytes = x_re.to_bytes(48, byteorder='big')
        x_im_bytes = x_im.to_bytes(48, byteorder='big')
        y_re_bytes = y_re.to_bytes(48, byteorder='big')
        y_im_bytes = y_im.to_bytes(48, byteorder='big')
        x_bytes = x_im_bytes + x_re_bytes
        y_bytes = y_im_bytes + y_re_bytes
        return f'0x{(x_bytes + y_bytes).hex()}'

    @staticmethod
    def of_hex(hexstr: str) -> G2Point:
        x_bytes = bytes.fromhex(hexstr[2:194])
        y_bytes = bytes.fromhex(hexstr[194:])
        x_im_bytes, x_re_bytes = x_bytes[0:48], x_bytes[48:]
        y_im_bytes, y_re_bytes = y_bytes[0:48], y_bytes[48:]
        x_im = int.from_bytes(x_im_bytes, byteorder='big')
        x_re = int.from_bytes(x_re_bytes, byteorder='big')
        y_im = int.from_bytes(y_im_bytes, byteorder='big')
        y_re = int.from_bytes(y_re_bytes, byteorder='big')
        return (FQ2([x_re, x_im]), FQ2([y_re, y_im]), FQ2([1, 0]))

    @staticmethod
    # Generate a random non-infinity point on G2
    def random(gen) -> G2Point:
        # Get a random point by multiplying a random Fr by the generator
        g2_point = G2.mul(G2.one, Fr.random(gen))
        assert G2.is_on_curve(g2_point)
        return g2_point


def pairing_check(points: Iterable[Tuple[G1Point, G2Point]]) -> bool:
    prod = FQ12.one()
    for g1_point, g2_point in points:
        prod = prod * bls12_381.pairing(g2_point, g1_point)
    return FQ12.one() == prod
