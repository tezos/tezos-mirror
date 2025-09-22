# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    BigInt,
)

def test_big_int():
    big_int_from_int = BigInt.from_int(2**63 - 1)
    big_int_from_string = BigInt.from_string("9223372036854775807")

    assert big_int_from_int == big_int_from_string

    other_big_int_from_string = BigInt.from_string("9223372036854775808")

    assert big_int_from_int != other_big_int_from_string
