# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    Error,
    ContractTz1Hash,
    PublicKeyBls,
    PublicKey,
)


def test_tz1_decode_encode():
    # sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
    base_b58_pkh = "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2VJ"
    pkh = ContractTz1Hash.from_b58check(base_b58_pkh)
    b58_pkh = pkh.to_b58check()
    assert b58_pkh == base_b58_pkh


def test_blpk_decode_encode():
    # sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    base_b58_pk = "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
    pk = PublicKeyBls.from_b58check(base_b58_pk)
    b58_pk = pk.to_b58check()
    assert b58_pk == base_b58_pk


def test_pk_decode_encode():
    # sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
    base_b58_pk = "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B"
    pk = PublicKey.from_b58check(base_b58_pk)
    b58_pk = pk.to_b58check()
    assert b58_pk == base_b58_pk


def test_decode_invalid_checksum():
    try:
        ContractTz1Hash.from_b58check("tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2Vj")
        raise AssertionError("Converting hash with invalid checksum must fail")
    except Error.Base58 as e:
        expected_error = Error.Base58("Base58check conversion failure: InvalidChecksum")
        assert str(e) == str(expected_error)
