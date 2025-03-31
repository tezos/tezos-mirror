# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    PublicKeySecp256k1,
    PublicKeyP256,
    PublicKeyBls,
    PublicKey,
    Signature,
    Ed25519Signature,
    Secp256k1Signature,
    P256Signature,
    forge_message
)


def test_blpk_to_tz4():
    # sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
    b58_pk = "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp"
    expected_b58_pkh = "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
    pk = PublicKeyBls.from_b58check(b58_pk)
    pkh = pk.pk_hash()
    b58_pkh = pkh.to_b58check()
    assert b58_pkh == expected_b58_pkh


def test_pk_to_pkh():
    # sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
    b58_pk = "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ"
    expected_b58_pkh = "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
    pk = PublicKey.from_b58check(b58_pk)
    pkh = pk.pk_hash()
    b58_pkh = pkh.to_b58check()
    assert b58_pkh == expected_b58_pkh


def test_edsig_into_generic_sig():
    # sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
    # msg: "a"
    b58_sig = "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1"
    sig = Ed25519Signature.from_b58check(b58_sig)
    generic_sig = sig.into_generic()
    assert generic_sig.to_b58check() == b58_sig
    generic_sig = Signature.from_ed25519_signature(sig)
    assert generic_sig.to_b58check() == b58_sig


def test_spsig_from_generic_sig():
    # sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
    # msg: "a"
    b58_sig = "sigmstyCA7dwXvY7UXo8mkUG47WPCybhngBYvHokpnReuZdKZwPoXK8aKp4sEZgAhxeJsnNyJ68Q4zdJyJuSw25JTjxgtVpA"
    expected_b58_sig = "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E"
    generic_sig = Signature.from_b58check(b58_sig)
    sig = Secp256k1Signature.try_from_generic(generic_sig)
    assert sig.to_b58check() == expected_b58_sig
    sig = generic_sig.try_into_secp256k1_signature()
    assert sig.to_b58check() == expected_b58_sig


def test_verify_p2sig():
    # sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
    # msg: "a"
    b58_pk = "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G"
    b58_sig = "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
    msg = "a"
    pk = PublicKeyP256.from_b58check(b58_pk)
    sig = P256Signature.from_b58check(b58_sig)
    raw_msg = forge_message(msg)
    assert pk.verify_signature(sig, raw_msg)


def test_verify_sig():
    # sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
    # msg: "a"
    b58_pk = "edpktvpansLmKrvHCS1aWkFHS6gJdSd5haH1Z74MJFAAeNDSuSgHBH"
    b58_sig = "sigWrzQCbre6B7VLP4kGntoQGrEBLLvc8cFPySNiDj5m2cTd4DfJG2feBLhgyjtHcTiLenwrActDgp9y6pxp3MS5m5sqVCY2"
    msg = "a"
    pk = PublicKey.from_b58check(b58_pk)
    sig = Signature.from_b58check(b58_sig)
    raw_msg = forge_message(msg)
    assert pk.verify_signature(sig, raw_msg)


def test_verify_wrong_sig():
    # sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
    b58_pk = "sppk7aBZCsBJTTDTV1Lwo4eZBnqmSTSnChYF1GxHUsgeWumUCHcZyxv"
    # sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
    # msg: "a"
    b58_sig = "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF"
    msg = "a"
    pk = PublicKeySecp256k1.from_b58check(b58_pk)
    sig = Secp256k1Signature.from_b58check(b58_sig)
    raw_msg = forge_message(msg)
    assert not pk.verify_signature(sig, raw_msg)
