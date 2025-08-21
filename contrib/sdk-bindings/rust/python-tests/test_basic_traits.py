# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    ContractTz1Hash,
    PublicKey,
    P256Signature,
)

def test_public_key_equality():
    # sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
    raw_pk1 = "sppk7cW8fKpjrM2713enEhvk6e5xhctLdhVCbE2YzMLCmNBHhphH4zE"
    # sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
    raw_pk2 = "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
    pk = PublicKey.from_b58check(raw_pk1)
    pk1 = PublicKey.from_b58check(raw_pk1)
    pk2 = PublicKey.from_b58check(raw_pk2)

    assert pk == pk1
    assert pk != pk2

def test_tz1_display():
    # sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
    raw_tz1 = "tz1XFq85mnnXhyhzpNEpxFvrkcuNtFBsSsVu"
    tz1 = ContractTz1Hash.from_b58check(raw_tz1)

    assert str(tz1) == raw_tz1

def test_p256_sig_debug():
    # sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
    # msg: "a"
    raw_sig = "p2sigqusPxJeh3NDzdCPdfk6UecDjL99MowfGGtV9i1PibkZ8XX6geDRsegSkKtqwuc93uuHS42vbpGF7wxbzExuVDZNXjp9Xh"
    sig = P256Signature.from_b58check(raw_sig)

    assert raw_sig in repr(sig)
