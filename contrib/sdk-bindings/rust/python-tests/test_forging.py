# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    forge_message,
    Delegation,
    PublicKeyHash,
)

def test_message_forging():
    msg = "message"
    raw_msg = forge_message(msg)
    expected_bytes = bytes([
        0x05, 0x01, 0x00, 0x00, 0x00, 0x07,
        ord('m'), ord('e'), ord('s'), ord('s'),
        ord('a'), ord('g'), ord('e')
    ])
    assert raw_msg == expected_bytes

def test_delegation_forging():
    """
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn",
      "fee": "3141",
      "counter": "24006",
      "gas_limit": "90",
      "storage_limit": "1",
      "delegate": "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
    }'
    """
    delegate = PublicKeyHash.from_b58check("tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu")
    source = PublicKeyHash.from_b58check("tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn")
    delegation = Delegation(
        source=source,
        fee=3141,
        counter=24006,
        gas_limit=90,
        storage_limit=1,
        delegate=delegate,
    )
    raw_delegation = delegation.forge()
    expected_bytes = bytes.fromhex('6e02ebfd1371b542831b4be730161d08885c5312e442c518c6bb015a01ff01a7b4ff0f28869d9f9c27d653c73aee41bd7fc777')
    assert raw_delegation == expected_bytes
