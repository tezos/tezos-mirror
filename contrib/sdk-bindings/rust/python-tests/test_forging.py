# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    forge_message,
    forge_operation,
    Operation,
    PublicKey,
    PublicKeyHash,
    BlsSignature,
    Contract,
    BlockHash,
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


def test_reveal_forging():
    """
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz4HWRiWnefpPHKUmjTfJhHG74q7fEmS5euR",
      "fee": "3",
      "counter": "718",
      "gas_limit": "3679",
      "storage_limit": "25",
      "public_key": "BLpk1nco14hJwGiTzrRp2FBCxZz6ZJ19QRAoh8hyaNMeVARFyuvTGZQgmrPTT5ZkXfS5yKuEupPN",
      "proof": "BLsigAy1Lc4y9kagjUjPFRb5avizhuXSYQyeSsGAsWbiLBwAEHZvQMxrNrH11Bm9SfHpBbCb9RjnucX2FEZbTZ6it7Z7UGFf3mo7uUdi8V2DX5SmaqqwyzyD12E3rUKxgwHkaSyEiMKcTB"
    }'
    """
    public_key = PublicKey.from_b58check("BLpk1nco14hJwGiTzrRp2FBCxZz6ZJ19QRAoh8hyaNMeVARFyuvTGZQgmrPTT5ZkXfS5yKuEupPN")
    source = PublicKeyHash.from_b58check("tz4HWRiWnefpPHKUmjTfJhHG74q7fEmS5euR")
    proof = BlsSignature.from_b58check("BLsigAy1Lc4y9kagjUjPFRb5avizhuXSYQyeSsGAsWbiLBwAEHZvQMxrNrH11Bm9SfHpBbCb9RjnucX2FEZbTZ6it7Z7UGFf3mo7uUdi8V2DX5SmaqqwyzyD12E3rUKxgwHkaSyEiMKcTB")
    reveal = Operation.reveal(
        source=source,
        fee=3,
        counter=718,
        gas_limit=3679,
        storage_limit=25,
        public_key=public_key,
        proof=proof,
    )
    raw_reveal = reveal.forge()
    expected_bytes = bytes.fromhex('6b035d4586d123b894f9d7cd1b28fff62e6b4bb8dd1103ce05df1c1903883927221f890dd4873e2296498a888d79b8020fe15b47c52547da05e73197fa21a44d87ef2ac7e69e91238031d9e531ff00000060a592dd097f55fa1df1598f577e8005413e822489bba23114cd9ed8bd2dd66891ecd52ec0ca3ff250badb9b3261f36ee9127847540ec6600e38013757c07ef249f6cf6270519a101bf143e232a1e9390952e8699a16999ef21efa267aac2e54cc')
    assert raw_reveal == expected_bytes


def test_transaction_forging():
    """
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8",
      "fee": "1000000000",
      "counter": "123456",
      "gas_limit": "0",
      "storage_limit": "0",
      "amount": "1",
      "destination": "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn"
    }'
    """
    source = PublicKeyHash.from_b58check("tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8")
    destination = Contract.from_b58check("tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn")
    transaction = Operation.transaction(
        source=source,
        fee=1000000000,
        counter=123456,
        gas_limit=0,
        storage_limit=0,
        amount=1,
        destination=destination,
    )
    raw_transaction = transaction.forge()
    expected_bytes = bytes.fromhex('6c023f3b21e54e98db3845a2a5033d96877f7f9cea878094ebdc03c0c4070000010002ebfd1371b542831b4be730161d08885c5312e44200')
    assert raw_transaction == expected_bytes


def test_origination_forging():
    """
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8",
      "fee": "2",
      "counter": "603",
      "gas_limit": "59",
      "storage_limit": "109",
      "balance": "90",
      "script": {
        "code": [],
        "storage": {
          "prim": "unit"
        }
      }
    }'
    """
    source = PublicKeyHash.from_b58check("tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8")
    origination = Operation.origination(
        source=source,
        fee=2,
        counter=603,
        gas_limit=59,
        storage_limit=109,
        balance=90,
        delegate=None
    )
    raw_origination = origination.forge()
    expected_bytes = bytes.fromhex('6d023f3b21e54e98db3845a2a5033d96877f7f9cea8702db043b6d5a0000000005020000000000000002036c')
    assert raw_origination == expected_bytes


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
    delegation = Operation.delegation(
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


def test_operation_forging():
    """
    jq -n '{
      "branch": "BKuGrbLSk7bBwh3x9PAXtao3bLJBtSFN5syraP4mz414BAeSxaT",
      "contents": [range(1024) | {
        "kind": "transaction",
        "source": "tz3bCBLYQUspvznkeq8cZrM1dnd1mpNqNyTF",
        "fee": "806",
        "counter": "160",
        "gas_limit": "614",
        "storage_limit": "407",
        "amount": "42",
        "destination": "tz4MFCwVMrN89ymtDsPMAXpdgug5SmGcLLJt"
      }]
    }' > /tmp/tezos-sdk-bindings-tests-many-operations.json
    octez-codec encode "023-PtSeouLo.operation.unsigned" from /tmp/tezos-sdk-bindings-tests-many-operations.json
    """
    NB_TX = 1024
    transaction = Operation.transaction(
        source=PublicKeyHash.from_b58check("tz3bCBLYQUspvznkeq8cZrM1dnd1mpNqNyTF"),
        fee=806,
        counter=160,
        gas_limit=614,
        storage_limit=407,
        amount=42,
        destination=Contract.from_b58check("tz4MFCwVMrN89ymtDsPMAXpdgug5SmGcLLJt"),
    )

    branch = BlockHash.from_b58check("BKuGrbLSk7bBwh3x9PAXtao3bLJBtSFN5syraP4mz414BAeSxaT")
    operations = [transaction] * NB_TX

    forged_operation = forge_operation(branch, operations)

    expected_bytes = bytes.fromhex(
        # octez-codec encode "operation.shell_header" from '{ "branch": "BKuGrbLSk7bBwh3x9PAXtao3bLJBtSFN5syraP4mz414BAeSxaT" }'
        "18f543ab7d89ffceddb0704543a1909fe5d29ab091379d34a04252867e40e84c" +
        # octez-codec encode "023-PtSeouLo.operation.contents" from '{
        #   "kind": "transaction",
        #   "source": "tz3bCBLYQUspvznkeq8cZrM1dnd1mpNqNyTF",
        #   "fee": "806",
        #   "counter": "160",
        #   "gas_limit": "614",
        #   "storage_limit": "407",
        #   "amount": "42",
        #   "destination": "tz4MFCwVMrN89ymtDsPMAXpdgug5SmGcLLJt"
        # }'
        "6c02a30d014fbe864d819e0efe698f1ebeb762a8dbc2a606a001e60497032a00038645200f1586a6c8d291fed4e57aa07da82ec59400" * NB_TX
    )
    assert forged_operation == expected_bytes
