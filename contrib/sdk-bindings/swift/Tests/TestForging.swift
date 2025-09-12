// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestForging: XCTestCase {

    public static var allTests = [
      ("messageForging", messageForging),
      ("revealForging", revealForging),
      ("transactionForging", transactionForging),
      ("originationForging", originationForging),
      ("delegationForging", delegationForging),
      ("operationForging", operationForging),
    ]

    func messageForging() {
        let msg = "message"
        let rawMsg = forgeMessage(msg: msg)
        let expectedBytes: [UInt8] = [
          0x05, 0x01, 0x00, 0x00, 0x00, 0x07,
          UInt8(ascii: "m"), UInt8(ascii: "e"), UInt8(ascii: "s"),
          UInt8(ascii: "s"), UInt8(ascii: "a"), UInt8(ascii: "g"),
          UInt8(ascii: "e")
        ]
        XCTAssertEqual(Array(rawMsg), expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz28peirZbqKQR1udNdaL4fWtRGggHygyF9D",
      "fee": "47",
      "counter": "5",
      "gas_limit": "78",
      "storage_limit": "7932",
      "public_key": "sppk7aDfaJGufpeUHY36yeuRu3YLrZqeroqF755SV33mxsZ14psWg9D"
    }'
    */
    func revealForging() {
        let publicKey = try! PublicKey.fromB58check(data: "sppk7aDfaJGufpeUHY36yeuRu3YLrZqeroqF755SV33mxsZ14psWg9D")
        let source = try! PublicKeyHash.fromB58check(data: "tz28peirZbqKQR1udNdaL4fWtRGggHygyF9D")
        let reveal = Operation.reveal(
          source: source,
          fee: 47,
          counter: 5,
          gasLimit: 78,
          storageLimit: 7932,
          publicKey: publicKey
        )
        let rawReveal = try! reveal.forge()
        let expectedBytes: [UInt8] = [
          0x6b, 0x01, 0x05, 0x90, 0x25, 0xd1, 0x96, 0x25, 0xd3, 0x6b,
          0x7d, 0xe3, 0x3b, 0x2a, 0xb0, 0xbf, 0x29, 0x54, 0x97, 0x5e,
          0xba, 0xb2, 0x2f, 0x05, 0x4e, 0xfc, 0x3d, 0x01, 0x02, 0x78,
          0x69, 0x09, 0x45, 0x5d, 0xa1, 0x64, 0x20, 0x60, 0x7a, 0x9c,
          0xff, 0xf3, 0xde, 0xce, 0x5f, 0x69, 0xf7, 0x98, 0x00, 0x2d,
          0x6d, 0x21, 0xdb, 0xa2, 0x96, 0x26, 0x6d, 0x53, 0x90, 0x23,
          0x42, 0x00
        ]
        XCTAssertEqual(Array(rawReveal), expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p",
      "fee": "0",
      "counter": "14",
      "gas_limit": "0",
      "storage_limit": "0",
      "amount": "0",
      "destination": "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p",
      "parameters": {
        "entrypoint": "finalize_unstake",
        "value": { "prim": "Unit" }
      }
    }'
    */
    func transactionForging() {
        let source = try! PublicKeyHash.fromB58check(data: "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p")
        let destination = try! Contract.fromB58check(data: "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p")
        let entrypoint = try! Entrypoint(name: "finalize_unstake")
        let transaction = Operation.transaction(
          source: source,
          fee: 0,
          counter: 14,
          gasLimit: 0,
          storageLimit: 0,
          amount: 0,
          destination: destination,
          entrypoint: entrypoint
        )
        let rawTransaction = try! transaction.forge()
        let expectedBytes: [UInt8] = [
          0x6c, 0x01, 0x58, 0x5a, 0x32, 0x14, 0x26, 0xfb, 0xb1, 0x30,
          0x3d, 0x16, 0xb5, 0x69, 0xe5, 0x71, 0x10, 0x9e, 0xb6, 0x8d,
          0x8c, 0x1b, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x00, 0x01, 0x58,
          0x5a, 0x32, 0x14, 0x26, 0xfb, 0xb1, 0x30, 0x3d, 0x16, 0xb5,
          0x69, 0xe5, 0x71, 0x10, 0x9e, 0xb6, 0x8d, 0x8c, 0x1b, 0xff,
          0x08, 0x00, 0x00, 0x00, 0x02, 0x03, 0x0b
        ]
        XCTAssertEqual(Array(rawTransaction), expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz2RcdU4n2PvJHUNYkS8FPuvcnFmBqEccxb4",
      "fee": "1",
      "counter": "30",
      "gas_limit": "500",
      "storage_limit": "7000",
      "balance": "90000",
      "delegate": "tz1XFq85mnnXhyhzpNEpxFvrkcuNtFBsSsVu",
      "script": {
        "code": [],
        "storage": {
          "prim": "unit"
        }
      }
    }'
    */
    func originationForging() {
        let delegate = try! PublicKeyHash.fromB58check(data: "tz1XFq85mnnXhyhzpNEpxFvrkcuNtFBsSsVu")
        let source = try! PublicKeyHash.fromB58check(data: "tz2RcdU4n2PvJHUNYkS8FPuvcnFmBqEccxb4")
        let origination = Operation.origination(
          source: source,
          fee: 1,
          counter: 30,
          gasLimit: 500,
          storageLimit: 7000,
          balance: 90000,
          delegate: delegate
        )
        let rawOrigination = try! origination.forge()
        let expectedBytes: [UInt8] = [
          0x6d, 0x01, 0xbd, 0xc4, 0x3f, 0x34, 0xca, 0x86, 0xf3, 0x28,
          0xac, 0x92, 0x16, 0xcf, 0xa9, 0x73, 0x2f, 0x1c, 0x39, 0x02,
          0x24, 0x5c, 0x01, 0x1e, 0xf4, 0x03, 0xd8, 0x36, 0x90, 0xbf,
          0x05, 0xff, 0x00, 0x7f, 0x6e, 0xb3, 0x50, 0x0f, 0x83, 0x77,
          0x2f, 0x53, 0xe7, 0x54, 0x03, 0xf9, 0x09, 0xb3, 0x10, 0x2f,
          0x10, 0xe8, 0x11, 0x00, 0x00, 0x00, 0x05, 0x02, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x03, 0x6c
        ]
        XCTAssertEqual(Array(rawOrigination), expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz1QFD9WqLWZmmAuqnnTPPUjfauitYEWdshv",
      "fee": "1",
      "counter": "2",
      "gas_limit": "3",
      "storage_limit": "4",
      "delegate": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
    }'
    */
    func delegationForging() {
        let delegate = try! PublicKeyHash.fromB58check(data: "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
        let source = try! PublicKeyHash.fromB58check(data: "tz1QFD9WqLWZmmAuqnnTPPUjfauitYEWdshv")
        let delegation = Operation.delegation(
          source: source,
          fee: 1,
          counter: 2,
          gasLimit: 3,
          storageLimit: 4,
          delegate: delegate
        )
        let rawDelegation = try! delegation.forge()
        let expectedBytes: [UInt8] = [
          0x6e, 0x00, 0x32, 0x87, 0xca, 0x0e, 0x27, 0x68, 0xbe, 0x95,
          0x4c, 0x01, 0x42, 0x78, 0x3b, 0xad, 0x9a, 0xe1, 0xb3, 0xda,
          0xe2, 0x00, 0x01, 0x02, 0x03, 0x04, 0xff, 0x00, 0x02, 0x29,
          0x8c, 0x03, 0xed, 0x7d, 0x45, 0x4a, 0x10, 0x1e, 0xb7, 0x02,
          0x2b, 0xc9, 0x5f, 0x7e, 0x5f, 0x41, 0xac, 0x78
        ]
        XCTAssertEqual(Array(rawDelegation), expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.unsigned" from '{
      "branch": "BLABQbLzzNcrgTgcumYBgCkFzgT9LSx7WK2SsjjZdcJGtTRtCzz",
      "contents": [
        {
          "kind": "transaction",
          "source": "tz2BctFU4ggoq31YSKu5L3MgSmFhvU7zDhY8",
          "fee": "537",
          "counter": "623698",
          "gas_limit": "248",
          "storage_limit": "735",
          "amount": "700000",
          "destination": "tz3i3aDaYUtfSnza4VC1XQKGFJgf99pFtoHk"
        }
      ]
    }'
    */
    func operationForging() {
        let source = try! PublicKeyHash.fromB58check(data: "tz2BctFU4ggoq31YSKu5L3MgSmFhvU7zDhY8")
        let destination = try! Contract.fromB58check(data: "tz3i3aDaYUtfSnza4VC1XQKGFJgf99pFtoHk")

        let transaction = Operation.transaction(
          source: source,
          fee: 537,
          counter: 623698,
          gasLimit: 248,
          storageLimit: 735,
          amount: 700000,
          destination: destination
        )

        let branch = try! BlockHash.fromB58check(data: "BLABQbLzzNcrgTgcumYBgCkFzgT9LSx7WK2SsjjZdcJGtTRtCzz")
        let operations = [transaction]

        let forgedOperation = try! forgeOperation(branch: branch, operations: operations)

        let expectedBytes: [UInt8] = [
          0x3a, 0xcd, 0xd9, 0xe7, 0xa0, 0xcd, 0x11, 0x20, 0x77, 0x3c,
          0x0f, 0xe0, 0x56, 0x7a, 0x31, 0x9e, 0xac, 0x66, 0x37, 0x11,
          0xb0, 0xbd, 0x4b, 0xd4, 0x55, 0x26, 0x2a, 0xde, 0x9d, 0x52,
          0x34, 0x70, 0x6c, 0x01, 0x24, 0x3e, 0xd3, 0x72, 0xff, 0xcb,
          0x2d, 0xc1, 0x5a, 0xed, 0x65, 0x9c, 0x84, 0xe7, 0x36, 0xf7,
          0xfa, 0xa7, 0xcb, 0x78, 0x99, 0x04, 0xd2, 0x88, 0x26, 0xf8,
          0x01, 0xdf, 0x05, 0xe0, 0xdc, 0x2a, 0x00, 0x02, 0xee, 0x35,
          0x3d, 0xbd, 0x57, 0x8e, 0xb2, 0x08, 0x9d, 0x7f, 0xcb, 0xa3,
          0x02, 0x07, 0x4d, 0x99, 0xec, 0x26, 0xef, 0xd2, 0x00
        ]
        XCTAssertEqual(Array(forgedOperation), expectedBytes)
    }
}
