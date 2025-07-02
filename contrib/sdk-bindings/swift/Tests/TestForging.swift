// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestForging: XCTestCase {

    public static var allTests = [
      ("messageForging", messageForging),
      ("delegationForging", delegationForging),
    ]

    func messageForging() {
        let msg = "message"
        let rawMsg = try! forgeMessage(msg: msg)
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
        let delegation = Delegation(
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
}
