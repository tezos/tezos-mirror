// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestForging: XCTestCase {

    public static var allTests = [
      ("messageForging", messageForging),
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
}
