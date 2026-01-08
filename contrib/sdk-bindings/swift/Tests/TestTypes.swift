// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestTypes: XCTestCase {

    public static var allTests = [
      ("testBigInt", testBigInt),
    ]

    func testBigInt() {
        let bigIntFromInt = BigInt.fromInt(num: 0)
        let bigIntFromString = try! BigInt.fromString(str: "0")

        XCTAssertEqual(bigIntFromInt, bigIntFromString)

        XCTAssertThrowsError(
          try BigInt.fromString(str: "foo")
        ) { error in
            XCTAssertEqual(
              error.localizedDescription,
              "Tezos.Error.Parsing(message: \"Parsing failure: \\\"invalid digit found in string\\\"\")"
            )
        }
    }

}
