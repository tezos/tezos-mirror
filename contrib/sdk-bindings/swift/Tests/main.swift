// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TezosTests: XCTestCase {

    public static var allTests = [
        ("testAdd", testAdd),
    ]

    func testAdd() {
        XCTAssertEqual(Tezos.add(left: 1, right: 2), 3)
    }
}

XCTMain(
  [
    testCase(TezosTests.allTests),
    testCase(TestBase58Check.allTests),
  ]
)
