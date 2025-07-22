// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestEntrypoints: XCTestCase {

    public static var allTests = [
      ("buildEntrypoint", buildEntrypoint),
    ]

    func buildEntrypoint() {
        let rawEntrypoint = "my_custom_entrypoint"
        let entrypoint = try! Entrypoint(name: rawEntrypoint)
        XCTAssertEqual(String(describing: entrypoint), rawEntrypoint)
    }
}
