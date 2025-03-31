// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest

XCTMain(
  [
    testCase(TestBase58Check.allTests),
    testCase(TestKeys.allTests),
    testCase(TestForging.allTests),
  ]
)
