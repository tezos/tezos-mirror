// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest

XCTMain(
  [
    testCase(TestBase58Check.allTests),
    testCase(TestBasicTraits.allTests),
    testCase(TestKeys.allTests),
    testCase(TestForging.allTests),
    testCase(TestEntrypoints.allTests),
    testCase(TestTypes.allTests),
    testCase(TestMicheline.allTests),
  ]
)
