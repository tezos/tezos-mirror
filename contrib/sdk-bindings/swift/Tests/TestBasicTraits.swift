// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestBasicTraits: XCTestCase {

    public static var allTests = [
      ("testContractTz4HashEquality", testContractTz4HashEquality),
      ("testSignatureDisplay", testSignatureDisplay),
      ("testPublicKeySecp256k1Debug", testPublicKeySecp256k1Debug),
    ]

    func testContractTz4HashEquality() {
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        let rawTz41 = "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
        // sk: BLsk2SdiXbRuYrWkfkSDbN1tCBGjGV7tTHxjVrokaiJsv17rDd8scd
        let rawTz42 = "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ"
        let tz4 = try! ContractTz4Hash.fromB58check(data: rawTz41)
        let tz41 = try! ContractTz4Hash.fromB58check(data: rawTz41)
        let tz42 = try! ContractTz4Hash.fromB58check(data: rawTz42)

        XCTAssertEqual(tz4, tz41)
        XCTAssertNotEqual(tz4, tz42)
    }

    func testSignatureDisplay() {
        // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
        // msg: "a"
        let rawSig = "edsigtzr9dzEN2jY59fMGEoFb1X99KPg5JaJzDsDTLraJgxHKQQEp6XMcXYEAAjV1oJno67WQCf1z3KxhYE8jWUmLLfz113HM19"
        let sig = try! Signature.fromB58check(data: rawSig)

        XCTAssertEqual(sig.description, rawSig)
    }

    func testPublicKeySecp256k1Debug() {
        // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
        let rawPk = "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B"
        let pk = try! PublicKeySecp256k1.fromB58check(data: rawPk)

        XCTAssertTrue(pk.debugDescription.contains(rawPk))
    }
}
