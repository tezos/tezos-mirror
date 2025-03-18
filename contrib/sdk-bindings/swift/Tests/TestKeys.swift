// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestKeys: XCTestCase {

    public static var allTests = [
      ("blpkToTz4", blpkToTz4),
      ("pkToPkh", pkToPkh),
    ]

    func blpkToTz4() {
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        let b58Pk = "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp"
        let expectedB58Pkh = "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
        let pk = try! PublicKeyBls.fromB58check(data: b58Pk)
        let pkh = pk.pkHash()
        let b58Pkh = pkh.toB58check()
        XCTAssertEqual(b58Pkh, expectedB58Pkh)
    }

    func pkToPkh() {
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        let b58Pk = "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ"
        let expectedB58Pkh = "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
        let pk = try! PublicKey.fromB58check(data: b58Pk)
        let pkh = pk.pkHash()
        let b58Pkh = pkh.toB58check()
        XCTAssertEqual(b58Pkh, expectedB58Pkh)
    }
}
