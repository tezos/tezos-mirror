// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestBase58Check: XCTestCase {

    public static var allTests = [
      ("tz1DecodeEncode", tz1DecodeEncode),
      ("blpkDecodeEncode", blpkDecodeEncode),
      ("pkDecodeEncode", pkDecodeEncode),
      ("decodeInvalidChecksum", decodeInvalidChecksum),
    ]

    func tz1DecodeEncode() {
        // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
        let baseB58Pkh = "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2VJ"
        let pkh = try! ContractTz1Hash.fromB58check(data: baseB58Pkh)
        let b58Pkh = pkh.toB58check()
        XCTAssertEqual(baseB58Pkh, b58Pkh)
    }

    func blpkDecodeEncode() {
        // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
        let baseB58Pk = "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
        let pk = try! PublicKeyBls.fromB58check(data: baseB58Pk)
        let b58Pk = pk.toB58check()
        XCTAssertEqual(baseB58Pk, b58Pk)
    }

    func pkDecodeEncode() {
        // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
        let baseB58Pk = "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B"
        let pk = try! PublicKey.fromB58check(data: baseB58Pk)
        let b58Pk = pk.toB58check()
        XCTAssertEqual(baseB58Pk, b58Pk)
    }

    func decodeInvalidChecksum() {
        XCTAssertThrowsError(
          try ContractTz1Hash.fromB58check(data: "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2Vj")
        ) { error in
            XCTAssertEqual(
              error.localizedDescription,
              "Tezos.Error.Base58(message: \"Base58check conversion failure: InvalidChecksum\")"
            )
        }
    }
}
