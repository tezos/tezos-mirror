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
      ("p2SigDecodeEncode", p2SigDecodeEncode),
      ("sigDecodeEncode", sigDecodeEncode),
      ("kt1DecodeEncode", kt1DecodeEncode),
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

    func p2SigDecodeEncode() {
        // sk: p2sk432A1xe56akKM7n8vk4g7tSyTTWodJgcwwH9mSmA6jjrApUB3F
        // msg: "a"
        let baseB58Sig = "p2sigqUk7jhcXFUQqLkRhb7o3oTyp7hbttCenbcrsg4qmmh4HjM4fHfXExj7raBTFoaJJKt3oR3sUjy4V163rkQLx9pAYERW7r"
        let sig = try! P256Signature.fromB58check(data: baseB58Sig)
        let b58Sig = sig.toB58check()
        XCTAssertEqual(baseB58Sig, b58Sig)
    }

    func sigDecodeEncode() {
        // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
        // msg: "a"
        let baseB58Sig = "edsigtgNwQE9rwEWL8Jr7AL414yUN1ZvVK6dZrCf5fuXUxn9q8Te2SVbzFcw5VdgKipscVoYAcvFyAnJDB7MRQRswPGKY4DY73h"
        let sig = try! Signature.fromB58check(data: baseB58Sig)
        let b58Sig = sig.toB58check()
        XCTAssertEqual(baseB58Sig, b58Sig)
    }

    func kt1DecodeEncode() {
        let baseB58Kt1 = "KT1DvqNXfdigfmDeh2zyF9Q4mTFruyUr1rmv"
        let kt1 = try! ContractKt1Hash.fromB58check(data: baseB58Kt1)
        let b58Kt1 = kt1.toB58check()
        XCTAssertEqual(baseB58Kt1, b58Kt1)
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
