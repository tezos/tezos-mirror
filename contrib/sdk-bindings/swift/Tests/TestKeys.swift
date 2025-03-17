// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestKeys: XCTestCase {

    public static var allTests = [
      ("blpkToTz4", blpkToTz4),
      ("pkToPkh", pkToPkh),
      ("verifyP2sig", verifyP2sig),
      ("verifySig", verifySig),
      ("verifyWrongSig", verifyWrongSig),
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

    func verifyP2sig() {
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        let b58Pk = "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G"
        let b58Sig = "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        let msg = "a"
        let pk = try! PublicKeyP256.fromB58check(data: b58Pk)
        let sig = try! P256Signature.fromB58check(data: b58Sig)
        let rawMsg = try! forgeMessage(msg: msg)
        XCTAssert(try! pk.verifySignature(signature: sig, msg: rawMsg))
    }

    func verifySig() {
        // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
        // msg: "a"
        let b58Pk = "edpktvpansLmKrvHCS1aWkFHS6gJdSd5haH1Z74MJFAAeNDSuSgHBH"
        let b58Sig = "sigWrzQCbre6B7VLP4kGntoQGrEBLLvc8cFPySNiDj5m2cTd4DfJG2feBLhgyjtHcTiLenwrActDgp9y6pxp3MS5m5sqVCY2"
        let msg = "a"
        let pk = try! PublicKey.fromB58check(data: b58Pk)
        let sig = try! Signature.fromB58check(data: b58Sig)
        let rawMsg = try! forgeMessage(msg: msg)
        XCTAssert(try! pk.verifySignature(signature: sig, msg: rawMsg))
    }

    func verifyWrongSig() {
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        let b58Pk = "sppk7aBZCsBJTTDTV1Lwo4eZBnqmSTSnChYF1GxHUsgeWumUCHcZyxv"
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        // msg: "a"
        let b58Sig = "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF"
        let msg = "a"
        let pk = try! PublicKey.fromB58check(data: b58Pk)
        let sig = try! Signature.fromB58check(data: b58Sig)
        let rawMsg = try! forgeMessage(msg: msg)
        XCTAssertFalse(try! pk.verifySignature(signature: sig, msg: rawMsg))
    }
}
