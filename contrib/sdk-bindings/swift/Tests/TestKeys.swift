// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestKeys: XCTestCase {

    public static var allTests = [
      ("blpkToTz4", blpkToTz4),
      ("pkToPkh", pkToPkh),
      ("sigIntoGenericSig", sigIntoGenericSig),
      ("blsSigFromGenericSig", blsSigFromGenericSig),
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

    func sigIntoGenericSig() {
        // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
        // msg: "a"
        let b58Sig = "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp"
        let sig = try! UnknownSignature.fromB58check(data: b58Sig)
        let genericSig = sig.intoGeneric()
        XCTAssertEqual(genericSig.toB58check(), b58Sig)
        let genericSig2 = Signature.fromUnknownSignature(sig: sig)
        XCTAssertEqual(genericSig2.toB58check(), b58Sig)
    }

    func blsSigFromGenericSig() {
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        // msg: "a"
        let b58Sig = "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx"
        let genericSig = try! Signature.fromB58check(data: b58Sig)
        let sig = try! BlsSignature.tryFromGeneric(sig: genericSig)
        XCTAssertEqual(sig.toB58check(), b58Sig)
        let sig2 = try! genericSig.tryIntoBlsSignature()
        XCTAssertEqual(sig2.toB58check(), b58Sig)
    }

    func verifyP2sig() {
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        let b58Pk = "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G"
        let b58Sig = "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        let msg = "a"
        let pk = try! PublicKeyP256.fromB58check(data: b58Pk)
        let sig = try! P256Signature.fromB58check(data: b58Sig)
        let rawMsg = forgeMessage(msg: msg)
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
        let rawMsg = forgeMessage(msg: msg)
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
        let rawMsg = forgeMessage(msg: msg)
        XCTAssertFalse(try! pk.verifySignature(signature: sig, msg: rawMsg))
    }
}
