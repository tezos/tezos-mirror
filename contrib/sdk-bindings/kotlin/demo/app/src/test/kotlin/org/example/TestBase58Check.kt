/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class Base58CheckTest {

    @Test fun tz1DecodeEncode() {
        // sk: edsk3x51ok91omWQd5ZBzBuMV3pjCyFCjxh4fyoTRJ4QYuk59sjZch
        val baseB58Pkh = "tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2VJ"
        val pkh = ContractTz1Hash.fromB58check(baseB58Pkh)
        val b58Pkh = pkh.toB58check()
        assertEquals(baseB58Pkh, b58Pkh)
    }

    @Test fun blpkDecodeEncode() {
        // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
        val baseB58Pk = "BLpk1xwPVMQMstf4Nt41TGYeFNeck9RQJ7pHGqWpJQYgsQqGSPnhfoMMmU6fTzr1BD3mPLyzCoQB"
        val pk = PublicKeyBls.fromB58check(baseB58Pk)
        val b58Pk = pk.toB58check()
        assertEquals(baseB58Pk, b58Pk)
    }

    @Test fun pkDecodeEncode() {
        // sk: spsk236JVFEqgJtsg4EbVeMjcppsumD1CdwNiBnP1YWeDhHubWRh1Z
        val baseB58Pk = "sppk7ZMef32rXEYsxrAE7jjLVqrcbHPRA3ikSSrywzREro52PPKU42B"
        val pk = PublicKey.fromB58check(baseB58Pk)
        val b58Pk = pk.toB58check()
        assertEquals(baseB58Pk, b58Pk)
    }

    @Test fun spSig() {
        // sk: spsk1ppL4ohtyZeighKZehzfGr2p6dL51kwQqEV2N1sNT7rx9cg5jG
        // msg: "a"
        val baseB58Sig = "spsig1Dm7vPsHHTNdeYrTYtyBjEgwmdcMpCRBEHctmE63qQ4Cbt78sNxLYFG2qQq7ouQwqAqH2hGXr7fi2fAJh2WLUtvDFbSH2e"
        val sig = Secp256k1Signature.fromB58check(baseB58Sig)
        val b58Sig = sig.toB58check()
        assertEquals(baseB58Sig, b58Sig)
    }

    @Test fun blSig() {
        // sk: BLsk27Wo9tVS8XbNDfCD7X9FnQBwZ4o2gQCZxXkJRmdFYoUTvuTu1S
        // msg: "a"
        val baseB58Sig = "BLsigB5MLy2Godk2FSEfWyi1A8XsvgMmZq6siYN7fdTiu5kRapXtrd7Ebit97qinPmF9j5zpPGGoe7Du6JB2Ybpo9839z4E6CSQRNCR3FRqAfWYdoMLz5cTYjmYT1MczxXahrnV8JtZzNF"
        val sig = BlsSignature.fromB58check(baseB58Sig)
        val b58Sig = sig.toB58check()
        assertEquals(baseB58Sig, b58Sig)
    }

    @Test fun contractDecodeEncode() {
        val baseB58Contract = "tz3dvgoUnnXoAP9MDqAwiZwRtvTFHcuDMxTR"
        val contract = Contract.fromB58check(baseB58Contract)
        val b58Contract = contract.toB58check()
        assertEquals(baseB58Contract, b58Contract)
    }

    @Test fun decodeInvalidChecksum() {
        val exception = assertFailsWith<Exception.Base58>(
            "Converting hash with invalid checksum must fail"
        ) {
            ContractTz1Hash.fromB58check("tz1XnHxtAwqiJzwNicuU1A1haG2RW5sxJ2Vj")
        }
        assertEquals(exception.message, "Base58check conversion failure: InvalidChecksum")
    }
}
