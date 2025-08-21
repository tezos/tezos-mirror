/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.assertFalse

class KeysTest {

    @Test fun blpkToTz4() {
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        val b58Pk = "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp"
        val expectedB58Pkh = "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
        val pk = PublicKeyBls.fromB58check(b58Pk)
        val pkh = pk.pkHash()
        val b58Pkh = pkh.toB58check()
        assertEquals(expectedB58Pkh, b58Pkh)
    }

    @Test fun pkToPkh() {
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        val b58Pk = "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ"
        val expectedB58Pkh = "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
        val pk = PublicKey.fromB58check(b58Pk)
        val pkh = pk.pkHash()
        val b58Pkh = pkh.toB58check()
        assertEquals(expectedB58Pkh, b58Pkh)
    }

    @Test fun blsigIntoGenericSig() {
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        // msg: "a"
        val b58Sig = "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx"
        val sig = BlsSignature.fromB58check(b58Sig)
        val genericSig = sig.intoGeneric()
        assertEquals(genericSig.toB58check(), b58Sig)
        val genericSig2 = Signature.fromBlsSignature(sig)
        assertEquals(genericSig2.toB58check(), b58Sig)
    }

    @Test fun sigFromGenericSig() {
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        val b58Sig = "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        val expectedB58Sig = "sigUkkYXApp64hzZ2CrmsSTw8Unn8GF5zSaJu15qE29hT4aEAhVCdbiCmnY3DAZMP1aAZBHjdSUfXcx5oFs84k8S4FhnDrUk"
        val genericSig = Signature.fromB58check(b58Sig)
        val sig = UnknownSignature.tryFromGeneric(genericSig)
        assertEquals(sig.toB58check(), expectedB58Sig)
        val sig2 = genericSig.tryIntoUnknownSignature()
        assertEquals(sig2.toB58check(), expectedB58Sig)
    }

    @Test fun verifyP2sig() {
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        val b58Pk = "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G"
        val b58Sig = "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        val msg = "a"
        val pk = PublicKeyP256.fromB58check(b58Pk)
        val sig = P256Signature.fromB58check(b58Sig)
        val rawMsg = forgeMessage(msg)
        assertTrue(pk.verifySignature(sig, rawMsg))
    }

    @Test fun verifySig() {
        // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
        // msg: "a"
        val b58Pk = "edpktvpansLmKrvHCS1aWkFHS6gJdSd5haH1Z74MJFAAeNDSuSgHBH"
        val b58Sig = "sigWrzQCbre6B7VLP4kGntoQGrEBLLvc8cFPySNiDj5m2cTd4DfJG2feBLhgyjtHcTiLenwrActDgp9y6pxp3MS5m5sqVCY2"
        val msg = "a"
        val pk = PublicKey.fromB58check(b58Pk)
        val sig = Signature.fromB58check(b58Sig)
        val rawMsg = forgeMessage(msg)
        assertTrue(pk.verifySignature(sig, rawMsg))
    }

    @Test fun verifyWrongSig() {
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        val b58Pk = "sppk7aBZCsBJTTDTV1Lwo4eZBnqmSTSnChYF1GxHUsgeWumUCHcZyxv"
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        // msg: "a"
        val b58Sig = "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF"
        val msg = "a"
        val pk = PublicKeySecp256k1.fromB58check(b58Pk)
        val sig = Secp256k1Signature.fromB58check(b58Sig)
        val rawMsg = forgeMessage(msg)
        assertFalse(pk.verifySignature(sig, rawMsg))
    }
}
