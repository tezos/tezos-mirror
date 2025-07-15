/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertContentEquals

class ForgingTest {

    @Test fun messageForging() {
        val msg = "message"
        val rawMsg = forgeMessage(msg)
        val expectedBytes = byteArrayOf(
            0x05, 0x01, 0x00, 0x00, 0x00, 0x07,
            'm'.code.toByte(), 'e'.code.toByte(), 's'.code.toByte(),
            's'.code.toByte(), 'a'.code.toByte(), 'g'.code.toByte(),
            'e'.code.toByte()
        )
        assertContentEquals(rawMsg, expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz3NGKzVp8ezNnu5qx8mY3iioSUXKfC1d8Yc",
      "fee": "1000",
      "counter": "34",
      "gas_limit": "1",
      "storage_limit": "16",
      "public_key": "p2pk66qj2rKg5s6NCHprzPHheLK1dV3ZmGJbfBJEAizxsz2o3qms8vF"
    }'
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test fun revealForging() {
        val publicKey = PublicKey.fromB58check("p2pk66qj2rKg5s6NCHprzPHheLK1dV3ZmGJbfBJEAizxsz2o3qms8vF")
        val source = PublicKeyHash.fromB58check("tz3NGKzVp8ezNnu5qx8mY3iioSUXKfC1d8Yc")
        val reveal = Reveal(source = source, fee = 1000UL, counter = 34UL, gasLimit = 1UL, storageLimit = 16UL, publicKey = publicKey)
        val rawReveal = reveal.forge()
        val expectedBytes = "6b02153c42139fbbe509e9023bb85eac281709766070e80722011002032abac5ad6fc0fbe8e9a0beb3bbbc7481318f8b686b366ceb00ee0b3b51e40c4900".hexToByteArray()
        assertContentEquals(rawReveal, expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
      "fee": "0",
      "counter": "54",
      "gas_limit": "82",
      "storage_limit": "107006"
    }'
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test fun removeDelegateForging() {
        val source = PublicKeyHash.fromB58check("tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ")
        val delegation = Delegation(source = source, fee = 0UL, counter = 54UL, gasLimit = 82UL, storageLimit = 107006UL, delegate = null)
        val rawDelegation = delegation.forge()
        val expectedBytes = "6e03db557924e5a295652eff2c1f141d5a5b72b9cc91003652fec30600".hexToByteArray()
        assertContentEquals(rawDelegation, expectedBytes)
    }
}
