/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.assertNotEquals

class BasicTraits {

    @Test
    fun testUnknownSignatureEquality() {
        // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
        // msg: "a"
        val rawSig1 = "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp"
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        // msg: "a"
        val rawSig2 = "sigmstyCA7dwXvY7UXo8mkUG47WPCybhngBYvHokpnReuZdKZwPoXK8aKp4sEZgAhxeJsnNyJ68Q4zdJyJuSw25JTjxgtVpA"
        val sig = UnknownSignature.fromB58check(rawSig1)
        val sig1 = UnknownSignature.fromB58check(rawSig1)
        val sig2 = UnknownSignature.fromB58check(rawSig2)
        assertEquals(sig, sig1)
        assertNotEquals(sig, sig2)
    }

    @Test
    fun testPublicKeyEd25519Display() {
        // sk: edsk4BYt5bFFa7rHeoZSxgbTUJFYJjSynS9tFgFBzjQpRTL4Gku2x2
        val rawPk = "edpkuWbLgKBsLVDE5dn678Y4ogZUjBQtCdh4HebhfJ9CaTR2o6WpFp"
        val pk = PublicKeyEd25519.fromB58check(rawPk)
        assertEquals(pk.toString(), rawPk)
    }

    // No derivation for the Debug trait in Kotlin
}
