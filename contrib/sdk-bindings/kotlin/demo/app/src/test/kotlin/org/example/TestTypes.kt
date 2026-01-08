/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

class Types {

    @Test
    fun testBigInt() {
        val bigIntFromInt = BigInt.fromInt(Long.MIN_VALUE)
        val bigIntFromString = BigInt.fromString("-9223372036854775808")

        assertEquals(bigIntFromInt, bigIntFromString)

        val otherBigIntFromString = BigInt.fromString("-9223372036854775809")

        assertNotEquals(bigIntFromInt, otherBigIntFromString)
    }

}
