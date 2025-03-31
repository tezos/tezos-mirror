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
}
