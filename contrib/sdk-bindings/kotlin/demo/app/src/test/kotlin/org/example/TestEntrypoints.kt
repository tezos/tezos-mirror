/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals

class EntrypointsTest {

    @Test fun buildEntrypoint() {
        val entrypoint = Entrypoint("")
        assertEquals(entrypoint.toString(), "default")
    }
}
