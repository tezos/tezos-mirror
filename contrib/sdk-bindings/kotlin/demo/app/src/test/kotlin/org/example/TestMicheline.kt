/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertTrue

class TestMicheline {

    /*
    { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
    */
    @Test
    fun testBuildBasicScriptMicheline() {
        val micheline = Micheline.Seq(
            listOf(
                Micheline.App(
                    Prim.K_PARAMETER,
                    listOf(Micheline.App(Prim.T_UNIT, emptyList(), emptyList())),
                    emptyList()
                ),
                Micheline.App(
                    Prim.K_STORAGE,
                    listOf(Micheline.App(Prim.T_UNIT, emptyList(), emptyList())),
                    emptyList()
                ),
                Micheline.App(
                    Prim.K_CODE,
                    listOf(
                        Micheline.Seq(
                            listOf(
                                Micheline.App(Prim.I_CDR, emptyList(), emptyList()),
                                Micheline.App(
                                    Prim.I_NIL,
                                    listOf(Micheline.App(Prim.T_OPERATION, emptyList(), emptyList())),
                                    emptyList()
                                ),
                                Micheline.App(Prim.I_PAIR, emptyList(), emptyList()),
                            )
                        )
                    ), emptyList()
                ),
            )
        )

        val michelineManager = MichelineManager()

        val parsedMicheline = michelineManager.parse(
            "{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }"
        )
        assertTrue(michelineManager.equalMicheline(micheline, parsedMicheline))
    }

    /*
    (Pair :foo "string" 0 0x00)
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test
    fun testBuildSimpleDataMicheline() {
        val micheline = Micheline.App(
            Prim.D_PAIR,
            listOf(
                Micheline.String("string"),
                Micheline.Int(BigInt.fromInt(0L)),
                Micheline.Bytes(byteArrayOf(0x00)),
            ),
            listOf(Annotation.Type("foo"))
        )

        val michelineManager = MichelineManager()

        val parsedMicheline = michelineManager.parse("""(Pair :foo "string" 0 0x00)""")
        assertTrue(michelineManager.equalMicheline(micheline, parsedMicheline))
    }

}
