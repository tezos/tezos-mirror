/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertTrue
import kotlin.test.assertContentEquals

class TestMicheline {

    /*
    octez-client convert data "
    { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
    " from Michelson to binary
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
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

        val rawMicheline = michelineManager.encode(micheline)
        val expectedBytes = "02000000170500036c0501036c050202000000080317053d036d0342".hexToByteArray()
        assertContentEquals(rawMicheline, expectedBytes)
    }

    /*
    octez-client convert data '(Pair :foo "string" 0 0x00)' from Michelson to binary
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

        val rawMicheline = michelineManager.encode(micheline)
        val expectedBytes = "0907000000130100000006737472696e6700000a0000000100000000043a666f6f".hexToByteArray()
        assertContentEquals(rawMicheline, expectedBytes)
    }

}
