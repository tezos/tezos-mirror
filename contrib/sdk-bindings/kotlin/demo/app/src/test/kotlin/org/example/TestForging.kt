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
      "kind": "transaction",
      "source": "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
      "fee": "30",
      "counter": "20",
      "gas_limit": "54",
      "storage_limit": "45",
      "amount": "0",
      "destination": "KT1WtTP1yGNfJLvBSubDV1H5z1zsKKXzAyGS",
      "parameters": {
        "entrypoint": "add_liquidity",
        "value": {
          "prim": "Pair",
          "args": [
            { "int": "93735" },
            { "prim": "Pair", "args": [
              { "int": "0" },
              { "string": "tz3c2XcRc6PhNuFwH1EhYcXtv7gBgdsx8k6J" }
            ] }
          ]
        }
      }
    }'
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test fun transactionForging() {
        val source = PublicKeyHash.fromB58check("tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ")
        val destination = Contract.fromB58check("KT1WtTP1yGNfJLvBSubDV1H5z1zsKKXzAyGS")
        val entrypoint = Entrypoint("add_liquidity")
	// octez-client convert data '(Pair 93735 (Pair 0 "tz3c2XcRc6PhNuFwH1EhYcXtv7gBgdsx8k6J"))' from Michelson to binary
	val value = "070700a7b80b070700000100000024747a336332586352633650684e75467748314568596358747637674267647378386b364a".hexToByteArray()
        val transaction = Transaction(source = source, fee = 30UL, counter = 20UL, gasLimit = 54UL, storageLimit = 45UL, amount = 0UL, destination = destination, entrypoint = entrypoint, value = value)
        val rawTransaction = transaction.forge()
        val expectedBytes = "6c03db557924e5a295652eff2c1f141d5a5b72b9cc911e14362d0001f4ab1300637efe3616ab27663ee0e4be07d80e4c00ffff0d6164645f6c697175696469747900000033070700a7b80b070700000100000024747a336332586352633650684e75467748314568596358747637674267647378386b364a".hexToByteArray()
        assertContentEquals(rawTransaction, expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL",
      "fee": "72",
      "counter": "41",
      "gas_limit": "8017",
      "storage_limit": "77",
      "balance": "400",
      "delegate": "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL",
      "script": {
        "code": [],
        "storage": {
          "prim": "unit"
        }
      }
    }'
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test fun OriginationForging() {
        val source = PublicKeyHash.fromB58check("tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL")
        val origination = Origination(source = source, fee = 72UL, counter = 41UL, gasLimit = 8017UL, storageLimit = 77UL, balance = 400UL, delegate = source)
        val rawOrigination = origination.forge()
        val expectedBytes = "6d03421585a37470f6cbba7aeb70d11bc40b2b23dd684829d13e4d9003ff03421585a37470f6cbba7aeb70d11bc40b2b23dd6800000005020000000000000002036c".hexToByteArray()
        assertContentEquals(rawOrigination, expectedBytes)
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
