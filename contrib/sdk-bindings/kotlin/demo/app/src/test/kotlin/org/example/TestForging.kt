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
        val reveal = Operation.reveal(source = source, fee = 1000UL, counter = 34UL, gasLimit = 1UL, storageLimit = 16UL, publicKey = publicKey)
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
        val transaction = Operation.transaction(source = source, fee = 30UL, counter = 20UL, gasLimit = 54UL, storageLimit = 45UL, amount = 0UL, destination = destination, entrypoint = entrypoint, value = value)
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
        val origination = Operation.origination(source = source, fee = 72UL, counter = 41UL, gasLimit = 8017UL, storageLimit = 77UL, balance = 400UL, delegate = source)
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
        val delegation = Operation.delegation(source = source, fee = 0UL, counter = 54UL, gasLimit = 82UL, storageLimit = 107006UL, delegate = null)
        val rawDelegation = delegation.forge()
        val expectedBytes = "6e03db557924e5a295652eff2c1f141d5a5b72b9cc91003652fec30600".hexToByteArray()
        assertContentEquals(rawDelegation, expectedBytes)
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.unsigned" from '{
      "branch": "BMUjWA9SovWoMPzvjXyxzu1xZo8P3zNZHTKXivGRFFwaJBb856Q",
      "contents": [
        {
          "kind": "reveal",
          "source": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h",
          "fee": "449",
          "counter": "444131",
          "gas_limit": "981",
          "storage_limit": "0",
          "public_key": "BLpk1wNkwwNS8QFmRfeB79DMHV9gRcg5gnGL4QqpKjqY87RLNJMwUmKhvvYSbD4Lo4omysqdnV1z",
          "proof": "BLsig9eDgfCYSwPbWQhGVDUHcnGscTqPuNTtbEwTJjjXcczUnSGNMdqdtupYtTfwzhJLpeTqnsxPzzbg7B4g3cdkky7gJZCyL53dkc4ULzFHJDi4gY9NF4EZvJ7mUZcoWpPC8sRn52ZYPs"
        },
        {
          "kind": "delegation",
          "source": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h",
          "fee": "843",
          "counter": "444132",
          "gas_limit": "511",
          "storage_limit": "86",
          "delegate": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h"
        },
        {
          "kind": "origination",
          "source": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h",
          "fee": "686",
          "counter": "444133",
          "gas_limit": "456",
          "storage_limit": "428",
          "balance": "0",
          "delegate": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h",
          "script": {
            "code": [],
            "storage": {
              "prim": "unit"
            }
          }
        },
        {
          "kind": "transaction",
          "source": "tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h",
          "fee": "547",
          "counter": "444134",
          "gas_limit": "968",
          "storage_limit": "47",
          "amount": "0",
          "destination": "KT1NYriLXv6V5GaggQscXH5YZooeVQEmomEu",
          "parameters": {
            "entrypoint": "default",
            "value": { "int": "13" }
          }
        }
      ]
    }'
    */
    @OptIn(kotlin.ExperimentalStdlibApi::class) // `hexToByteArray` is experimental
    @Test fun operationForging() {
        val pkh = PublicKeyHash.fromB58check("tz4TWB5Rmg9bqwn8wMpYRaaPax9NCtGSUN5h")
        val pk = PublicKey.fromB58check("BLpk1wNkwwNS8QFmRfeB79DMHV9gRcg5gnGL4QqpKjqY87RLNJMwUmKhvvYSbD4Lo4omysqdnV1z")

        val reveal = Operation.reveal(
            source = pkh,
            fee = 449UL,
            counter = 444131UL,
            gasLimit = 981UL,
            storageLimit = 0UL,
            publicKey = pk,
            proof = BlsSignature.fromB58check("BLsig9eDgfCYSwPbWQhGVDUHcnGscTqPuNTtbEwTJjjXcczUnSGNMdqdtupYtTfwzhJLpeTqnsxPzzbg7B4g3cdkky7gJZCyL53dkc4ULzFHJDi4gY9NF4EZvJ7mUZcoWpPC8sRn52ZYPs")
        )
        val delegation = Operation.delegation(
            source = pkh,
            fee = 843UL,
            counter = 444132UL,
            gasLimit = 511UL,
            storageLimit = 86UL,
            delegate = pkh
        )
        val origination = Operation.origination(
            source = pkh,
            fee = 686UL,
            counter = 444133UL,
            gasLimit = 456UL,
            storageLimit = 428UL,
            balance = 0UL,
            delegate = pkh
        )
        val transaction = Operation.transaction(
            source = pkh,
            fee = 547UL,
            counter = 444134UL,
            gasLimit = 968UL,
            storageLimit = 47UL,
            amount = 0UL,
            destination = Contract.fromB58check("KT1NYriLXv6V5GaggQscXH5YZooeVQEmomEu"),
            // octez-client convert data '13' from Michelson to binary
            value = "000d".hexToByteArray()
        )

        val branch = BlockHash.fromB58check("BMUjWA9SovWoMPzvjXyxzu1xZo8P3zNZHTKXivGRFFwaJBb856Q")
        val operations = listOf(reveal, delegation, origination, transaction)

        val forgedOperation = forgeOperation(branch, operations)

        val expectedBytes = "e8a0bf2fced89c606cbd506141c5db242c10d583f3e8db55b62e4b796a930a8b6b03caea906857692736dd9d8440bab0638cc57d15a8c103e38d1bd5070003acb5946d66a3a423951f864714200ba27dd58ba59e58f5b8e5579c53c4dc498cec391cb69b257889ebbb3dc52d25d1f3ff0000006083822b70fa68eceea7f18986efe9db0be816c44d4818faf7a04a9a2dfca72ea5d1d238761c941dca0a8c84979bdbe8230cf15973b0feecefc29d02117729571edb9f2ed6b618d46e23947b124914582c637aac0e3d069bf712a07bb2e7ae53546e03caea906857692736dd9d8440bab0638cc57d15a8cb06e48d1bff0356ff03caea906857692736dd9d8440bab0638cc57d15a86d03caea906857692736dd9d8440bab0638cc57d15a8ae05e58d1bc803ac0300ff03caea906857692736dd9d8440bab0638cc57d15a800000005020000000000000002036c6c03caea906857692736dd9d8440bab0638cc57d15a8a304e68d1bc8072f0001993539e91d1794f14316375d0a1d98b014ece3f700ff0000000002000d".hexToByteArray()
        assertContentEquals(forgedOperation, expectedBytes)
    }
}
