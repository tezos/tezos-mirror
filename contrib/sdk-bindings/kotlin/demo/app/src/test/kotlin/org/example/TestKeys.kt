/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
 *
 * SPDX-License-Identifier: MIT
 */

package org.example.tezos

import kotlin.test.Test
import kotlin.test.assertEquals

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
}
