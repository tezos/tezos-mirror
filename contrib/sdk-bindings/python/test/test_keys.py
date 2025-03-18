# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    PublicKeyBls,
    PublicKey,
)


def test_blpk_to_tz4():
    # sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
    b58_pk = "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp"
    expected_b58_pkh = "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
    pk = PublicKeyBls.from_b58check(b58_pk)
    pkh = pk.pk_hash()
    b58_pkh = pkh.to_b58check()
    assert b58_pkh == expected_b58_pkh


def test_pk_to_pkh():
    # sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
    b58_pk = "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ"
    expected_b58_pkh = "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu"
    pk = PublicKey.from_b58check(b58_pk)
    pkh = pk.pk_hash()
    b58_pkh = pkh.to_b58check()
    assert b58_pkh == expected_b58_pkh
