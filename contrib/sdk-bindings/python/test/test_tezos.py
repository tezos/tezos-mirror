# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

import tezos


def test_add():
    res = tezos.add(1, 2)
    assert (res == 3), f"{res} != 3"
