# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

import tezos

def test_message_forging():
    msg = "message"
    raw_msg = tezos.forge_message(msg)
    expected_bytes = bytes([
        0x05, 0x01, 0x00, 0x00, 0x00, 0x07,
        ord('m'), ord('e'), ord('s'), ord('s'),
        ord('a'), ord('g'), ord('e')
    ])
    assert raw_msg == expected_bytes
