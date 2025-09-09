# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    Prim,
    Annotation,
    Micheline,
    BigInt,
)

def test_build_basic_script_micheline():
    """
    parameter unit;
    storage unit;
    code {CDR; NIL operation; PAIR};
    """

    Micheline.SEQ([
        Micheline.APP(Prim.K_PARAMETER, [Micheline.APP(Prim.T_UNIT, [], [])], []),
        Micheline.APP(Prim.K_STORAGE, [Micheline.APP(Prim.T_UNIT, [], [])], []),
        Micheline.APP(Prim.K_CODE, [
            Micheline.SEQ([
                Micheline.APP(Prim.I_CDR, [], []),
                Micheline.APP(Prim.I_NIL, [Micheline.APP(Prim.T_OPERATION, [], [])], []),
                Micheline.APP(Prim.I_PAIR, [], []),
            ])
        ], []),
    ])

def test_build_simple_data_micheline():
    """
    (Pair :foo "string" 0 0x00)
    """

    Micheline.APP(Prim.D_PAIR, [
        Micheline.STRING("string"),
        Micheline.INT(BigInt.from_int(0)),
        Micheline.BYTES(bytes.fromhex('00')),
    ], [Annotation.TYPE("foo")])
