# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

from tezos import (
    Prim,
    Annotation,
    Micheline,
    BigInt,
    MichelineManager,
)

def test_build_basic_script_micheline():
    """
    { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
    """

    micheline = Micheline.SEQ([
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

    micheline_manager = MichelineManager()

    parsed_micheline = micheline_manager.parse(
        "{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }"
    )
    assert micheline_manager.equal_micheline(micheline, parsed_micheline)

def test_build_simple_data_micheline():
    """
    (Pair :foo "string" 0 0x00)
    """

    micheline = Micheline.APP(Prim.D_PAIR, [
        Micheline.STRING("string"),
        Micheline.INT(BigInt.from_int(0)),
        Micheline.BYTES(bytes.fromhex('00')),
    ], [Annotation.TYPE("foo")])

    micheline_manager = MichelineManager()

    parsed_micheline = micheline_manager.parse("""(Pair :foo "string" 0 0x00)""")
    assert micheline_manager.equal_micheline(micheline, parsed_micheline)
