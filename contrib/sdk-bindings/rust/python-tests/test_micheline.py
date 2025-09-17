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
    octez-client convert data "
    { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
    " from Michelson to binary
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

    raw_micheline = micheline_manager.encode(micheline)
    expected_bytes = bytes.fromhex('02000000170500036c0501036c050202000000080317053d036d0342')
    assert raw_micheline == expected_bytes

def test_build_simple_data_micheline():
    """
    octez-client convert data '(Pair :foo "string" 0 0x00)' from Michelson to binary
    """

    micheline = Micheline.APP(Prim.D_PAIR, [
        Micheline.STRING("string"),
        Micheline.INT(BigInt.from_int(0)),
        Micheline.BYTES(bytes.fromhex('00')),
    ], [Annotation.TYPE("foo")])

    micheline_manager = MichelineManager()

    parsed_micheline = micheline_manager.parse("""(Pair :foo "string" 0 0x00)""")
    assert micheline_manager.equal_micheline(micheline, parsed_micheline)

    raw_micheline = micheline_manager.encode(micheline)
    expected_bytes = bytes.fromhex('0907000000130100000006737472696e6700000a0000000100000000043a666f6f')
    assert raw_micheline == expected_bytes
