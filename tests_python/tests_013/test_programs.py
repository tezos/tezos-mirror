import itertools
from client.client import Client

CONVERT_INPUT_FORMATS = ["michelson", "json", "binary"]
CONVERT_OUTPUT_FORMATS = ["michelson", "json", "binary", "ocaml"]
CONVERT_SCRIPT = {
    "michelson": """{ parameter unit ;
  storage unit ;
  code { CDR ;
         NIL operation ;
         SELF ;
         PUSH mutez 0 ;
         UNIT ;
         TRANSFER_TOKENS ;
         DUP ;
         DIP { CONS } ;
         CONS ;
         PAIR } }""",
    "json": """[ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
  { "prim": "storage", "args": [ { "prim": "unit" } ] },
  { "prim": "code",
    "args":
      [ [ { "prim": "CDR" },
          { "prim": "NIL", "args": [ { "prim": "operation" } ] },
          { "prim": "SELF" },
          { "prim": "PUSH", "args": [ { "prim": "mutez" }, { "int": "0" } ] },
          { "prim": "UNIT" }, { "prim": "TRANSFER_TOKENS" },
          { "prim": "DUP" },
          { "prim": "DIP", "args": [ [ { "prim": "CONS" } ] ] },
          { "prim": "CONS" }, { "prim": "PAIR" } ] ] } ]""",
    "binary": "0x02000000300500036c0501036c050202000000210317053d036d03490743036a0000034f034d0321051f0200000002031b031b0342",  # pylint: disable=line-too-long # noqa: E501
    "ocaml": "Seq (0, [Prim (1, K_parameter, [Prim (2, T_unit, [], [])], []); Prim (3, K_storage, [Prim (4, T_unit, [], [])], []); Prim (5, K_code, [Seq (6, [Prim (7, I_CDR, [], []); Prim (8, I_NIL, [Prim (9, T_operation, [], [])], []); Prim (10, I_SELF, [], []); Prim (11, I_PUSH, [Prim (12, T_mutez, [], []); Int (13, Z.zero)], []); Prim (14, I_UNIT, [], []); Prim (15, I_TRANSFER_TOKENS, [], []); Prim (16, I_DUP, [], []); Prim (17, I_DIP, [Seq (18, [Prim (19, I_CONS, [], [])])], []); Prim (20, I_CONS, [], []); Prim (21, I_PAIR, [], [])])], [])])",  # pylint: disable=line-too-long # noqa: E501
}
CONVERT_DATA = {
    "michelson": """{ DROP ;
  PUSH address "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" ;
  CONTRACT unit ;
  { IF_NONE { { UNIT ; FAILWITH } } {} } ;
  PUSH mutez 1 ;
  UNIT ;
  TRANSFER_TOKENS ;
  DIP { NIL operation } ;
  CONS }""",
    "json": """[ { "prim": "DROP" },
  { "prim": "PUSH",
    "args":
      [ { "prim": "address" },
        { "string": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" } ] },
  { "prim": "CONTRACT", "args": [ { "prim": "unit" } ] },
  [ { "prim": "IF_NONE",
      "args": [ [ [ { "prim": "UNIT" }, { "prim": "FAILWITH" } ] ], [] ] } ],
  { "prim": "PUSH", "args": [ { "prim": "mutez" }, { "int": "1" } ] },
  { "prim": "UNIT" }, { "prim": "TRANSFER_TOKENS" },
  { "prim": "DIP",
    "args": [ [ { "prim": "NIL", "args": [ { "prim": "operation" } ] } ] ] },
  { "prim": "CONS" } ]""",
    "binary": "0x020000006403200743036e0100000024747a31666173774354446369527a45346f4a396a6e32566d3264766a6579413966557a550555036c0200000015072f02000000090200000004034f032702000000000743036a0001034f034d051f0200000004053d036d031b",  # pylint: disable=line-too-long # noqa: E501
    "ocaml": "Seq (0, [Prim (1, I_DROP, [], []); Prim (2, I_PUSH, [Prim (3, T_address, [], []); String (4, \"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU\")], []); Prim (5, I_CONTRACT, [Prim (6, T_unit, [], [])], []); Seq (7, [Prim (8, I_IF_NONE, [Seq (9, [Seq (10, [Prim (11, I_UNIT, [], []); Prim (12, I_FAILWITH, [], [])])]); Seq (13, [])], [])]); Prim (14, I_PUSH, [Prim (15, T_mutez, [], []); Int (16, Z.one)], []); Prim (17, I_UNIT, [], []); Prim (18, I_TRANSFER_TOKENS, [], []); Prim (19, I_DIP, [Seq (20, [Prim (21, I_NIL, [Prim (22, T_operation, [], [])], [])])], []); Prim (23, I_CONS, [], [])])",  # pylint: disable=line-too-long # noqa: E501
    "type": "lambda unit (list operation)",
}


class TestProgramsCommands:
    def test_convert_script(self, client: Client):
        for (input_, output) in itertools.product(
            CONVERT_INPUT_FORMATS, CONVERT_OUTPUT_FORMATS
        ):
            result = client.run(
                [
                    "convert",
                    "script",
                    CONVERT_SCRIPT[input_],
                    "from",
                    input_,
                    "to",
                    output,
                ]
            )
            assert result.strip() == f"{CONVERT_SCRIPT[output]}"

    def test_convert_data(self, client: Client):
        for (input_, output, typecheck) in itertools.product(
            CONVERT_INPUT_FORMATS, CONVERT_OUTPUT_FORMATS, [True, False]
        ):
            args = [
                "convert",
                "data",
                CONVERT_DATA[input_],
                "from",
                input_,
                "to",
                output,
            ]
            if typecheck:
                args += ["--type", CONVERT_DATA["type"]]
            result = client.run(args)
            assert result.strip() == f"{CONVERT_DATA[output]}"
