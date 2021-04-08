open Michelson_v1_primitives
open Micheline

let script =
  Seq
    ( 0,
      [ Prim
          ( 1,
            K_parameter,
            [ Prim
                ( 2,
                  T_or,
                  [ Prim
                      ( 3,
                        T_or,
                        [ Prim
                            ( 4,
                              T_or,
                              [ Prim
                                  ( 5,
                                    T_pair,
                                    [ Prim (6, T_address, [], ["%spender"]);
                                      Prim (7, T_nat, [], ["%value"]) ],
                                    ["%approve"] );
                                Prim
                                  ( 8,
                                    T_pair,
                                    [ Prim
                                        ( 9,
                                          T_pair,
                                          [ Prim (10, T_address, [], ["%owner"]);
                                            Prim
                                              (11, T_address, [], ["%spender"])
                                          ],
                                          ["%request"] );
                                      Prim
                                        ( 12,
                                          T_contract,
                                          [Prim (13, T_nat, [], [])],
                                          ["%callback"] ) ],
                                    ["%getAllowance"] ) ],
                              [] );
                          Prim
                            ( 14,
                              T_or,
                              [ Prim
                                  ( 15,
                                    T_pair,
                                    [ Prim (16, T_address, [], ["%owner"]);
                                      Prim
                                        ( 17,
                                          T_contract,
                                          [Prim (18, T_nat, [], [])],
                                          ["%callback"] ) ],
                                    ["%getBalance"] );
                                Prim
                                  ( 19,
                                    T_pair,
                                    [ Prim (20, T_unit, [], ["%request"]);
                                      Prim
                                        ( 21,
                                          T_contract,
                                          [Prim (22, T_nat, [], [])],
                                          ["%callback"] ) ],
                                    ["%getTotalSupply"] ) ],
                              [] ) ],
                        [] );
                    Prim
                      ( 23,
                        T_or,
                        [ Prim
                            ( 24,
                              T_pair,
                              [ Prim (25, T_int, [], ["%quantity"]);
                                Prim (26, T_address, [], ["%target"]) ],
                              ["%mintOrBurn"] );
                          Prim
                            ( 27,
                              T_pair,
                              [ Prim (28, T_address, [], ["%from"]);
                                Prim
                                  ( 29,
                                    T_pair,
                                    [ Prim (30, T_address, [], ["%to"]);
                                      Prim (31, T_nat, [], ["%value"]) ],
                                    [] ) ],
                              ["%transfer"] ) ],
                        [] ) ],
                  [] ) ],
            [] );
        Prim
          ( 32,
            K_storage,
            [ Prim
                ( 33,
                  T_pair,
                  [ Prim
                      ( 34,
                        T_big_map,
                        [Prim (35, T_address, [], []); Prim (36, T_nat, [], [])],
                        ["%tokens"] );
                    Prim
                      ( 37,
                        T_pair,
                        [ Prim
                            ( 38,
                              T_big_map,
                              [ Prim
                                  ( 39,
                                    T_pair,
                                    [ Prim (40, T_address, [], ["%owner"]);
                                      Prim (41, T_address, [], ["%spender"]) ],
                                    [] );
                                Prim (42, T_nat, [], []) ],
                              ["%allowances"] );
                          Prim
                            ( 43,
                              T_pair,
                              [ Prim (44, T_address, [], ["%admin"]);
                                Prim (45, T_nat, [], ["%total_supply"]) ],
                              [] ) ],
                        [] ) ],
                  [] ) ],
            [] );
        Prim
          ( 46,
            K_code,
            [ Seq
                ( 47,
                  [ Prim (48, I_DUP, [], []);
                    Prim (49, I_CDR, [], []);
                    Prim
                      ( 50,
                        I_PUSH,
                        [Prim (51, T_mutez, [], []); Int (52, Z.zero)],
                        [] );
                    Prim (53, I_AMOUNT, [], []);
                    Prim (54, I_COMPARE, [], []);
                    Prim (55, I_NEQ, [], []);
                    Prim
                      ( 56,
                        I_IF,
                        [ Seq
                            ( 57,
                              [ Prim
                                  ( 58,
                                    I_PUSH,
                                    [ Prim (59, T_string, [], []);
                                      String (60, "DontSendTez") ],
                                    [] );
                                Prim (61, I_FAILWITH, [], []) ] );
                          Seq (62, []) ],
                        [] );
                    Prim (63, I_SWAP, [], []);
                    Prim (64, I_CAR, [], []);
                    Prim
                      ( 65,
                        I_IF_LEFT,
                        [ Seq
                            ( 66,
                              [ Prim
                                  ( 67,
                                    I_IF_LEFT,
                                    [ Seq
                                        ( 68,
                                          [ Prim
                                              ( 69,
                                                I_IF_LEFT,
                                                [ Seq
                                                    ( 70,
                                                      [ Prim
                                                          (71, I_SWAP, [], []);
                                                        Prim (72, I_DUP, [], []);
                                                        Prim
                                                          ( 73,
                                                            I_DUG,
                                                            [ Int
                                                                (74, Z.of_int 2)
                                                            ],
                                                            [] );
                                                        Prim (75, I_CDR, [], []);
                                                        Prim (76, I_CAR, [], []);
                                                        Prim
                                                          (77, I_SWAP, [], []);
                                                        Prim (78, I_DUP, [], []);
                                                        Prim
                                                          ( 79,
                                                            I_DUG,
                                                            [ Int
                                                                (80, Z.of_int 2)
                                                            ],
                                                            [] );
                                                        Prim (81, I_CAR, [], []);
                                                        Prim
                                                          (82, I_SENDER, [], []);
                                                        Prim
                                                          (83, I_PAIR, [], []);
                                                        Prim
                                                          ( 84,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 85,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (86, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 87,
                                                            I_DIG,
                                                            [ Int
                                                                (88, Z.of_int 3)
                                                            ],
                                                            [] );
                                                        Prim (89, I_DUP, [], []);
                                                        Prim
                                                          ( 90,
                                                            I_DUG,
                                                            [ Int
                                                                (91, Z.of_int 4)
                                                            ],
                                                            [] );
                                                        Prim (92, I_CDR, [], []);
                                                        Prim
                                                          ( 93,
                                                            I_COMPARE,
                                                            [],
                                                            [] );
                                                        Prim (94, I_GT, [], []);
                                                        Prim
                                                          ( 95,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 96,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (97, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 98,
                                                            I_DIG,
                                                            [ Int
                                                                (99, Z.of_int 3)
                                                            ],
                                                            [] );
                                                        Prim
                                                          (100, I_DUP, [], []);
                                                        Prim
                                                          ( 101,
                                                            I_DUG,
                                                            [ Int
                                                                ( 102,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 103,
                                                            I_DIG,
                                                            [ Int
                                                                ( 104,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (105, I_DUP, [], []);
                                                        Prim
                                                          ( 106,
                                                            I_DUG,
                                                            [ Int
                                                                ( 107,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (108, I_GET, [], []);
                                                        Prim
                                                          ( 109,
                                                            I_IF_NONE,
                                                            [ Seq
                                                                ( 110,
                                                                  [ Prim
                                                                      ( 111,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 112,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            );
                                                                          Int
                                                                            ( 113,
                                                                              Z
                                                                              .zero
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq (114, []) ],
                                                            [] );
                                                        Prim
                                                          ( 115,
                                                            I_COMPARE,
                                                            [],
                                                            [] );
                                                        Prim (116, I_GT, [], []);
                                                        Prim
                                                          (117, I_AND, [], []);
                                                        Prim
                                                          ( 118,
                                                            I_IF,
                                                            [ Seq
                                                                ( 119,
                                                                  [ Prim
                                                                      ( 120,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 121,
                                                                              T_string,
                                                                              [],
                                                                              []
                                                                            );
                                                                          String
                                                                            ( 122,
                                                                              "UnsafeAllowanceChange"
                                                                            )
                                                                        ],
                                                                        [] );
                                                                    Prim
                                                                      ( 123,
                                                                        I_FAILWITH,
                                                                        [],
                                                                        [] ) ]
                                                                );
                                                              Seq (124, []) ],
                                                            [] );
                                                        Prim
                                                          ( 125,
                                                            I_DIG,
                                                            [ Int
                                                                ( 126,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (127, I_DUP, [], []);
                                                        Prim
                                                          ( 128,
                                                            I_DUG,
                                                            [ Int
                                                                ( 129,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (130, I_CDR, [], []);
                                                        Prim
                                                          (131, I_CDR, [], []);
                                                        Prim
                                                          ( 132,
                                                            I_DIG,
                                                            [ Int
                                                                ( 133,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 134,
                                                            I_DIG,
                                                            [ Int
                                                                ( 135,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (136, I_CDR, [], []);
                                                        Prim
                                                          ( 137,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 138,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (139, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          (140, I_SWAP, [], []);
                                                        Prim
                                                          (141, I_DUP, [], []);
                                                        Prim
                                                          ( 142,
                                                            I_DUG,
                                                            [ Int
                                                                ( 143,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 144,
                                                            I_COMPARE,
                                                            [],
                                                            [] );
                                                        Prim (145, I_EQ, [], []);
                                                        Prim
                                                          ( 146,
                                                            I_IF,
                                                            [ Seq
                                                                ( 147,
                                                                  [ Prim
                                                                      ( 148,
                                                                        I_DROP,
                                                                        [],
                                                                        [] );
                                                                    Prim
                                                                      ( 149,
                                                                        I_NONE,
                                                                        [ Prim
                                                                            ( 150,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq
                                                                ( 151,
                                                                  [ Prim
                                                                      ( 152,
                                                                        I_SOME,
                                                                        [],
                                                                        [] ) ]
                                                                ) ],
                                                            [] );
                                                        Prim
                                                          ( 153,
                                                            I_DIG,
                                                            [ Int
                                                                ( 154,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 155,
                                                            I_UPDATE,
                                                            [],
                                                            [] );
                                                        Prim
                                                          (156, I_PAIR, [], []);
                                                        Prim
                                                          (157, I_SWAP, [], []);
                                                        Prim
                                                          (158, I_CAR, [], []);
                                                        Prim
                                                          (159, I_PAIR, [], []);
                                                        Prim
                                                          ( 160,
                                                            I_NIL,
                                                            [ Prim
                                                                ( 161,
                                                                  T_operation,
                                                                  [],
                                                                  [] ) ],
                                                            [] );
                                                        Prim
                                                          (162, I_PAIR, [], [])
                                                      ] );
                                                  Seq
                                                    ( 163,
                                                      [ Prim
                                                          (164, I_SWAP, [], []);
                                                        Prim
                                                          (165, I_DUP, [], []);
                                                        Prim
                                                          ( 166,
                                                            I_DIG,
                                                            [ Int
                                                                ( 167,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 168,
                                                            I_NIL,
                                                            [ Prim
                                                                ( 169,
                                                                  T_operation,
                                                                  [],
                                                                  [] ) ],
                                                            [] );
                                                        Prim
                                                          (170, I_SWAP, [], []);
                                                        Prim
                                                          (171, I_DUP, [], []);
                                                        Prim
                                                          ( 172,
                                                            I_DUG,
                                                            [ Int
                                                                ( 173,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (174, I_CDR, [], []);
                                                        Prim
                                                          ( 175,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 176,
                                                                  T_mutez,
                                                                  [],
                                                                  [] );
                                                              Int (177, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 178,
                                                            I_DIG,
                                                            [ Int
                                                                ( 179,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (180, I_CDR, [], []);
                                                        Prim
                                                          (181, I_CAR, [], []);
                                                        Prim
                                                          ( 182,
                                                            I_DIG,
                                                            [ Int
                                                                ( 183,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (184, I_CAR, [], []);
                                                        Prim
                                                          (185, I_GET, [], []);
                                                        Prim
                                                          ( 186,
                                                            I_IF_NONE,
                                                            [ Seq
                                                                ( 187,
                                                                  [ Prim
                                                                      ( 188,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 189,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            );
                                                                          Int
                                                                            ( 190,
                                                                              Z
                                                                              .zero
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq (191, []) ],
                                                            [] );
                                                        Prim
                                                          ( 192,
                                                            I_TRANSFER_TOKENS,
                                                            [],
                                                            [] );
                                                        Prim
                                                          (193, I_CONS, [], []);
                                                        Prim
                                                          (194, I_PAIR, [], [])
                                                      ] ) ],
                                                [] ) ] );
                                      Seq
                                        ( 195,
                                          [ Prim
                                              ( 196,
                                                I_IF_LEFT,
                                                [ Seq
                                                    ( 197,
                                                      [ Prim
                                                          (198, I_SWAP, [], []);
                                                        Prim
                                                          (199, I_DUP, [], []);
                                                        Prim
                                                          ( 200,
                                                            I_DIG,
                                                            [ Int
                                                                ( 201,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 202,
                                                            I_NIL,
                                                            [ Prim
                                                                ( 203,
                                                                  T_operation,
                                                                  [],
                                                                  [] ) ],
                                                            [] );
                                                        Prim
                                                          (204, I_SWAP, [], []);
                                                        Prim
                                                          (205, I_DUP, [], []);
                                                        Prim
                                                          ( 206,
                                                            I_DUG,
                                                            [ Int
                                                                ( 207,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (208, I_CDR, [], []);
                                                        Prim
                                                          ( 209,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 210,
                                                                  T_mutez,
                                                                  [],
                                                                  [] );
                                                              Int (211, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 212,
                                                            I_DIG,
                                                            [ Int
                                                                ( 213,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (214, I_CAR, [], []);
                                                        Prim
                                                          ( 215,
                                                            I_DIG,
                                                            [ Int
                                                                ( 216,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (217, I_CAR, [], []);
                                                        Prim
                                                          (218, I_GET, [], []);
                                                        Prim
                                                          ( 219,
                                                            I_IF_NONE,
                                                            [ Seq
                                                                ( 220,
                                                                  [ Prim
                                                                      ( 221,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 222,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            );
                                                                          Int
                                                                            ( 223,
                                                                              Z
                                                                              .zero
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq (224, []) ],
                                                            [] );
                                                        Prim
                                                          ( 225,
                                                            I_TRANSFER_TOKENS,
                                                            [],
                                                            [] );
                                                        Prim
                                                          (226, I_CONS, [], []);
                                                        Prim
                                                          (227, I_PAIR, [], [])
                                                      ] );
                                                  Seq
                                                    ( 228,
                                                      [ Prim
                                                          (229, I_SWAP, [], []);
                                                        Prim
                                                          (230, I_DUP, [], []);
                                                        Prim
                                                          ( 231,
                                                            I_DIG,
                                                            [ Int
                                                                ( 232,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 233,
                                                            I_NIL,
                                                            [ Prim
                                                                ( 234,
                                                                  T_operation,
                                                                  [],
                                                                  [] ) ],
                                                            [] );
                                                        Prim
                                                          (235, I_SWAP, [], []);
                                                        Prim
                                                          (236, I_CDR, [], []);
                                                        Prim
                                                          ( 237,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 238,
                                                                  T_mutez,
                                                                  [],
                                                                  [] );
                                                              Int (239, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 240,
                                                            I_DIG,
                                                            [ Int
                                                                ( 241,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (242, I_CDR, [], []);
                                                        Prim
                                                          (243, I_CDR, [], []);
                                                        Prim
                                                          (244, I_CDR, [], []);
                                                        Prim
                                                          ( 245,
                                                            I_TRANSFER_TOKENS,
                                                            [],
                                                            [] );
                                                        Prim
                                                          (246, I_CONS, [], []);
                                                        Prim
                                                          (247, I_PAIR, [], [])
                                                      ] ) ],
                                                [] ) ] ) ],
                                    [] ) ] );
                          Seq
                            ( 248,
                              [ Prim
                                  ( 249,
                                    I_IF_LEFT,
                                    [ Seq
                                        ( 250,
                                          [ Prim (251, I_SWAP, [], []);
                                            Prim (252, I_DUP, [], []);
                                            Prim
                                              ( 253,
                                                I_DUG,
                                                [Int (254, Z.of_int 2)],
                                                [] );
                                            Prim (255, I_CDR, [], []);
                                            Prim (256, I_CDR, [], []);
                                            Prim (257, I_CAR, [], []);
                                            Prim (258, I_SENDER, [], []);
                                            Prim (259, I_COMPARE, [], []);
                                            Prim (260, I_NEQ, [], []);
                                            Prim
                                              ( 261,
                                                I_IF,
                                                [ Seq
                                                    ( 262,
                                                      [ Prim
                                                          ( 263,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 264,
                                                                  T_string,
                                                                  [],
                                                                  [] );
                                                              String
                                                                ( 265,
                                                                  "OnlyAdmin"
                                                                ) ],
                                                            [] );
                                                        Prim
                                                          ( 266,
                                                            I_FAILWITH,
                                                            [],
                                                            [] ) ] );
                                                  Seq (267, []) ],
                                                [] );
                                            Prim (268, I_DUP, [], []);
                                            Prim (269, I_CAR, [], []);
                                            Prim
                                              ( 270,
                                                I_DIG,
                                                [Int (271, Z.of_int 2)],
                                                [] );
                                            Prim (272, I_DUP, [], []);
                                            Prim
                                              ( 273,
                                                I_DUG,
                                                [Int (274, Z.of_int 3)],
                                                [] );
                                            Prim (275, I_CAR, [], []);
                                            Prim
                                              ( 276,
                                                I_DIG,
                                                [Int (277, Z.of_int 2)],
                                                [] );
                                            Prim (278, I_DUP, [], []);
                                            Prim
                                              ( 279,
                                                I_DUG,
                                                [Int (280, Z.of_int 3)],
                                                [] );
                                            Prim (281, I_CDR, [], []);
                                            Prim (282, I_GET, [], []);
                                            Prim
                                              ( 283,
                                                I_IF_NONE,
                                                [ Seq
                                                    ( 284,
                                                      [ Prim
                                                          ( 285,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 286,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (287, Z.zero)
                                                            ],
                                                            [] ) ] );
                                                  Seq (288, []) ],
                                                [] );
                                            Prim (289, I_ADD, [], []);
                                            Prim (290, I_ISNAT, [], []);
                                            Prim
                                              ( 291,
                                                I_IF_NONE,
                                                [ Seq
                                                    ( 292,
                                                      [ Prim
                                                          ( 293,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 294,
                                                                  T_string,
                                                                  [],
                                                                  [] );
                                                              String
                                                                ( 295,
                                                                  "Cannot \
                                                                   burn more \
                                                                   than the \
                                                                   target's \
                                                                   balance." )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 296,
                                                            I_FAILWITH,
                                                            [],
                                                            [] ) ] );
                                                  Seq (297, []) ],
                                                [] );
                                            Prim (298, I_SWAP, [], []);
                                            Prim (299, I_DUP, [], []);
                                            Prim
                                              ( 300,
                                                I_DUG,
                                                [Int (301, Z.of_int 2)],
                                                [] );
                                            Prim (302, I_CAR, [], []);
                                            Prim
                                              ( 303,
                                                I_DIG,
                                                [Int (304, Z.of_int 3)],
                                                [] );
                                            Prim (305, I_DUP, [], []);
                                            Prim
                                              ( 306,
                                                I_DUG,
                                                [Int (307, Z.of_int 4)],
                                                [] );
                                            Prim (308, I_CDR, [], []);
                                            Prim (309, I_CDR, [], []);
                                            Prim (310, I_CDR, [], []);
                                            Prim (311, I_ADD, [], []);
                                            Prim (312, I_ABS, [], []);
                                            Prim
                                              ( 313,
                                                I_DIG,
                                                [Int (314, Z.of_int 3)],
                                                [] );
                                            Prim (315, I_DUP, [], []);
                                            Prim
                                              ( 316,
                                                I_DUG,
                                                [Int (317, Z.of_int 4)],
                                                [] );
                                            Prim (318, I_CDR, [], []);
                                            Prim
                                              ( 319,
                                                I_DIG,
                                                [Int (320, Z.of_int 4)],
                                                [] );
                                            Prim (321, I_CAR, [], []);
                                            Prim
                                              ( 322,
                                                I_PUSH,
                                                [ Prim (323, T_nat, [], []);
                                                  Int (324, Z.zero) ],
                                                [] );
                                            Prim
                                              ( 325,
                                                I_DIG,
                                                [Int (326, Z.of_int 4)],
                                                [] );
                                            Prim (327, I_DUP, [], []);
                                            Prim
                                              ( 328,
                                                I_DUG,
                                                [Int (329, Z.of_int 5)],
                                                [] );
                                            Prim (330, I_COMPARE, [], []);
                                            Prim (331, I_EQ, [], []);
                                            Prim
                                              ( 332,
                                                I_IF,
                                                [ Seq
                                                    ( 333,
                                                      [ Prim
                                                          ( 334,
                                                            I_DIG,
                                                            [ Int
                                                                ( 335,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (336, I_DROP, [], []);
                                                        Prim
                                                          ( 337,
                                                            I_NONE,
                                                            [ Prim
                                                                ( 338,
                                                                  T_nat,
                                                                  [],
                                                                  [] ) ],
                                                            [] ) ] );
                                                  Seq
                                                    ( 339,
                                                      [ Prim
                                                          ( 340,
                                                            I_DIG,
                                                            [ Int
                                                                ( 341,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (342, I_SOME, [], [])
                                                      ] ) ],
                                                [] );
                                            Prim
                                              ( 343,
                                                I_DIG,
                                                [Int (344, Z.of_int 4)],
                                                [] );
                                            Prim (345, I_CDR, [], []);
                                            Prim (346, I_UPDATE, [], []);
                                            Prim (347, I_PAIR, [], []);
                                            Prim (348, I_DUP, [], []);
                                            Prim
                                              ( 349,
                                                I_DUG,
                                                [Int (350, Z.of_int 2)],
                                                [] );
                                            Prim (351, I_CDR, [], []);
                                            Prim (352, I_CDR, [], []);
                                            Prim (353, I_CAR, [], []);
                                            Prim (354, I_PAIR, [], []);
                                            Prim (355, I_SWAP, [], []);
                                            Prim (356, I_DUP, [], []);
                                            Prim
                                              ( 357,
                                                I_DUG,
                                                [Int (358, Z.of_int 2)],
                                                [] );
                                            Prim (359, I_CDR, [], []);
                                            Prim (360, I_CAR, [], []);
                                            Prim (361, I_PAIR, [], []);
                                            Prim (362, I_SWAP, [], []);
                                            Prim (363, I_CAR, [], []);
                                            Prim (364, I_PAIR, [], []);
                                            Prim
                                              ( 365,
                                                I_NIL,
                                                [ Prim
                                                    (366, T_operation, [], [])
                                                ],
                                                [] );
                                            Prim (367, I_PAIR, [], []) ] );
                                      Seq
                                        ( 368,
                                          [ Prim (369, I_SWAP, [], []);
                                            Prim (370, I_DUP, [], []);
                                            Prim
                                              ( 371,
                                                I_DUG,
                                                [Int (372, Z.of_int 2)],
                                                [] );
                                            Prim (373, I_CDR, [], []);
                                            Prim (374, I_CAR, [], []);
                                            Prim
                                              ( 375,
                                                I_DIG,
                                                [Int (376, Z.of_int 2)],
                                                [] );
                                            Prim (377, I_DUP, [], []);
                                            Prim
                                              ( 378,
                                                I_DUG,
                                                [Int (379, Z.of_int 3)],
                                                [] );
                                            Prim (380, I_CAR, [], []);
                                            Prim
                                              ( 381,
                                                I_DIG,
                                                [Int (382, Z.of_int 2)],
                                                [] );
                                            Prim (383, I_DUP, [], []);
                                            Prim
                                              ( 384,
                                                I_DUG,
                                                [Int (385, Z.of_int 3)],
                                                [] );
                                            Prim (386, I_CAR, [], []);
                                            Prim (387, I_SENDER, [], []);
                                            Prim (388, I_COMPARE, [], []);
                                            Prim (389, I_EQ, [], []);
                                            Prim
                                              ( 390,
                                                I_IF,
                                                [ Seq
                                                    ( 391,
                                                      [ Prim
                                                          (392, I_SWAP, [], [])
                                                      ] );
                                                  Seq
                                                    ( 393,
                                                      [ Prim
                                                          ( 394,
                                                            I_SENDER,
                                                            [],
                                                            [] );
                                                        Prim
                                                          ( 395,
                                                            I_DIG,
                                                            [ Int
                                                                ( 396,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (397, I_DUP, [], []);
                                                        Prim
                                                          ( 398,
                                                            I_DUG,
                                                            [ Int
                                                                ( 399,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (400, I_CAR, [], []);
                                                        Prim
                                                          (401, I_PAIR, [], []);
                                                        Prim
                                                          ( 402,
                                                            I_DIG,
                                                            [ Int
                                                                ( 403,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (404, I_DUP, [], []);
                                                        Prim
                                                          ( 405,
                                                            I_DUG,
                                                            [ Int
                                                                ( 406,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (407, I_CDR, [], []);
                                                        Prim
                                                          (408, I_CDR, [], []);
                                                        Prim
                                                          ( 409,
                                                            I_DIG,
                                                            [ Int
                                                                ( 410,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (411, I_DUP, [], []);
                                                        Prim
                                                          ( 412,
                                                            I_DUG,
                                                            [ Int
                                                                ( 413,
                                                                  Z.of_int 4 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 414,
                                                            I_DIG,
                                                            [ Int
                                                                ( 415,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (416, I_DUP, [], []);
                                                        Prim
                                                          ( 417,
                                                            I_DUG,
                                                            [ Int
                                                                ( 418,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (419, I_GET, [], []);
                                                        Prim
                                                          ( 420,
                                                            I_IF_NONE,
                                                            [ Seq
                                                                ( 421,
                                                                  [ Prim
                                                                      ( 422,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 423,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            );
                                                                          Int
                                                                            ( 424,
                                                                              Z
                                                                              .zero
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq (425, []) ],
                                                            [] );
                                                        Prim
                                                          (426, I_SUB, [], []);
                                                        Prim
                                                          (427, I_ISNAT, [], []);
                                                        Prim
                                                          ( 428,
                                                            I_IF_NONE,
                                                            [ Seq
                                                                ( 429,
                                                                  [ Prim
                                                                      ( 430,
                                                                        I_PUSH,
                                                                        [ Prim
                                                                            ( 431,
                                                                              T_string,
                                                                              [],
                                                                              []
                                                                            );
                                                                          String
                                                                            ( 432,
                                                                              "NotEnoughAllowance"
                                                                            )
                                                                        ],
                                                                        [] );
                                                                    Prim
                                                                      ( 433,
                                                                        I_FAILWITH,
                                                                        [],
                                                                        [] ) ]
                                                                );
                                                              Seq (434, []) ],
                                                            [] );
                                                        Prim
                                                          ( 435,
                                                            I_DIG,
                                                            [ Int
                                                                ( 436,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 437,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 438,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (439, Z.zero)
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 440,
                                                            I_DIG,
                                                            [ Int
                                                                ( 441,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          (442, I_DUP, [], []);
                                                        Prim
                                                          ( 443,
                                                            I_DUG,
                                                            [ Int
                                                                ( 444,
                                                                  Z.of_int 3 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 445,
                                                            I_COMPARE,
                                                            [],
                                                            [] );
                                                        Prim (446, I_EQ, [], []);
                                                        Prim
                                                          ( 447,
                                                            I_IF,
                                                            [ Seq
                                                                ( 448,
                                                                  [ Prim
                                                                      ( 449,
                                                                        I_SWAP,
                                                                        [],
                                                                        [] );
                                                                    Prim
                                                                      ( 450,
                                                                        I_DROP,
                                                                        [],
                                                                        [] );
                                                                    Prim
                                                                      ( 451,
                                                                        I_NONE,
                                                                        [ Prim
                                                                            ( 452,
                                                                              T_nat,
                                                                              [],
                                                                              []
                                                                            )
                                                                        ],
                                                                        [] ) ]
                                                                );
                                                              Seq
                                                                ( 453,
                                                                  [ Prim
                                                                      ( 454,
                                                                        I_SWAP,
                                                                        [],
                                                                        [] );
                                                                    Prim
                                                                      ( 455,
                                                                        I_SOME,
                                                                        [],
                                                                        [] ) ]
                                                                ) ],
                                                            [] );
                                                        Prim
                                                          ( 456,
                                                            I_DIG,
                                                            [ Int
                                                                ( 457,
                                                                  Z.of_int 2 )
                                                            ],
                                                            [] );
                                                        Prim
                                                          ( 458,
                                                            I_UPDATE,
                                                            [],
                                                            [] ) ] ) ],
                                                [] );
                                            Prim
                                              ( 459,
                                                I_DIG,
                                                [Int (460, Z.of_int 2)],
                                                [] );
                                            Prim (461, I_DUP, [], []);
                                            Prim
                                              ( 462,
                                                I_DUG,
                                                [Int (463, Z.of_int 3)],
                                                [] );
                                            Prim (464, I_CDR, [], []);
                                            Prim (465, I_CDR, [], []);
                                            Prim
                                              ( 466,
                                                I_DIG,
                                                [Int (467, Z.of_int 2)],
                                                [] );
                                            Prim (468, I_DUP, [], []);
                                            Prim
                                              ( 469,
                                                I_DUG,
                                                [Int (470, Z.of_int 3)],
                                                [] );
                                            Prim
                                              ( 471,
                                                I_DIG,
                                                [Int (472, Z.of_int 4)],
                                                [] );
                                            Prim (473, I_DUP, [], []);
                                            Prim
                                              ( 474,
                                                I_DUG,
                                                [Int (475, Z.of_int 5)],
                                                [] );
                                            Prim (476, I_CAR, [], []);
                                            Prim (477, I_GET, [], []);
                                            Prim
                                              ( 478,
                                                I_IF_NONE,
                                                [ Seq
                                                    ( 479,
                                                      [ Prim
                                                          ( 480,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 481,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (482, Z.zero)
                                                            ],
                                                            [] ) ] );
                                                  Seq (483, []) ],
                                                [] );
                                            Prim (484, I_SUB, [], []);
                                            Prim (485, I_ISNAT, [], []);
                                            Prim
                                              ( 486,
                                                I_IF_NONE,
                                                [ Seq
                                                    ( 487,
                                                      [ Prim
                                                          ( 488,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 489,
                                                                  T_string,
                                                                  [],
                                                                  [] );
                                                              String
                                                                ( 490,
                                                                  "NotEnoughBalance"
                                                                ) ],
                                                            [] );
                                                        Prim
                                                          ( 491,
                                                            I_FAILWITH,
                                                            [],
                                                            [] ) ] );
                                                  Seq (492, []) ],
                                                [] );
                                            Prim
                                              ( 493,
                                                I_DIG,
                                                [Int (494, Z.of_int 2)],
                                                [] );
                                            Prim
                                              ( 495,
                                                I_PUSH,
                                                [ Prim (496, T_nat, [], []);
                                                  Int (497, Z.zero) ],
                                                [] );
                                            Prim
                                              ( 498,
                                                I_DIG,
                                                [Int (499, Z.of_int 2)],
                                                [] );
                                            Prim (500, I_DUP, [], []);
                                            Prim
                                              ( 501,
                                                I_DUG,
                                                [Int (502, Z.of_int 3)],
                                                [] );
                                            Prim (503, I_COMPARE, [], []);
                                            Prim (504, I_EQ, [], []);
                                            Prim
                                              ( 505,
                                                I_IF,
                                                [ Seq
                                                    ( 506,
                                                      [ Prim
                                                          (507, I_SWAP, [], []);
                                                        Prim
                                                          (508, I_DROP, [], []);
                                                        Prim
                                                          ( 509,
                                                            I_NONE,
                                                            [ Prim
                                                                ( 510,
                                                                  T_nat,
                                                                  [],
                                                                  [] ) ],
                                                            [] ) ] );
                                                  Seq
                                                    ( 511,
                                                      [ Prim
                                                          (512, I_SWAP, [], []);
                                                        Prim
                                                          (513, I_SOME, [], [])
                                                      ] ) ],
                                                [] );
                                            Prim
                                              ( 514,
                                                I_DIG,
                                                [Int (515, Z.of_int 3)],
                                                [] );
                                            Prim (516, I_DUP, [], []);
                                            Prim
                                              ( 517,
                                                I_DUG,
                                                [Int (518, Z.of_int 4)],
                                                [] );
                                            Prim (519, I_CAR, [], []);
                                            Prim (520, I_UPDATE, [], []);
                                            Prim
                                              ( 521,
                                                I_DIG,
                                                [Int (522, Z.of_int 2)],
                                                [] );
                                            Prim (523, I_DUP, [], []);
                                            Prim
                                              ( 524,
                                                I_DUG,
                                                [Int (525, Z.of_int 3)],
                                                [] );
                                            Prim (526, I_CDR, [], []);
                                            Prim (527, I_CDR, [], []);
                                            Prim (528, I_SWAP, [], []);
                                            Prim (529, I_DUP, [], []);
                                            Prim
                                              ( 530,
                                                I_DUG,
                                                [Int (531, Z.of_int 2)],
                                                [] );
                                            Prim
                                              ( 532,
                                                I_DIG,
                                                [Int (533, Z.of_int 4)],
                                                [] );
                                            Prim (534, I_DUP, [], []);
                                            Prim
                                              ( 535,
                                                I_DUG,
                                                [Int (536, Z.of_int 5)],
                                                [] );
                                            Prim (537, I_CDR, [], []);
                                            Prim (538, I_CAR, [], []);
                                            Prim (539, I_GET, [], []);
                                            Prim
                                              ( 540,
                                                I_IF_NONE,
                                                [ Seq
                                                    ( 541,
                                                      [ Prim
                                                          ( 542,
                                                            I_PUSH,
                                                            [ Prim
                                                                ( 543,
                                                                  T_nat,
                                                                  [],
                                                                  [] );
                                                              Int (544, Z.zero)
                                                            ],
                                                            [] ) ] );
                                                  Seq (545, []) ],
                                                [] );
                                            Prim (546, I_ADD, [], []);
                                            Prim (547, I_SWAP, [], []);
                                            Prim
                                              ( 548,
                                                I_PUSH,
                                                [ Prim (549, T_nat, [], []);
                                                  Int (550, Z.zero) ],
                                                [] );
                                            Prim
                                              ( 551,
                                                I_DIG,
                                                [Int (552, Z.of_int 2)],
                                                [] );
                                            Prim (553, I_DUP, [], []);
                                            Prim
                                              ( 554,
                                                I_DUG,
                                                [Int (555, Z.of_int 3)],
                                                [] );
                                            Prim (556, I_COMPARE, [], []);
                                            Prim (557, I_EQ, [], []);
                                            Prim
                                              ( 558,
                                                I_IF,
                                                [ Seq
                                                    ( 559,
                                                      [ Prim
                                                          (560, I_SWAP, [], []);
                                                        Prim
                                                          (561, I_DROP, [], []);
                                                        Prim
                                                          ( 562,
                                                            I_NONE,
                                                            [ Prim
                                                                ( 563,
                                                                  T_nat,
                                                                  [],
                                                                  [] ) ],
                                                            [] ) ] );
                                                  Seq
                                                    ( 564,
                                                      [ Prim
                                                          (565, I_SWAP, [], []);
                                                        Prim
                                                          (566, I_SOME, [], [])
                                                      ] ) ],
                                                [] );
                                            Prim
                                              ( 567,
                                                I_DIG,
                                                [Int (568, Z.of_int 3)],
                                                [] );
                                            Prim (569, I_CDR, [], []);
                                            Prim (570, I_CAR, [], []);
                                            Prim (571, I_UPDATE, [], []);
                                            Prim
                                              ( 572,
                                                I_DIG,
                                                [Int (573, Z.of_int 2)],
                                                [] );
                                            Prim (574, I_CDR, [], []);
                                            Prim (575, I_SWAP, [], []);
                                            Prim (576, I_PAIR, [], []);
                                            Prim (577, I_DUP, [], []);
                                            Prim (578, I_CDR, [], []);
                                            Prim (579, I_CDR, [], []);
                                            Prim
                                              ( 580,
                                                I_DIG,
                                                [Int (581, Z.of_int 2)],
                                                [] );
                                            Prim (582, I_PAIR, [], []);
                                            Prim (583, I_SWAP, [], []);
                                            Prim (584, I_CAR, [], []);
                                            Prim (585, I_PAIR, [], []);
                                            Prim
                                              ( 586,
                                                I_NIL,
                                                [ Prim
                                                    (587, T_operation, [], [])
                                                ],
                                                [] );
                                            Prim (588, I_PAIR, [], []) ] ) ],
                                    [] ) ] ) ],
                        [] ) ] ) ],
            [] ) ] )
