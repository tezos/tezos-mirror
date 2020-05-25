(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Micheline
open Michelson_v1_primitives

let loc = 0

let seq l = Seq (loc, l)

let pair a b = Prim (loc, D_Pair, [a; b], [])

let int i = Int (loc, i)

let bytes s = Bytes (loc, s)

let storage ~threshold ~owner_keys =
  let keys =
    List.map
      (fun key ->
        bytes
        @@ Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding key)
      owner_keys
  in
  let counter = int Z.zero in
  let threshold = int (Z.of_int threshold) in
  strip_locations @@ pair counter (pair threshold (seq keys))

let code =
  strip_locations
  (* Micheline converted using this client command:
     [tezos-client convert script ./tests_python/contracts/baker.tz from michelson to ocaml --zero-loc --skip-typecheck] *)
  @@ Seq
       ( 0,
         [ Prim
             ( 0,
               K_parameter,
               [ Prim
                   ( 0,
                     T_or,
                     [ Prim (0, T_unit, [], ["%default"]);
                       Prim
                         ( 0,
                           T_pair,
                           [ Prim
                               ( 0,
                                 T_pair,
                                 [ Prim (0, T_nat, [], ["%counter"]);
                                   Prim
                                     ( 0,
                                       T_or,
                                       [ Prim
                                           ( 0,
                                             T_lambda,
                                             [ Prim (0, T_unit, [], []);
                                               Prim
                                                 ( 0,
                                                   T_pair,
                                                   [ Prim
                                                       ( 0,
                                                         T_list,
                                                         [ Prim
                                                             ( 0,
                                                               T_operation,
                                                               [],
                                                               [] ) ],
                                                         [] );
                                                     Prim
                                                       ( 0,
                                                         T_list,
                                                         [ Prim
                                                             ( 0,
                                                               T_baker_operation,
                                                               [],
                                                               [] ) ],
                                                         [] ) ],
                                                   [] ) ],
                                             ["%operation"] );
                                         Prim
                                           ( 0,
                                             T_pair,
                                             [ Prim
                                                 (0, T_nat, [], ["%threshold"]);
                                               Prim
                                                 ( 0,
                                                   T_list,
                                                   [Prim (0, T_key, [], [])],
                                                   ["%keys"] ) ],
                                             ["%change_keys"] ) ],
                                       [":action"] ) ],
                                 [":payload"] );
                             Prim
                               ( 0,
                                 T_list,
                                 [ Prim
                                     ( 0,
                                       T_option,
                                       [Prim (0, T_signature, [], [])],
                                       [] ) ],
                                 ["%sigs"] ) ],
                           ["%main"] ) ],
                     [] ) ],
               ["%root"] );
           Prim
             ( 0,
               K_storage,
               [ Prim
                   ( 0,
                     T_pair,
                     [ Prim (0, T_nat, [], ["%stored_counter"]);
                       Prim
                         ( 0,
                           T_pair,
                           [ Prim (0, T_nat, [], ["%threshold"]);
                             Prim
                               (0, T_list, [Prim (0, T_key, [], [])], ["%keys"])
                           ],
                           [] ) ],
                     [] ) ],
               [] );
           Prim
             ( 0,
               K_code,
               [ Seq
                   ( 0,
                     [ Seq
                         ( 0,
                           [ Seq
                               ( 0,
                                 [ Prim (0, I_DUP, [], []);
                                   Prim (0, I_CAR, [], []);
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [Seq (0, [Prim (0, I_CDR, [], [])])],
                                       [] ) ] ) ] );
                       Prim
                         ( 0,
                           I_IF_LEFT,
                           [ Seq
                               ( 0,
                                 [ Prim (0, I_DROP, [], []);
                                   Prim
                                     ( 0,
                                       I_NIL,
                                       [Prim (0, T_baker_operation, [], [])],
                                       [] );
                                   Prim
                                     ( 0,
                                       I_NIL,
                                       [Prim (0, T_operation, [], [])],
                                       [] );
                                   Prim (0, I_PAIR, [], []);
                                   Prim (0, I_PAIR, [], []) ] );
                             Seq
                               ( 0,
                                 [ Prim
                                     ( 0,
                                       I_PUSH,
                                       [ Prim (0, T_mutez, [], []);
                                         Int (0, Z.zero) ],
                                       [] );
                                   Prim (0, I_AMOUNT, [], []);
                                   Seq
                                     ( 0,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_COMPARE, [], []);
                                               Prim (0, I_EQ, [], []) ] );
                                         Prim
                                           ( 0,
                                             I_IF,
                                             [ Seq (0, []);
                                               Seq
                                                 ( 0,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_UNIT, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_FAILWITH,
                                                               [],
                                                               [] ) ] ) ] ) ],
                                             [] ) ] );
                                   Prim (0, I_SWAP, [], []);
                                   Prim (0, I_DUP, [], []);
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [Seq (0, [Prim (0, I_SWAP, [], [])])],
                                       [] );
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [ Seq
                                           ( 0,
                                             [ Seq
                                                 ( 0,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_DUP, [], []);
                                                           Prim
                                                             (0, I_CAR, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_DIP,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Prim
                                                                         ( 0,
                                                                           I_CDR,
                                                                           [],
                                                                           []
                                                                         ) ] )
                                                               ],
                                                               [] ) ] ) ] );
                                               Prim (0, I_DUP, [], []);
                                               Prim (0, I_SELF_ADDRESS, [], []);
                                               Prim (0, I_CHAIN_ID, [], []);
                                               Prim (0, I_PAIR, [], []);
                                               Prim (0, I_PAIR, [], []);
                                               Prim (0, I_PACK, [], []);
                                               Prim
                                                 ( 0,
                                                   I_DIP,
                                                   [ Seq
                                                       ( 0,
                                                         [ Seq
                                                             ( 0,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Prim
                                                                         ( 0,
                                                                           I_DUP,
                                                                           [],
                                                                           []
                                                                         );
                                                                       Prim
                                                                         ( 0,
                                                                           I_CAR,
                                                                           [],
                                                                           [ "@counter"
                                                                           ] );
                                                                       Prim
                                                                         ( 0,
                                                                           I_DIP,
                                                                           [ Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_CDR,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                           ],
                                                                           []
                                                                         ) ] )
                                                               ] );
                                                           Prim
                                                             ( 0,
                                                               I_DIP,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Prim
                                                                         ( 0,
                                                                           I_SWAP,
                                                                           [],
                                                                           []
                                                                         ) ] )
                                                               ],
                                                               [] ) ] ) ],
                                                   [] );
                                               Prim (0, I_SWAP, [], []) ] ) ],
                                       [] );
                                   Seq
                                     ( 0,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_DUP, [], []);
                                               Prim
                                                 ( 0,
                                                   I_CAR,
                                                   [],
                                                   ["@stored_counter"] );
                                               Prim
                                                 ( 0,
                                                   I_DIP,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_CDR, [], [])
                                                         ] ) ],
                                                   [] ) ] ) ] );
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [Seq (0, [Prim (0, I_SWAP, [], [])])],
                                       [] );
                                   Seq
                                     ( 0,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_COMPARE, [], []);
                                               Prim (0, I_EQ, [], []) ] );
                                         Prim
                                           ( 0,
                                             I_IF,
                                             [ Seq (0, []);
                                               Seq
                                                 ( 0,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_UNIT, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_FAILWITH,
                                                               [],
                                                               [] ) ] ) ] ) ],
                                             [] ) ] );
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [Seq (0, [Prim (0, I_SWAP, [], [])])],
                                       [] );
                                   Seq
                                     ( 0,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_DUP, [], []);
                                               Prim
                                                 (0, I_CAR, [], ["@threshold"]);
                                               Prim
                                                 ( 0,
                                                   I_DIP,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             ( 0,
                                                               I_CDR,
                                                               [],
                                                               ["@keys"] ) ] )
                                                   ],
                                                   [] ) ] ) ] );
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [ Seq
                                           ( 0,
                                             [ Prim
                                                 ( 0,
                                                   I_PUSH,
                                                   [ Prim (0, T_nat, [], []);
                                                     Int (0, Z.zero) ],
                                                   ["@valid"] );
                                               Prim (0, I_SWAP, [], []);
                                               Prim
                                                 ( 0,
                                                   I_ITER,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             ( 0,
                                                               I_DIP,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Prim
                                                                         ( 0,
                                                                           I_SWAP,
                                                                           [],
                                                                           []
                                                                         ) ] )
                                                               ],
                                                               [] );
                                                           Prim
                                                             (0, I_SWAP, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_IF_CONS,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Seq
                                                                         ( 0,
                                                                           [ Prim
                                                                               ( 
                                                                               0,
                                                                               I_IF_NONE,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_SWAP,
                                                                               [],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DROP,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               );
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_SWAP,
                                                                               [],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIP,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_SWAP,
                                                                               [],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIP,
                                                                               [ 
                                                                               Int
                                                                               ( 
                                                                               0,
                                                                               Z
                                                                               .of_int
                                                                               2
                                                                               );
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIP,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DUP,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_SWAP,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               );
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIP,
                                                                               [ 
                                                                               Int
                                                                               ( 
                                                                               0,
                                                                               Z
                                                                               .of_int
                                                                               2
                                                                               );
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DUP,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIG,
                                                                               [ 
                                                                               Int
                                                                               ( 
                                                                               0,
                                                                               Z
                                                                               .of_int
                                                                               3
                                                                               )
                                                                               ],
                                                                               []
                                                                               )
                                                                               ]
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DIP,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_CHECK_SIGNATURE,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_SWAP,
                                                                               [],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_IF,
                                                                               [ 
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_DROP,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               );
                                                                               Seq
                                                                               ( 
                                                                               0,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_FAILWITH,
                                                                               [],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               )
                                                                               ]
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_PUSH,
                                                                               [ 
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               T_nat,
                                                                               [],
                                                                               []
                                                                               );
                                                                               Int
                                                                               ( 
                                                                               0,
                                                                               Z
                                                                               .one
                                                                               )
                                                                               ],
                                                                               []
                                                                               );
                                                                               Prim
                                                                               ( 
                                                                               0,
                                                                               I_ADD,
                                                                               [],
                                                                               [ 
                                                                               "@valid"
                                                                               ]
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ],
                                                                               []
                                                                               )
                                                                           ] )
                                                                     ] );
                                                                 Seq
                                                                   ( 0,
                                                                     [ Seq
                                                                         ( 0,
                                                                           [ Prim
                                                                               ( 
                                                                               0,
                                                                               I_UNIT,
                                                                               [],
                                                                               []
                                                                               );
                                                                             Prim
                                                                               ( 
                                                                               0,
                                                                               I_FAILWITH,
                                                                               [],
                                                                               []
                                                                               )
                                                                           ] )
                                                                     ] ) ],
                                                               [] );
                                                           Prim
                                                             (0, I_SWAP, [], [])
                                                         ] ) ],
                                                   [] ) ] ) ],
                                       [] );
                                   Seq
                                     ( 0,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_COMPARE, [], []);
                                               Prim (0, I_LE, [], []) ] );
                                         Prim
                                           ( 0,
                                             I_IF,
                                             [ Seq (0, []);
                                               Seq
                                                 ( 0,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_UNIT, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_FAILWITH,
                                                               [],
                                                               [] ) ] ) ] ) ],
                                             [] ) ] );
                                   Prim
                                     ( 0,
                                       I_IF_CONS,
                                       [ Seq
                                           ( 0,
                                             [ Seq
                                                 ( 0,
                                                   [ Prim (0, I_UNIT, [], []);
                                                     Prim
                                                       (0, I_FAILWITH, [], [])
                                                   ] ) ] );
                                         Seq (0, []) ],
                                       [] );
                                   Prim (0, I_DROP, [], []);
                                   Prim
                                     ( 0,
                                       I_DIP,
                                       [ Seq
                                           ( 0,
                                             [ Seq
                                                 ( 0,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_DUP, [], []);
                                                           Prim
                                                             (0, I_CAR, [], []);
                                                           Prim
                                                             ( 0,
                                                               I_DIP,
                                                               [ Seq
                                                                   ( 0,
                                                                     [ Prim
                                                                         ( 0,
                                                                           I_CDR,
                                                                           [],
                                                                           []
                                                                         ) ] )
                                                               ],
                                                               [] ) ] ) ] );
                                               Prim
                                                 ( 0,
                                                   I_PUSH,
                                                   [ Prim (0, T_nat, [], []);
                                                     Int (0, Z.one) ],
                                                   [] );
                                               Prim
                                                 ( 0,
                                                   I_ADD,
                                                   [],
                                                   ["@new_counter"] );
                                               Prim (0, I_PAIR, [], []) ] ) ],
                                       [] );
                                   Prim
                                     ( 0,
                                       I_IF_LEFT,
                                       [ Seq
                                           ( 0,
                                             [ Prim (0, I_UNIT, [], []);
                                               Prim (0, I_EXEC, [], []) ] );
                                         Seq
                                           ( 0,
                                             [ Prim
                                                 ( 0,
                                                   I_DIP,
                                                   [ Seq
                                                       ( 0,
                                                         [ Prim
                                                             (0, I_CAR, [], [])
                                                         ] ) ],
                                                   [] );
                                               Prim (0, I_SWAP, [], []);
                                               Prim (0, I_PAIR, [], []);
                                               Prim
                                                 ( 0,
                                                   I_NIL,
                                                   [ Prim
                                                       ( 0,
                                                         T_baker_operation,
                                                         [],
                                                         [] ) ],
                                                   [] );
                                               Prim
                                                 ( 0,
                                                   I_NIL,
                                                   [ Prim
                                                       (0, T_operation, [], [])
                                                   ],
                                                   [] );
                                               Prim (0, I_PAIR, [], []) ] ) ],
                                       [] );
                                   Prim (0, I_PAIR, [], []) ] ) ],
                           [] ) ] ) ],
               [] ) ] )
