(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = CLST

type with_storage = {kind : t; storage : Script_repr.lazy_expr}

module CLST_contract = struct
  let token_metadata_id0 =
    let open Micheline in
    let open Michelson_v1_primitives in
    let elt key value =
      Prim
        ( dummy_location,
          D_Elt,
          [String (dummy_location, key); Bytes (dummy_location, value)],
          [] )
    in
    Prim
      ( dummy_location,
        D_Elt,
        [
          Int (dummy_location, Z.zero);
          Prim
            ( dummy_location,
              D_Pair,
              [
                Int (dummy_location, Z.zero);
                Seq
                  ( dummy_location,
                    [
                      elt "decimals" (Bytes.of_string "6");
                      elt "name" (Bytes.of_string "Staked Tez");
                      elt "symbol" (Bytes.of_string "sTez");
                      elt "tokenKind" (Bytes.of_string "fungible");
                    ] );
              ],
              [] );
        ],
        [] )

  let initial_storage =
    Micheline.(
      Prim
        ( dummy_location,
          Michelson_v1_primitives.D_Pair,
          [
            Seq (dummy_location, []);
            Prim
              ( dummy_location,
                Michelson_v1_primitives.D_Pair,
                [
                  Int (dummy_location, Z.zero);
                  Prim
                    ( dummy_location,
                      Michelson_v1_primitives.D_Pair,
                      [
                        Seq (dummy_location, []);
                        Seq (dummy_location, [token_metadata_id0]);
                      ],
                      [] );
                ],
                [] );
          ],
          [] ))
    |> Micheline.strip_locations |> Script_repr.lazy_expr

  let with_initial_storage = {kind = CLST; storage = initial_storage}
end

let encoding : t Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"CLST"
        unit
        (function CLST -> Some ())
        (fun () -> CLST);
    ]

let with_storage_encoding =
  Data_encoding.(
    conv
      (fun {kind; storage} -> (kind, storage))
      (fun (kind, storage) -> {kind; storage})
      (obj2
         (req "kind" encoding)
         (req "storage" Script_repr.lazy_expr_encoding)))

let rpc_arg =
  RPC_arg.make
    ~descr:"A native contract kind"
    ~name:"kind"
    ~construct:(function CLST -> "CLST")
    ~destruct:(fun _ -> Ok CLST)
    ()

let equal k1 k2 = match (k1, k2) with CLST, CLST -> true
