(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2019 Cryptium Labs <contact@cryptium-labs.com>              *)
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

let manager_script_code : Script_repr.lazy_expr =
  let open Micheline in
  let open Michelson_v1_primitives in
  Script_repr.lazy_expr @@ strip_locations
  @@ Seq
       ( 0,
         [ Prim
             ( 0,
               K_parameter,
               [ Prim
                   ( 0,
                     T_or,
                     [ Prim
                         ( 0,
                           T_lambda,
                           [ Prim (0, T_unit, [], []);
                             Prim
                               (0, T_list, [Prim (0, T_operation, [], [])], [])
                           ],
                           ["%do"] );
                       Prim (0, T_unit, [], ["%default"]) ],
                     [] ) ],
               [] );
           Prim (0, K_storage, [Prim (0, T_key_hash, [], [])], []);
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
                                   Seq
                                     ( 0,
                                       [ Prim
                                           ( 0,
                                             I_DIP,
                                             [ Seq
                                                 (0, [Prim (0, I_DUP, [], [])])
                                             ],
                                             [] );
                                         Prim (0, I_SWAP, [], []) ] );
                                   Prim (0, I_IMPLICIT_ACCOUNT, [], []);
                                   Prim (0, I_ADDRESS, [], []);
                                   Prim (0, I_SENDER, [], []);
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
                                   Prim (0, I_UNIT, [], []);
                                   Prim (0, I_EXEC, [], []);
                                   Prim (0, I_PAIR, [], []) ] );
                             Seq
                               ( 0,
                                 [ Prim (0, I_DROP, [], []);
                                   Prim
                                     ( 0,
                                       I_NIL,
                                       [Prim (0, T_operation, [], [])],
                                       [] );
                                   Prim (0, I_PAIR, [], []) ] ) ],
                           [] ) ] ) ],
               [] ) ] )

(* Find the toplevel expression with a given prim type from list,
   because they can be in arbitrary order. *)
let find_toplevel toplevel exprs =
  let open Micheline in
  let rec iter toplevel = function
    | (Prim (_, prim, _, _) as found) :: _
      when String.equal toplevel (Michelson_v1_primitives.string_of_prim prim)
      ->
        Some found
    | _ :: rest ->
        iter toplevel rest
    | [] ->
        None
  in
  iter (Michelson_v1_primitives.string_of_prim toplevel) exprs

let add_do :
    manager_pkh:Signature.Public_key_hash.t ->
    script_code:Script_repr.lazy_expr ->
    script_storage:Script_repr.lazy_expr ->
    (Script_repr.lazy_expr * Script_repr.lazy_expr) tzresult Lwt.t =
 fun ~manager_pkh ~script_code ~script_storage ->
  let open Micheline in
  let open Michelson_v1_primitives in
  Lwt.return (Script_repr.force_decode script_code)
  >>=? fun (script_code_expr, _gas_cost) ->
  Lwt.return (Script_repr.force_decode script_storage)
  >>|? fun (script_storage_expr, _gas_cost) ->
  let storage_expr = root script_storage_expr in
  match root script_code_expr with
  | Seq (_, toplevel) -> (
    match
      ( find_toplevel K_parameter toplevel,
        find_toplevel K_storage toplevel,
        find_toplevel K_code toplevel )
    with
    | ( Some
          (Prim
            ( _,
              K_parameter,
              [Prim (_, parameter_type, parameter_expr, parameter_annot)],
              prim_param_annot )),
        Some
          (Prim
            ( _,
              K_storage,
              [ Prim
                  (_, code_storage_type, code_storage_expr, code_storage_annot)
              ],
              k_storage_annot )),
        Some (Prim (_, K_code, [code_expr], code_annot)) ) ->
        (* Note that we intentionally don't deal with potential duplicate entrypoints in this migration as there already might be some in contracts that we don't touch. *)
        let migrated_code =
          Seq
            ( 0,
              [ Prim
                  ( 0,
                    K_parameter,
                    [ Prim
                        ( 0,
                          T_or,
                          [ Prim
                              ( 0,
                                T_lambda,
                                [ Prim (0, T_unit, [], []);
                                  Prim
                                    ( 0,
                                      T_list,
                                      [Prim (0, T_operation, [], [])],
                                      [] ) ],
                                ["%do"] );
                            Prim
                              ( 0,
                                parameter_type,
                                parameter_expr,
                                "%default" :: parameter_annot ) ],
                          [] ) ],
                    prim_param_annot );
                Prim
                  ( 0,
                    K_storage,
                    [ Prim
                        ( 0,
                          T_pair,
                          [ Prim (0, T_key_hash, [], []);
                            Prim
                              ( 0,
                                code_storage_type,
                                code_storage_expr,
                                code_storage_annot ) ],
                          [] ) ],
                    k_storage_annot );
                Prim
                  ( 0,
                    K_code,
                    [ Seq
                        ( 0,
                          [ Prim (0, I_DUP, [], []);
                            Prim (0, I_CAR, [], []);
                            Prim
                              ( 0,
                                I_IF_LEFT,
                                [ Seq
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
                                                                  ( 0,
                                                                    I_UNIT,
                                                                    [],
                                                                    [] );
                                                                Prim
                                                                  ( 0,
                                                                    I_FAILWITH,
                                                                    [],
                                                                    [] ) ] ) ]
                                                      ) ],
                                                  [] ) ] );
                                        Seq
                                          ( 0,
                                            [ Prim
                                                ( 0,
                                                  I_DIP,
                                                  [ Seq
                                                      ( 0,
                                                        [ Prim
                                                            (0, I_DUP, [], [])
                                                        ] ) ],
                                                  [] );
                                              Prim (0, I_SWAP, [], []) ] );
                                        Prim (0, I_CDR, [], []);
                                        Prim (0, I_CAR, [], []);
                                        Prim (0, I_IMPLICIT_ACCOUNT, [], []);
                                        Prim (0, I_ADDRESS, [], []);
                                        Prim (0, I_SENDER, [], []);
                                        Seq
                                          ( 0,
                                            [ Prim (0, I_COMPARE, [], []);
                                              Prim (0, I_NEQ, [], []);
                                              Prim
                                                ( 0,
                                                  I_IF,
                                                  [ Seq
                                                      ( 0,
                                                        [ Prim
                                                            ( 0,
                                                              I_SENDER,
                                                              [],
                                                              [] );
                                                          Prim
                                                            ( 0,
                                                              I_PUSH,
                                                              [ Prim
                                                                  ( 0,
                                                                    T_string,
                                                                    [],
                                                                    [] );
                                                                String
                                                                  ( 0,
                                                                    "Only the \
                                                                     owner \
                                                                     can \
                                                                     operate."
                                                                  ) ],
                                                              [] );
                                                          Prim
                                                            (0, I_PAIR, [], []);
                                                          Prim
                                                            ( 0,
                                                              I_FAILWITH,
                                                              [],
                                                              [] ) ] );
                                                    Seq
                                                      ( 0,
                                                        [ Prim
                                                            (0, I_UNIT, [], []);
                                                          Prim
                                                            (0, I_EXEC, [], []);
                                                          Prim
                                                            ( 0,
                                                              I_DIP,
                                                              [ Seq
                                                                  ( 0,
                                                                    [ Prim
                                                                        ( 0,
                                                                          I_CDR,
                                                                          [],
                                                                          [] )
                                                                    ] ) ],
                                                              [] );
                                                          Prim
                                                            (0, I_PAIR, [], [])
                                                        ] ) ],
                                                  [] ) ] ) ] );
                                  Seq
                                    ( 0,
                                      [ Prim
                                          ( 0,
                                            I_DIP,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_CDR, [], []);
                                                    Prim (0, I_DUP, [], []);
                                                    Prim (0, I_CDR, [], []) ]
                                                ) ],
                                            [] );
                                        Prim (0, I_PAIR, [], []);
                                        code_expr;
                                        Prim (0, I_SWAP, [], []);
                                        Prim (0, I_CAR, [], []);
                                        Prim (0, I_SWAP, [], []);
                                        Seq
                                          ( 0,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_DUP, [], []);
                                                    Prim (0, I_CAR, [], []);
                                                    Prim
                                                      ( 0,
                                                        I_DIP,
                                                        [ Seq
                                                            ( 0,
                                                              [ Prim
                                                                  ( 0,
                                                                    I_CDR,
                                                                    [],
                                                                    [] ) ] ) ],
                                                        [] ) ] ) ] );
                                        Prim
                                          ( 0,
                                            I_DIP,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_SWAP, [], []);
                                                    Prim (0, I_PAIR, [], []) ]
                                                ) ],
                                            [] );
                                        Prim (0, I_PAIR, [], []) ] ) ],
                                [] ) ] ) ],
                    code_annot ) ] )
        in
        let migrated_storage =
          Prim
            ( 0,
              D_Pair,
              [ (* Instead of
                   `String (0, Signature.Public_key_hash.to_b58check manager_pkh)`
                   the storage is written as unparsed with [Optimized] *)
                Bytes
                  ( 0,
                    Data_encoding.Binary.to_bytes_exn
                      Signature.Public_key_hash.encoding
                      manager_pkh );
                storage_expr ],
              [] )
        in
        ( Script_repr.lazy_expr @@ strip_locations migrated_code,
          Script_repr.lazy_expr @@ strip_locations migrated_storage )
    | _ ->
        (script_code, script_storage) )
  | _ ->
      (script_code, script_storage)

let add_set_delegate :
    manager_pkh:Signature.Public_key_hash.t ->
    script_code:Script_repr.lazy_expr ->
    script_storage:Script_repr.lazy_expr ->
    (Script_repr.lazy_expr * Script_repr.lazy_expr) tzresult Lwt.t =
 fun ~manager_pkh ~script_code ~script_storage ->
  let open Micheline in
  let open Michelson_v1_primitives in
  Lwt.return (Script_repr.force_decode script_code)
  >>=? fun (script_code_expr, _gas_cost) ->
  Lwt.return (Script_repr.force_decode script_storage)
  >>|? fun (script_storage_expr, _gas_cost) ->
  let storage_expr = root script_storage_expr in
  match root script_code_expr with
  | Seq (_, toplevel) -> (
    match
      ( find_toplevel K_parameter toplevel,
        find_toplevel K_storage toplevel,
        find_toplevel K_code toplevel )
    with
    | ( Some
          (Prim
            ( _,
              K_parameter,
              [Prim (_, parameter_type, parameter_expr, parameter_annot)],
              prim_param_annot )),
        Some
          (Prim
            ( _,
              K_storage,
              [ Prim
                  (_, code_storage_type, code_storage_expr, code_storage_annot)
              ],
              k_storage_annot )),
        Some (Prim (_, K_code, [code_expr], code_annot)) ) ->
        (* Note that we intentionally don't deal with potential duplicate entrypoints in this migration as there already might be some in contracts that we don't touch. *)
        let migrated_code =
          Seq
            ( 0,
              [ Prim
                  ( 0,
                    K_parameter,
                    [ Prim
                        ( 0,
                          T_or,
                          [ Prim
                              ( 0,
                                T_or,
                                [ Prim (0, T_key_hash, [], ["%set_delegate"]);
                                  Prim (0, T_unit, [], ["%remove_delegate"]) ],
                                [] );
                            Prim
                              ( 0,
                                parameter_type,
                                parameter_expr,
                                "%default" :: parameter_annot ) ],
                          [] ) ],
                    prim_param_annot );
                Prim
                  ( 0,
                    K_storage,
                    [ Prim
                        ( 0,
                          T_pair,
                          [ Prim (0, T_key_hash, [], []);
                            Prim
                              ( 0,
                                code_storage_type,
                                code_storage_expr,
                                code_storage_annot ) ],
                          [] ) ],
                    k_storage_annot );
                Prim
                  ( 0,
                    K_code,
                    [ Seq
                        ( 0,
                          [ Prim (0, I_DUP, [], []);
                            Prim (0, I_CAR, [], []);
                            Prim
                              ( 0,
                                I_IF_LEFT,
                                [ Seq
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
                                                                  ( 0,
                                                                    I_UNIT,
                                                                    [],
                                                                    [] );
                                                                Prim
                                                                  ( 0,
                                                                    I_FAILWITH,
                                                                    [],
                                                                    [] ) ] ) ]
                                                      ) ],
                                                  [] ) ] );
                                        Seq
                                          ( 0,
                                            [ Prim
                                                ( 0,
                                                  I_DIP,
                                                  [ Seq
                                                      ( 0,
                                                        [ Prim
                                                            (0, I_DUP, [], [])
                                                        ] ) ],
                                                  [] );
                                              Prim (0, I_SWAP, [], []) ] );
                                        Prim (0, I_CDR, [], []);
                                        Prim (0, I_CAR, [], []);
                                        Prim (0, I_IMPLICIT_ACCOUNT, [], []);
                                        Prim (0, I_ADDRESS, [], []);
                                        Prim (0, I_SENDER, [], []);
                                        Seq
                                          ( 0,
                                            [ Prim (0, I_COMPARE, [], []);
                                              Prim (0, I_NEQ, [], []);
                                              Prim
                                                ( 0,
                                                  I_IF,
                                                  [ Seq
                                                      ( 0,
                                                        [ Prim
                                                            ( 0,
                                                              I_SENDER,
                                                              [],
                                                              [] );
                                                          Prim
                                                            ( 0,
                                                              I_PUSH,
                                                              [ Prim
                                                                  ( 0,
                                                                    T_string,
                                                                    [],
                                                                    [] );
                                                                String
                                                                  ( 0,
                                                                    "Only the \
                                                                     owner \
                                                                     can \
                                                                     operate."
                                                                  ) ],
                                                              [] );
                                                          Prim
                                                            (0, I_PAIR, [], []);
                                                          Prim
                                                            ( 0,
                                                              I_FAILWITH,
                                                              [],
                                                              [] ) ] );
                                                    Seq
                                                      ( 0,
                                                        [ Prim
                                                            ( 0,
                                                              I_DIP,
                                                              [ Seq
                                                                  ( 0,
                                                                    [ Prim
                                                                        ( 0,
                                                                          I_CDR,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_NIL,
                                                                          [ Prim
                                                                              ( 
                                                                              0,
                                                                               T_operation,
                                                                               [],
                                                                               []
                                                                              )
                                                                          ],
                                                                          [] )
                                                                    ] ) ],
                                                              [] );
                                                          Prim
                                                            ( 0,
                                                              I_IF_LEFT,
                                                              [ Seq
                                                                  ( 0,
                                                                    [ Prim
                                                                        ( 0,
                                                                          I_SOME,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_SET_DELEGATE,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_CONS,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_PAIR,
                                                                          [],
                                                                          [] )
                                                                    ] );
                                                                Seq
                                                                  ( 0,
                                                                    [ Prim
                                                                        ( 0,
                                                                          I_DROP,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_NONE,
                                                                          [ Prim
                                                                              ( 
                                                                              0,
                                                                               T_key_hash,
                                                                               [],
                                                                               []
                                                                              )
                                                                          ],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_SET_DELEGATE,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_CONS,
                                                                          [],
                                                                          [] );
                                                                      Prim
                                                                        ( 0,
                                                                          I_PAIR,
                                                                          [],
                                                                          [] )
                                                                    ] ) ],
                                                              [] ) ] ) ],
                                                  [] ) ] ) ] );
                                  Seq
                                    ( 0,
                                      [ Prim
                                          ( 0,
                                            I_DIP,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_CDR, [], []);
                                                    Prim (0, I_DUP, [], []);
                                                    Prim (0, I_CDR, [], []) ]
                                                ) ],
                                            [] );
                                        Prim (0, I_PAIR, [], []);
                                        code_expr;
                                        Prim (0, I_SWAP, [], []);
                                        Prim (0, I_CAR, [], []);
                                        Prim (0, I_SWAP, [], []);
                                        Seq
                                          ( 0,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_DUP, [], []);
                                                    Prim (0, I_CAR, [], []);
                                                    Prim
                                                      ( 0,
                                                        I_DIP,
                                                        [ Seq
                                                            ( 0,
                                                              [ Prim
                                                                  ( 0,
                                                                    I_CDR,
                                                                    [],
                                                                    [] ) ] ) ],
                                                        [] ) ] ) ] );
                                        Prim
                                          ( 0,
                                            I_DIP,
                                            [ Seq
                                                ( 0,
                                                  [ Prim (0, I_SWAP, [], []);
                                                    Prim (0, I_PAIR, [], []) ]
                                                ) ],
                                            [] );
                                        Prim (0, I_PAIR, [], []) ] ) ],
                                [] ) ] ) ],
                    code_annot ) ] )
        in
        let migrated_storage =
          Prim
            ( 0,
              D_Pair,
              [ (* Instead of
                   `String (0, Signature.Public_key_hash.to_b58check manager_pkh)`
                   the storage is written as unparsed with [Optimized] *)
                Bytes
                  ( 0,
                    Data_encoding.Binary.to_bytes_exn
                      Signature.Public_key_hash.encoding
                      manager_pkh );
                storage_expr ],
              [] )
        in
        ( Script_repr.lazy_expr @@ strip_locations migrated_code,
          Script_repr.lazy_expr @@ strip_locations migrated_storage )
    | _ ->
        (script_code, script_storage) )
  | _ ->
      (script_code, script_storage)

let has_default_entrypoint expr =
  let open Micheline in
  let open Michelson_v1_primitives in
  match Script_repr.force_decode expr with
  | Error _ ->
      false
  | Ok (expr, _) -> (
    match root expr with
    | Seq (_, toplevel) -> (
      match find_toplevel K_parameter toplevel with
      | Some (Prim (_, K_parameter, [_], ["%default"])) ->
          false
      | Some (Prim (_, K_parameter, [parameter_expr], _)) ->
          let rec has_default = function
            | Prim (_, T_or, [l; r], annots) ->
                List.exists (String.equal "%default") annots
                || has_default l || has_default r
            | Prim (_, _, _, annots) ->
                List.exists (String.equal "%default") annots
            | _ ->
                false
          in
          has_default parameter_expr
      | Some _ | None ->
          false )
    | _ ->
        false )

let add_root_entrypoint :
    script_code:Script_repr.lazy_expr -> Script_repr.lazy_expr tzresult Lwt.t =
 fun ~script_code ->
  let open Micheline in
  let open Michelson_v1_primitives in
  Lwt.return (Script_repr.force_decode script_code)
  >>|? fun (script_code_expr, _gas_cost) ->
  match root script_code_expr with
  | Seq (_, toplevel) ->
      let migrated_code =
        Seq
          ( 0,
            List.map
              (function
                | Prim (_, K_parameter, [parameter_expr], _) ->
                    Prim (0, K_parameter, [parameter_expr], ["%root"])
                | Prim (_, K_code, exprs, annots) ->
                    let rec rewrite_self = function
                      | ( Int _
                        | String _
                        | Bytes _
                        | Prim (_, I_CREATE_CONTRACT, _, _) ) as leaf ->
                          leaf
                      | Prim (_, I_SELF, [], annots) ->
                          Prim (0, I_SELF, [], "%root" :: annots)
                      | Prim (_, name, args, annots) ->
                          Prim (0, name, List.map rewrite_self args, annots)
                      | Seq (_, args) ->
                          Seq (0, List.map rewrite_self args)
                    in
                    Prim (0, K_code, List.map rewrite_self exprs, annots)
                | other ->
                    other)
              toplevel )
      in
      Script_repr.lazy_expr @@ strip_locations migrated_code
  | _ ->
      script_code
