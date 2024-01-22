(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Micheline
open Michelson_v1_primitives

let create_context () =
  let open Lwt_result_syntax in
  let*? accounts = Account.generate_accounts 2 in
  Block.alpha_context (Account.make_bootstrap_accounts accounts)

let expr_to_hash expr =
  let open Result_syntax in
  let lexpr = Script_repr.lazy_expr expr in
  let+ b = Script_repr.force_bytes lexpr in
  Script_expr_hash.hash_bytes [b]

let assert_expr_equal loc =
  Assert.equal
    ~loc
    ( = )
    "Michelson Expressions Not Equal"
    Michelson_v1_printer.print_expr

let assert_proto_error_id loc id result =
  let test err =
    (Error_monad.find_info_of_error err).id
    = "proto." ^ Protocol.name ^ "." ^ id
  in
  Assert.error ~loc result test

let assert_ok_lwt x =
  match Lwt_main.run x with
  | Ok x -> x
  | Error _ -> raise @@ Failure "Called assert_ok_lwt on Error"

let assert_ok = function
  | Ok x -> x
  | Error _ -> raise @@ Failure "Called assert_ok on Error"

(** Filters out values that would cause [register] *)
let assume_expr_not_too_large expr =
  let node = root expr in
  QCheck2.assume @@ not
  @@ Global_constants_storage.Internal_for_tests.node_too_large node

module Generators = struct
  let context_gen () = QCheck2.Gen.return (create_context () |> assert_ok_lwt)

  let prims =
    [
      K_parameter;
      K_storage;
      K_code;
      D_False;
      D_Elt;
      D_Left;
      D_None;
      D_Pair;
      D_Right;
      D_Some;
      D_True;
      D_Unit;
      I_PACK;
      I_UNPACK;
      I_BLAKE2B;
      I_SHA256;
      I_SHA512;
      I_ABS;
      I_ADD;
      I_AMOUNT;
      I_AND;
      I_BALANCE;
      I_CAR;
      I_CDR;
      I_CHAIN_ID;
      I_CHECK_SIGNATURE;
      I_COMPARE;
      I_CONCAT;
      I_CONS;
      I_CREATE_ACCOUNT;
      I_CREATE_CONTRACT;
      I_IMPLICIT_ACCOUNT;
      I_DIP;
      I_DROP;
      I_DUP;
      I_EDIV;
      I_EMPTY_BIG_MAP;
      I_EMPTY_MAP;
      I_EMPTY_SET;
      I_EQ;
      I_EXEC;
      I_APPLY;
      I_FAILWITH;
      I_GE;
      I_GET;
      I_GET_AND_UPDATE;
      I_GT;
      I_HASH_KEY;
      I_IF;
      I_IF_CONS;
      I_IF_LEFT;
      I_IF_NONE;
      I_INT;
      I_LAMBDA;
      I_LE;
      I_LEFT;
      I_LEVEL;
      I_LOOP;
      I_LSL;
      I_LSR;
      I_LT;
      I_MAP;
      I_MEM;
      I_MUL;
      I_NEG;
      I_NEQ;
      I_NIL;
      I_NONE;
      I_NOT;
      I_NOW;
      I_OR;
      I_PAIR;
      I_UNPAIR;
      I_PUSH;
      I_RIGHT;
      I_SIZE;
      I_SOME;
      I_SOURCE;
      I_SENDER;
      I_SELF;
      I_SELF_ADDRESS;
      I_SLICE;
      I_STEPS_TO_QUOTA;
      I_SUB;
      I_SWAP;
      I_TRANSFER_TOKENS;
      I_SET_DELEGATE;
      I_UNIT;
      I_UPDATE;
      I_XOR;
      I_ITER;
      I_LOOP_LEFT;
      I_ADDRESS;
      I_CONTRACT;
      I_ISNAT;
      I_CAST;
      I_RENAME;
      I_SAPLING_EMPTY_STATE;
      I_SAPLING_VERIFY_UPDATE;
      I_DIG;
      I_DUG;
      I_NEVER;
      I_VOTING_POWER;
      I_TOTAL_VOTING_POWER;
      I_KECCAK;
      I_SHA3;
      I_PAIRING_CHECK;
      I_TICKET;
      I_READ_TICKET;
      I_SPLIT_TICKET;
      I_JOIN_TICKETS;
      T_bool;
      T_contract;
      T_int;
      T_key;
      T_key_hash;
      T_lambda;
      T_list;
      T_map;
      T_big_map;
      T_nat;
      T_option;
      T_or;
      T_pair;
      T_set;
      T_signature;
      T_string;
      T_bytes;
      T_mutez;
      T_timestamp;
      T_unit;
      T_operation;
      T_address;
      T_sapling_transaction_deprecated;
      T_sapling_state;
      T_chain_id;
      T_never;
      T_bls12_381_g1;
      T_bls12_381_g2;
      T_bls12_381_fr;
      T_ticket;
      H_constant;
    ]

  let prim_gen = QCheck2.Gen.oneofl prims

  let prims_without_constants_gen =
    QCheck2.Gen.oneofl (List.filter (fun x -> x != H_constant) prims)

  let z_gen = QCheck2.Gen.map Z.of_int QCheck2.Gen.int

  let micheline_node_gen l_gen p_gen annot_gen :
      ('l, 'p) Micheline.node QCheck2.Gen.t =
    let open Micheline in
    let open QCheck2.Gen in
    fix
      (fun self () ->
        frequency
          [
            (3, map (fun (l, x) -> Int (l, x)) (pair l_gen z_gen));
            (3, map (fun (l, x) -> String (l, x)) (pair l_gen string));
            ( 3,
              map
                (fun (l, x) -> Bytes (l, Bytes.of_string x))
                (pair l_gen string) );
            ( 1,
              map
                (fun (l, p, args, annot) -> Prim (l, p, args, annot))
                (quad
                   l_gen
                   p_gen
                   (list_size (int_bound 10) (self ()))
                   annot_gen) );
            ( 1,
              map
                (fun (l, args) -> Seq (l, args))
                (pair l_gen (list_size (int_bound 10) (self ()))) );
          ])
      ()

  let rec replace_with_constant :
      Script.node -> Script.location -> Script.node * Script.node option =
   fun node loc ->
    let open Michelson_v1_primitives in
    let open Micheline in
    let rec loop : Script.node list -> Script.node list * Script.node option =
      function
      | [] -> ([], None)
      | hd :: tl -> (
          match replace_with_constant hd loc with
          | node, Some x -> (node :: tl, Some x)
          | _, None ->
              let l, x = loop tl in
              (hd :: l, x))
    in
    match node with
    | (Int (l, _) | String (l, _) | Bytes (l, _)) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else (node, None)
    | Prim (l, prim, args, annot) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else
          let result, x = loop args in
          (Prim (l, prim, result, annot), x)
    | Seq (l, args) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else
          let result, x = loop args in
          (Seq (l, result), x)

  let micheline_gen p_gen annot_gen =
    QCheck2.Gen.map
      Micheline.strip_locations
      (micheline_node_gen (QCheck2.Gen.return (-1)) p_gen annot_gen)

  let canonical_without_constant_gen () =
    QCheck2.Gen.map
      strip_locations
      (micheline_node_gen
         (QCheck2.Gen.return (-1))
         prims_without_constants_gen
         (QCheck2.Gen.return []))

  let canonical_with_constant_gen () =
    let open QCheck2.Gen in
    canonical_without_constant_gen () >>= fun expr ->
    let size = Script_repr.micheline_nodes (root expr) in
    0 -- (size - 1) >|= fun loc ->
    match replace_with_constant (root expr) loc with
    | _, None -> assert false
    | node, Some replaced_node ->
        (expr, strip_locations node, strip_locations replaced_node)
end
