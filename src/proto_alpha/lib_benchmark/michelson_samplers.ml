(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* ------------------------------------------------------------------------- *)
(* Helpers. *)

let comparable_downcast = Script_ir_translator.ty_of_comparable_ty

(* ------------------------------------------------------------------------- *)
(* Type names. *)

(* We only want to generated inhabited types, hence Never is not included. *)

type type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `TKey
  | `TTimestamp
  | `TAddress
  | `TBool
  | `TPair
  | `TUnion
  | `TLambda
  | `TOption
  | `TList
  | `TSet
  | `TMap
  | `TBig_map
  | `TContract
  | `TSapling_transaction
  | `TSapling_state
  | `TOperation
  | `TChain_id
  | `TBls12_381_g1
  | `TBls12_381_g2
  | `TBls12_381_fr
  | `TTicket ]

let all_type_names : type_name array =
  [|
    `TUnit;
    `TInt;
    `TNat;
    `TSignature;
    `TString;
    `TBytes;
    `TMutez;
    `TKey_hash;
    `TKey;
    `TTimestamp;
    `TAddress;
    `TBool;
    `TPair;
    `TUnion;
    `TLambda;
    `TOption;
    `TList;
    `TSet;
    `TMap;
    `TBig_map;
    `TContract;
    `TSapling_transaction;
    `TSapling_state;
    `TOperation;
    `TChain_id;
    `TBls12_381_g1;
    `TBls12_381_g2;
    `TBls12_381_fr;
    `TTicket;
  |]

type atomic_type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `TKey
  | `TTimestamp
  | `TAddress
  | `TBool
  | `TSapling_transaction
  | `TSapling_state
  | `TChain_id
  | `TBls12_381_g1
  | `TBls12_381_g2
  | `TBls12_381_fr ]

type non_atomic_type_name =
  [ `TPair
  | `TUnion
  | `TLambda
  | `TOption
  | `TList
  | `TSet
  | `TMap
  | `TBig_map
  | `TContract
  | `TTicket ]

(* Ensure inclusion of atomic_type_name in type_name *)
let (_ : atomic_type_name -> type_name) = fun x -> (x :> type_name)

(* Ensure inclusion of non_atomic_type_name in type_name *)
let (_ : non_atomic_type_name -> type_name) = fun x -> (x :> type_name)

let all_atomic_type_names : atomic_type_name array =
  [|
    `TUnit;
    `TInt;
    `TNat;
    `TSignature;
    `TString;
    `TBytes;
    `TMutez;
    `TKey_hash;
    `TKey;
    `TTimestamp;
    `TAddress;
    `TBool;
    `TSapling_transaction;
    `TSapling_state;
    `TChain_id;
    `TBls12_381_g1;
    `TBls12_381_g2;
    `TBls12_381_fr;
  |]

let all_non_atomic_type_names : non_atomic_type_name array =
  [|
    `TPair;
    `TUnion;
    `TLambda;
    `TOption;
    `TList;
    `TSet;
    `TMap;
    `TBig_map;
    `TContract;
    `TTicket;
  |]

type comparable_type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TBool
  | `TKey_hash
  | `TKey
  | `TTimestamp
  | `TChain_id
  | `TAddress
  | `TPair
  | `TUnion
  | `TOption ]

(* Ensure inclusion of comparable_type_name in type_name *)
let (_ : comparable_type_name -> type_name) = fun x -> (x :> type_name)

let all_comparable_type_names : comparable_type_name array =
  [|
    `TUnit;
    `TInt;
    `TNat;
    `TSignature;
    `TString;
    `TBytes;
    `TMutez;
    `TBool;
    `TKey_hash;
    `TKey;
    `TTimestamp;
    `TChain_id;
    `TAddress;
    `TPair;
    `TUnion;
    `TOption;
  |]

type 'a comparable_and_atomic = 'a
  constraint 'a = [< comparable_type_name] constraint 'a = [< atomic_type_name]

let all_comparable_atomic_type_names : 'a comparable_and_atomic array =
  [|
    `TUnit;
    `TInt;
    `TNat;
    `TSignature;
    `TString;
    `TBytes;
    `TMutez;
    `TBool;
    `TKey_hash;
    `TKey;
    `TTimestamp;
    `TChain_id;
    `TAddress;
  |]

type 'a comparable_and_non_atomic = 'a
  constraint 'a = [< comparable_type_name]
  constraint 'a = [< non_atomic_type_name]

let all_comparable_non_atomic_type_names : 'a comparable_and_non_atomic array =
  [|`TPair; `TUnion; `TOption|]

(* Ensure inclusion of comparable_and_atomic in type_name *)
let (_ : 'a comparable_and_atomic -> type_name) = fun x -> (x :> type_name)

let type_names_count = Array.length all_type_names

let atomic_type_names_count = Array.length all_atomic_type_names

let non_atomic_type_names_count = Array.length all_non_atomic_type_names

let comparable_type_names_count = Array.length all_comparable_type_names

let comparable_atomic_type_names_count =
  Array.length all_comparable_atomic_type_names

let comparable_non_atomic_type_names_count =
  Array.length all_comparable_non_atomic_type_names

(* ------------------------------------------------------------------------- *)
(* Uniform type name generators *)

open Sampling_helpers

let uniform_type_name : type_name sampler =
 fun rng_state ->
  let i = Random.State.int rng_state type_names_count in
  all_type_names.(i)

let uniform_atomic_type_name : atomic_type_name sampler =
 fun rng_state ->
  let i = Random.State.int rng_state atomic_type_names_count in
  all_atomic_type_names.(i)

let uniform_non_atomic_type_name : non_atomic_type_name sampler =
 fun rng_state ->
  let i = Random.State.int rng_state non_atomic_type_names_count in
  all_non_atomic_type_names.(i)

let uniform_comparable_type_name : comparable_type_name sampler =
 fun rng_state ->
  let i = Random.State.int rng_state comparable_type_names_count in
  all_comparable_type_names.(i)

let uniform_comparable_atomic_type_name : 'a comparable_and_atomic sampler =
 fun rng_state ->
  let i = Random.State.int rng_state comparable_atomic_type_names_count in
  all_comparable_atomic_type_names.(i)

let uniform_comparable_non_atomic_type_name :
    'a comparable_and_non_atomic sampler =
 fun rng_state ->
  let i = Random.State.int rng_state comparable_non_atomic_type_names_count in
  all_comparable_non_atomic_type_names.(i)

(* ------------------------------------------------------------------------- *)
(* Existentially packed typed value. *)

type ex_value = Ex_value : {typ : 'a Script_typed_ir.ty; v : 'a} -> ex_value

(* ------------------------------------------------------------------------- *)
(* Random generation functor. *)

let sample_list state ~range ~sampler =
  let length = Base_samplers.sample_in_interval state ~range in
  let list = List.init length (fun _i -> sampler ()) in
  (length, list)

module type S = sig
  val sampling_parameters : Michelson_samplers_parameters.t

  module Crypto_samplers : Crypto_samplers.Finite_key_pool_S

  module Michelson_base : sig
    val int : Alpha_context.Script_int.z Alpha_context.Script_int.num sampler

    val nat : Alpha_context.Script_int.n Alpha_context.Script_int.num sampler

    val signature : Tezos_crypto.Signature.t sampler

    val string : Alpha_context.Script_string.t sampler

    val bytes : bytes sampler

    val tez : Alpha_context.Tez.tez sampler

    val timestamp : Alpha_context.Script_timestamp.t sampler

    val bool : bool sampler
  end

  module Random_type : sig
    val m_type : max_depth:int -> Script_ir_translator.ex_ty sampler

    val m_comparable_type :
      max_depth:int -> Script_ir_translator.ex_comparable_ty sampler

    val m_comparable_type_by_size :
      size:int -> Script_ir_translator.ex_comparable_ty sampler
  end

  module rec Random_value : sig
    val value : 'a Script_typed_ir.ty -> 'a sampler

    val comparable : 'a Script_typed_ir.comparable_ty -> 'a sampler

    val stack : ('a, 'b) Script_typed_ir.stack_ty -> ('a * 'b) sampler
  end
end

module Make (P : Michelson_samplers_parameters.S) : S = struct
  include Michelson_samplers_base.Make_full (P)

  let memo_size =
    Alpha_context.Sapling.Memo_size.parse_z Z.zero |> Result.get_ok

  (* Random generation of Michelson types. *)
  module Random_type = struct
    let type_of_atomic_type_name (at_tn : atomic_type_name) :
        Script_ir_translator.ex_ty =
      match at_tn with
      | `TString -> Ex_ty (String_t {annot = None})
      | `TNat -> Ex_ty (Nat_t {annot = None})
      | `TKey -> Ex_ty (Key_t {annot = None})
      | `TBytes -> Ex_ty (Bytes_t {annot = None})
      | `TBool -> Ex_ty (Bool_t {annot = None})
      | `TAddress -> Ex_ty (Address_t {annot = None})
      | `TTimestamp -> Ex_ty (Timestamp_t {annot = None})
      | `TKey_hash -> Ex_ty (Key_hash_t {annot = None})
      | `TMutez -> Ex_ty (Mutez_t {annot = None})
      | `TSignature -> Ex_ty (Signature_t {annot = None})
      | `TUnit -> Ex_ty (Unit_t {annot = None})
      | `TInt -> Ex_ty (Int_t {annot = None})
      | `TSapling_state -> Ex_ty (Sapling_state_t (memo_size, {annot = None}))
      | `TSapling_transaction ->
          Ex_ty (Sapling_transaction_t (memo_size, {annot = None}))
      | `TChain_id -> Ex_ty (Chain_id_t {annot = None})
      | `TBls12_381_g1 -> Ex_ty (Bls12_381_g1_t {annot = None})
      | `TBls12_381_g2 -> Ex_ty (Bls12_381_g2_t {annot = None})
      | `TBls12_381_fr -> Ex_ty (Bls12_381_fr_t {annot = None})

    let comparable_type_of_comparable_atomic_type_name
        (cmp_tn : 'a comparable_and_atomic) :
        Script_ir_translator.ex_comparable_ty =
      match cmp_tn with
      | `TString -> Ex_comparable_ty (String_key {annot = None})
      | `TNat -> Ex_comparable_ty (Nat_key {annot = None})
      | `TBytes -> Ex_comparable_ty (Bytes_key {annot = None})
      | `TBool -> Ex_comparable_ty (Bool_key {annot = None})
      | `TAddress -> Ex_comparable_ty (Address_key {annot = None})
      | `TTimestamp -> Ex_comparable_ty (Timestamp_key {annot = None})
      | `TKey_hash -> Ex_comparable_ty (Key_hash_key {annot = None})
      | `TMutez -> Ex_comparable_ty (Mutez_key {annot = None})
      | `TInt -> Ex_comparable_ty (Int_key {annot = None})
      | `TUnit -> Ex_comparable_ty (Unit_key {annot = None})
      | `TSignature -> Ex_comparable_ty (Signature_key {annot = None})
      | `TKey -> Ex_comparable_ty (Key_key {annot = None})
      | `TChain_id -> Ex_comparable_ty (Chain_id_key {annot = None})

    let rec m_type ~max_depth : Script_ir_translator.ex_ty sampler =
      let open Script_ir_translator in
      let open M in
      if max_depth = 0 then
        (* let* at_tn = uniform_atomic_type_name in
           return (type_of_atomic_type_name at_tn) *)
        let_star uniform_atomic_type_name (fun at_tn ->
            return (type_of_atomic_type_name at_tn))
      else
        (* let* tn = uniform_type_name in *)
        let_star uniform_type_name (fun tn ->
            match tn with
            | #atomic_type_name as at_tn ->
                return (type_of_atomic_type_name at_tn)
            | `TPair ->
                let max_depth = max_depth - 1 in
                (*let* Ex_ty left = m_type ~max_depth in
                  let* Ex_ty right = m_type ~max_depth in*)
                let_star (m_type ~max_depth) (fun (Ex_ty left) ->
                    let_star (m_type ~max_depth) (fun (Ex_ty right) ->
                        return
                        @@ Ex_ty
                             (Pair_t
                                ( (left, None, None),
                                  (right, None, None),
                                  {annot = None} ))))
            | `TLambda ->
                let max_depth = max_depth - 1 in
                (*let* Ex_ty domain = m_type ~max_depth in
                  let* Ex_ty range = m_type ~max_depth in*)
                let_star (m_type ~max_depth) (fun (Ex_ty domain) ->
                    let_star (m_type ~max_depth) (fun (Ex_ty range) ->
                        return
                        @@ Ex_ty (Lambda_t (domain, range, {annot = None}))))
            | `TUnion ->
                let max_depth = max_depth - 1 in
                (* let* Ex_ty left = m_type ~max_depth in
                   let* Ex_ty right = m_type ~max_depth in *)
                let_star (m_type ~max_depth) (fun (Ex_ty left) ->
                    let_star (m_type ~max_depth) (fun (Ex_ty right) ->
                        return
                        @@ Ex_ty
                             (Union_t
                                ((left, None), (right, None), {annot = None}))))
            | `TOption ->
                (* let* Ex_ty t = m_type ~max_depth:(max_depth - 1) in *)
                let_star
                  (m_type ~max_depth:(max_depth - 1))
                  (fun (Ex_ty t) ->
                    return @@ Ex_ty (Option_t (t, {annot = None})))
            | `TMap ->
                let max_depth = max_depth - 1 in
                (* let* Ex_comparable_ty key = m_comparable_type ~max_depth in
                   let* Ex_ty elt = m_type ~max_depth in *)
                let_star
                  (m_comparable_type ~max_depth)
                  (fun (Ex_comparable_ty key) ->
                    let_star (m_type ~max_depth) (fun (Ex_ty elt) ->
                        return @@ Ex_ty (Map_t (key, elt, {annot = None}))))
            | `TSet ->
                (*let* Ex_comparable_ty key_ty =
                  m_comparable_type ~max_depth:(max_depth - 1)
                                    in*)
                let_star
                  (m_comparable_type ~max_depth:(max_depth - 1))
                  (fun (Ex_comparable_ty key_ty) ->
                    return (Ex_ty (Set_t (key_ty, {annot = None}))))
            | `TList ->
                (* let* Ex_ty elt = m_type ~max_depth:(max_depth - 1) in *)
                let_star
                  (m_type ~max_depth:(max_depth - 1))
                  (fun (Ex_ty elt) ->
                    return (Ex_ty (List_t (elt, {annot = None}))))
            | `TTicket ->
                (* let* Ex_comparable_ty contents =
                     m_comparable_type ~max_depth:(max_depth - 1)
                   in*)
                let_star
                  (m_comparable_type ~max_depth:(max_depth - 1))
                  (fun (Ex_comparable_ty contents) ->
                    return (Ex_ty (Ticket_t (contents, {annot = None}))))
            | `TContract | `TOperation | `TBig_map ->
                (* Don't know what to do with theses. Redraw. *)
                m_type ~max_depth)

    and m_comparable_type ~max_depth :
        Script_ir_translator.ex_comparable_ty sampler =
      let open M in
      let open Script_ir_translator in
      if max_depth = 0 then
        (* let* at_tn = uniform_comparable_atomic_type_name in *)
        let_star uniform_comparable_atomic_type_name (fun at_tn ->
            return (comparable_type_of_comparable_atomic_type_name at_tn))
      else
        (* let* cmp_tn = uniform_comparable_type_name in *)
        let_star uniform_comparable_type_name (fun cmp_tn ->
            match cmp_tn with
            | `TString -> return (Ex_comparable_ty (String_key {annot = None}))
            | `TNat -> return (Ex_comparable_ty (Nat_key {annot = None}))
            | `TPair ->
                let max_depth = max_depth - 1 in
                (*let* Ex_comparable_ty l = m_comparable_type ~max_depth in
                  let* Ex_comparable_ty r = m_comparable_type ~max_depth in*)
                let_star
                  (m_comparable_type ~max_depth)
                  (fun (Ex_comparable_ty l) ->
                    let_star
                      (m_comparable_type ~max_depth)
                      (fun (Ex_comparable_ty r) ->
                        return
                          (Ex_comparable_ty
                             (Pair_key ((l, None), (r, None), {annot = None})))))
            | `TKey -> return (Ex_comparable_ty (Key_key {annot = None}))
            | `TUnion ->
                let max_depth = max_depth - 1 in
                (* let* Ex_comparable_ty l = m_comparable_type ~max_depth in
                   let* Ex_comparable_ty r = m_comparable_type ~max_depth in *)
                let_star
                  (m_comparable_type ~max_depth)
                  (fun (Ex_comparable_ty l) ->
                    let_star
                      (m_comparable_type ~max_depth)
                      (fun (Ex_comparable_ty r) ->
                        return
                          (Ex_comparable_ty
                             (Union_key ((l, None), (r, None), {annot = None})))))
            | `TOption ->
                let max_depth = max_depth - 1 in
                (* let* Ex_comparable_ty t = m_comparable_type ~max_depth in *)
                let_star
                  (m_comparable_type ~max_depth)
                  (fun (Ex_comparable_ty t) ->
                    return (Ex_comparable_ty (Option_key (t, {annot = None}))))
            | `TBytes -> return (Ex_comparable_ty (Bytes_key {annot = None}))
            | `TChain_id ->
                return (Ex_comparable_ty (Chain_id_key {annot = None}))
            | `TBool -> return (Ex_comparable_ty (Bool_key {annot = None}))
            | `TAddress ->
                return (Ex_comparable_ty (Address_key {annot = None}))
            | `TTimestamp ->
                return (Ex_comparable_ty (Timestamp_key {annot = None}))
            | `TKey_hash ->
                return (Ex_comparable_ty (Key_hash_key {annot = None}))
            | `TMutez -> return (Ex_comparable_ty (Mutez_key {annot = None}))
            | `TSignature ->
                return (Ex_comparable_ty (Signature_key {annot = None}))
            | `TUnit -> return (Ex_comparable_ty (Unit_key {annot = None}))
            | `TInt -> return (Ex_comparable_ty (Int_key {annot = None})))

    let rec m_comparable_type_by_size ~size :
        Script_ir_translator.ex_comparable_ty sampler =
      let open M in
      let open Script_ir_translator in
      let atomic_case () =
        let_star uniform_comparable_atomic_type_name (fun at_tn ->
            return (comparable_type_of_comparable_atomic_type_name at_tn))
      in
      let option_case size =
        let size = size - 1 in
        let_star (m_comparable_type_by_size ~size) (fun (Ex_comparable_ty t) ->
            return (Ex_comparable_ty (Option_key (t, {annot = None}))))
      in
      let pair_case size =
        let size = size - 1 in
        let_star
          (Base_samplers.sample_in_interval ~range:{min = 1; max = size - 1})
          (fun size_left ->
            let size_right = size - size_left in
            let_star
              (m_comparable_type_by_size ~size:size_left)
              (fun (Ex_comparable_ty l) ->
                let_star
                  (m_comparable_type_by_size ~size:size_right)
                  (fun (Ex_comparable_ty r) ->
                    return
                      (Ex_comparable_ty
                         (Pair_key ((l, None), (r, None), {annot = None}))))))
      in
      let union_case size =
        let size = size - 1 in
        let_star
          (Base_samplers.sample_in_interval ~range:{min = 1; max = size - 1})
          (fun size_left ->
            let size_right = size - size_left in
            let_star
              (m_comparable_type_by_size ~size:size_left)
              (fun (Ex_comparable_ty l) ->
                let_star
                  (m_comparable_type_by_size ~size:size_right)
                  (fun (Ex_comparable_ty r) ->
                    return
                      (Ex_comparable_ty
                         (Union_key ((l, None), (r, None), {annot = None}))))))
      in

      if size <= 1 then atomic_case ()
      else if size = 2 then option_case size
      else
        let_star uniform_comparable_non_atomic_type_name (fun cmp_tn ->
            match cmp_tn with
            | `TPair -> pair_case size
            | `TUnion -> union_case size
            | `TOption -> option_case size)
  end

  (* Type-directed generation of random values. *)
  module rec Random_value : sig
    val value : 'a Script_typed_ir.ty -> 'a sampler

    val comparable : 'a Script_typed_ir.comparable_ty -> 'a sampler

    val stack : ('a, 'b) Script_typed_ir.stack_ty -> ('a * 'b) sampler
  end = struct
    let address rng_state =
      if Michelson_base.bool rng_state then
        ( Alpha_context.Contract.implicit_contract
            (Crypto_samplers.pkh rng_state),
          "default" )
      else
        (* For a description of the format, see
           tezos-codec describe alpha.contract binary encoding *)
        let string =
          "\001" ^ Base_samplers.uniform_string ~nbytes:20 rng_state ^ "\000"
        in
        let contract =
          Data_encoding.Binary.of_string_exn
            Alpha_context.Contract.encoding
            string
        in
        let ep = Base_samplers.string ~size:{min = 1; max = 31} rng_state in
        (contract, ep)

    let chain_id rng_state =
      let string = Base_samplers.uniform_string ~nbytes:4 rng_state in
      Data_encoding.Binary.of_string_exn Chain_id.encoding string

    let rec value : type a. a Script_typed_ir.ty -> a sampler =
      let open Script_typed_ir in
      fun typ ->
        match typ with
        | Never_t _ -> assert false
        | Unit_t _ -> M.return ()
        | Int_t _ -> Michelson_base.int
        | Nat_t _ -> Michelson_base.nat
        | Signature_t _ -> Michelson_base.signature
        | String_t _ -> Michelson_base.string
        | Bytes_t _ -> Michelson_base.bytes
        | Mutez_t _ -> Michelson_base.tez
        | Key_hash_t _ -> Crypto_samplers.pkh
        | Key_t _ -> Crypto_samplers.pk
        | Timestamp_t _ -> Michelson_base.timestamp
        | Bool_t _ -> Michelson_base.bool
        | Address_t _ -> address
        | Pair_t ((left_t, _, _), (right_t, _, _), _) ->
            M.(
              (* let* left_v = value left_t in
                 let* right_v = value right_t in *)
              let_star (value left_t) (fun left_v ->
                  let_star (value right_t) (fun right_v ->
                      return (left_v, right_v))))
        | Union_t ((left_t, _), (right_t, _), _) ->
            fun rng_state ->
              if Michelson_base.bool rng_state then L (value left_t rng_state)
              else R (value right_t rng_state)
        | Lambda_t (arg_ty, ret_ty, _) -> generate_lambda arg_ty ret_ty
        | Option_t (ty, _) ->
            fun rng_state ->
              if Michelson_base.bool rng_state then None
              else Some (value ty rng_state)
        | List_t (elt_ty, _) -> generate_list elt_ty
        | Set_t (elt_ty, _) -> generate_set elt_ty
        | Map_t (key_ty, val_ty, _) -> generate_map key_ty val_ty
        | Contract_t (arg_ty, _) -> generate_contract arg_ty
        | Operation_t _ -> generate_operation
        | Big_map_t (key_ty, val_ty, _) -> generate_big_map key_ty val_ty
        | Chain_id_t _ -> chain_id
        | Bls12_381_g1_t _ -> generate_bls12_381_g1
        | Bls12_381_g2_t _ -> generate_bls12_381_g2
        | Bls12_381_fr_t _ -> generate_bls12_381_fr
        | Ticket_t (contents_ty, _) ->
            let ty = comparable_downcast contents_ty in
            generate_ticket ty
        | Sapling_transaction_t _ ->
            Stdlib.failwith
              "Michelson_samplers: sapling transactions not handled yet"
        | Sapling_state_t _ ->
            Stdlib.failwith "Michelson_samplers: sapling state not handled yet"
        | Chest_key_t _ ->
            Stdlib.failwith "Michelson_samplers: chest key not handled yet"
        | Chest_t _ ->
            Stdlib.failwith "Michelson_samplers: chest not handled yet"

    and generate_lambda :
        type arg ret.
        arg Script_typed_ir.ty ->
        ret Script_typed_ir.ty ->
        (arg, ret) Script_typed_ir.lambda sampler =
     fun _arg_ty _ret_ty _rng_state ->
      (* TODO: plug michelson sampler *)
      assert false

    and generate_list :
        type elt.
        elt Script_typed_ir.ty -> elt Script_typed_ir.boxed_list sampler =
     fun elt_type rng_state ->
      let (length, elements) =
        (* TODO: fix interface of list sampler *)
        sample_list rng_state ~range:P.parameters.list_size ~sampler:(fun () ->
            value elt_type rng_state)
      in
      Script_typed_ir.{elements; length}

    and generate_set :
        type elt.
        elt Script_typed_ir.comparable_ty -> elt Script_typed_ir.set sampler =
     fun elt_ty rng_state ->
      let ety = comparable_downcast elt_ty in
      let {Script_typed_ir.elements; length = _} =
        generate_list ety rng_state
      in
      List.fold_left
        (fun set x -> Script_set.update x true set)
        (Script_set.empty elt_ty)
        elements

    and generate_map :
        type key elt.
        key Script_typed_ir.comparable_ty ->
        elt Script_typed_ir.ty ->
        (key, elt) Script_typed_ir.map sampler =
     fun key_ty elt_ty rng_state ->
      let size =
        Base_samplers.sample_in_interval rng_state ~range:P.parameters.map_size
      in
      let kty = comparable_downcast key_ty in
      let keys = List.init size (fun _ -> value kty rng_state) in
      let elts = List.init size (fun _ -> value elt_ty rng_state) in
      List.fold_left2
        (fun map key elt -> Script_map.update key (Some elt) map)
        (Script_map.empty key_ty)
        keys
        elts

    and generate_big_map :
        type key elt.
        key Script_typed_ir.comparable_ty ->
        elt Script_typed_ir.ty ->
        (key, elt) Script_typed_ir.big_map sampler =
      let open Script_typed_ir in
      fun key_ty elt_ty rng_state ->
        let open TzPervasives in
        let result =
          Lwt_main.run
            ( Execution_context.make ~rng_state >>=? fun (ctxt, _) ->
              let big_map = Script_ir_translator.empty_big_map key_ty elt_ty in
              (* Cannot have big maps under big maps *)
              let opt_elt_ty = Option_t (elt_ty, {annot = None}) in
              let map = generate_map key_ty opt_elt_ty rng_state in
              Script_map.fold
                (fun k v acc ->
                  acc >>=? fun (bm, ctxt_acc) ->
                  Script_ir_translator.big_map_update ctxt_acc k v bm)
                map
                (return (big_map, ctxt))
              >|= Environment.wrap_tzresult
              >>=? fun (big_map, _) -> return big_map )
        in
        match result with
        | Ok x -> x
        | Error e ->
            Format.eprintf
              "%a@."
              (Error_monad.TzTrace.pp_print Error_monad.pp)
              e ;
            Stdlib.failwith "raise_if_error"

    and generate_contract :
        type arg.
        arg Script_typed_ir.ty -> arg Script_typed_ir.typed_contract sampler =
     fun arg_ty ->
      let open M in
      (* let* addr = value (Address_t None) in*)
      let_star
        (value (Address_t {annot = None}))
        (fun addr -> return (arg_ty, addr))

    and generate_operation :
        (Alpha_context.packed_internal_operation
        * Alpha_context.Lazy_storage.diffs option)
        sampler =
     fun rng_state ->
      let transfer = generate_transfer_tokens rng_state in
      (transfer, None)

    and generate_transfer_tokens :
        Alpha_context.packed_internal_operation sampler =
     fun _rng_state -> Stdlib.failwith "generate_transfer_tokens: unimplemented"

    and generate_bls12_381_g1 : Environment.Bls12_381.G1.t sampler =
     fun rng_state ->
      let b =
        Bls12_381.G1.Uncompressed.(to_bytes (random ~state:rng_state ()))
      in
      match Environment.Bls12_381.G1.of_bytes_opt b with
      | Some x -> x
      | None -> assert false

    and generate_bls12_381_g2 : Environment.Bls12_381.G2.t sampler =
     fun rng_state ->
      let b =
        Bls12_381.G2.Uncompressed.(to_bytes (random ~state:rng_state ()))
      in
      match Environment.Bls12_381.G2.of_bytes_opt b with
      | Some x -> x
      | None -> assert false

    and generate_bls12_381_fr : Environment.Bls12_381.Fr.t sampler =
     fun rng_state ->
      let b = Bls12_381.Fr.(to_bytes (random ~state:rng_state ())) in
      match Environment.Bls12_381.Fr.of_bytes_opt b with
      | Some x -> x
      | None -> assert false

    and generate_ticket :
        type a. a Script_typed_ir.ty -> a Script_typed_ir.ticket sampler =
     fun ty rng_state ->
      let contents = value ty rng_state in
      let ticketer =
        ( Alpha_context.Contract.implicit_contract
            (Crypto_samplers.pkh rng_state),
          "default" )
      in
      let amount = Michelson_base.nat rng_state in
      Script_typed_ir.{ticketer; contents; amount}

    let comparable ty = value (comparable_downcast ty)

    (* Random stack generation. *)
    let rec stack : type a b. (a, b) Script_typed_ir.stack_ty -> (a * b) sampler
        =
      let open M in
      let open Script_typed_ir in
      fun stack_ty ->
        match stack_ty with
        | Item_t (ty, tl, _) ->
            (*
            let* elt = value ty in
            let* tl = stack tl in *)
            let_star (value ty) (fun elt ->
                let_star (stack tl) (fun tl -> return ((elt, tl) : a * b)))
        | Bot_t -> return (EmptyCell, EmptyCell)
  end
end
