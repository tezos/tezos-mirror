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

(** Autocompletion functions (removing holes from Mikhailsky terms). *)

open Sampling_helpers

(* ------------------------------------------------------------------------- *)
(* Helpers *)

let rec stack_length (stack : Type.Stack.t) acc =
  match stack.node with
  | Empty_t -> acc
  | Stack_var_t _ -> acc + 1
  | Item_t (_, tl) -> stack_length tl (acc + 1)

(* We need to sort and remove duplicate elements
   of sets and maps to make them Michelson-compatible. *)
let sort_set_elements elements =
  List.sort_uniq
    (Structural_compare.compare
       ~prim_compare:Mikhailsky.Mikhailsky_signature.compare)
    elements

let sort_map_elements elements =
  let open Micheline in
  List.sort_uniq
    (fun node1 node2 ->
      match (node1, node2) with
      | ( Prim (_, Mikhailsky_prim.D_Elt, [k1; _v1], _),
          Prim (_, Mikhailsky_prim.D_Elt, [k2; _v2], _) ) ->
          Structural_compare.compare
            ~prim_compare:Mikhailsky.Mikhailsky_signature.compare
            k1
            k2
      | _ -> Stdlib.failwith "Autocomp.sort_map_elements: invalid Michelson map")
    elements

(* ------------------------------------------------------------------------- *)
(* Error handling *)

type error_case =
  | Cannot_complete_data of Mikhailsky.node * Kernel.Path.t
  | Cannot_complete_code of Mikhailsky.node * Kernel.Path.t

exception Autocompletion_error of error_case

let cannot_complete_data node path =
  raise (Autocompletion_error (Cannot_complete_data (node, path)))

let cannot_complete_code node path =
  raise (Autocompletion_error (Cannot_complete_code (node, path)))

(* ------------------------------------------------------------------------- *)
(* Code & data autocompletion *)

(* By default, comparable values are unit. *)
let default_comparable_type = Type.unit

let generate_comparable _sp = Mikhailsky.Data.unit

(* Instantiates variables in a base type, remaining variables
   are mapped to some consistent choice of ground type
   (this is made complicated by comparability constraints) *)
let rec instantiate_and_set ty =
  let open Inference.M in
  Inference.instantiate_base ty >>= fun ty -> replace_vars ty

and replace_vars (ty : Type.Base.t) =
  let open Inference.M in
  let node = ty.node in
  match node with
  | Type.Base.Unit_t | Type.Base.Int_t | Type.Base.Nat_t | Type.Base.Bool_t
  | Type.Base.String_t | Type.Base.Bytes_t | Type.Base.Key_hash_t
  | Type.Base.Timestamp_t | Type.Base.Mutez_t | Type.Base.Key_t ->
      return ty
  | Type.Base.Var_t v -> (
      get_repr_exn v >>= fun repr ->
      match repr with
      | Inference.Stack_type _ -> assert false
      | Inference.Base_type {comparable = _; repr = Some _} -> assert false
      | Inference.Base_type {comparable; repr = None} -> (
          match comparable with
          | Inference.Comparable -> return default_comparable_type
          | Inference.Unconstrained | Inference.Not_comparable ->
              return Type.unit))
  | Type.Base.Option_t ty ->
      replace_vars ty >>= fun ty -> return (Type.option ty)
  | Type.Base.Pair_t (lt, rt) ->
      replace_vars lt >>= fun lt ->
      replace_vars rt >>= fun rt -> return (Type.pair lt rt)
  | Type.Base.Or_t (lt, rt) ->
      replace_vars lt >>= fun lt ->
      replace_vars rt >>= fun rt -> return (Type.or_ lt rt)
  | Type.Base.List_t ty -> replace_vars ty >>= fun ty -> return (Type.list ty)
  | Type.Base.Set_t ty -> replace_vars ty >>= fun ty -> return (Type.set ty)
  | Type.Base.Map_t (k, v) ->
      replace_vars k >>= fun k ->
      replace_vars v >>= fun v -> return (Type.map k v)
  | Type.Base.Lambda_t (dom, range) ->
      replace_vars dom >>= fun dom ->
      replace_vars range >>= fun range -> return (Type.lambda dom range)

let rec instantiate_and_set_stack (stack_ty : Type.Stack.t) =
  let open Inference.M in
  let node = stack_ty.node in
  match node with
  | Type.Stack.Empty_t -> return Type.empty
  | Type.Stack.Stack_var_t _ -> return Type.empty
  | Type.Stack.Item_t (hd, tl) ->
      instantiate_and_set hd >>= fun hd ->
      instantiate_and_set_stack tl >>= fun tl -> return (Type.item hd tl)

(* In the following we perform computations in the composite monad
   (sampler o Inference.M.t), it is convenient to define the bind and return
   explicitly. *)
module SM = struct
  type 'a t = 'a Inference.M.t sampler

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
   fun m f rng_state s ->
    let x, s = m rng_state s in
    f x rng_state s
  [@@inline]

  let sample : 'a sampler -> 'a Inference.M.t sampler =
   fun x rng_state st -> (x rng_state, st)
  [@@inline]

  let deterministic : 'a Inference.M.t -> 'a t = fun x _rng_state -> x

  let return x _ s = (x, s) [@@inline]
end

module Make
    (Michelson_base : Michelson_samplers_base.S)
    (Crypto_samplers : Crypto_samplers.Finite_key_pool_S) =
struct
  (* Generates minimally sized random data of specified type.
     Used in autocompletion. *)
  (* /!\ Always call [instantiate_and_set] on the type argument of
         [generate_data]. /!\ *)
  let rec generate_data : Type.Base.t -> Mikhailsky.node SM.t =
   fun ty ->
    let open SM in
    let open Type.Base in
    let desc = ty.node in
    match desc with
    | Var_t _v -> assert false
    | Unit_t -> return Mikhailsky.Data.unit
    | Int_t ->
        sample @@ Michelson_base.int >>= fun i ->
        let i = Protocol.Script_int.to_zint i in
        return (Mikhailsky.Data.big_integer i)
    | Nat_t ->
        sample @@ Michelson_base.nat >>= fun n ->
        let n = Protocol.Script_int.to_zint n in
        return (Mikhailsky.Data.big_natural n)
    | Bool_t ->
        sample Base_samplers.uniform_bool >>= fun b ->
        if b then return Mikhailsky.Data.true_
        else return Mikhailsky.Data.false_
    | String_t ->
        sample Michelson_base.string >>= fun str ->
        let str = Protocol.Script_string.to_string str in
        return (Mikhailsky.Data.string str)
    | Bytes_t ->
        sample Michelson_base.bytes >>= fun bytes ->
        return (Mikhailsky.Data.bytes bytes)
    | Key_hash_t ->
        sample Crypto_samplers.pkh >>= fun pkh ->
        return (Mikhailsky.Data.key_hash pkh)
    | Timestamp_t ->
        sample Michelson_base.timestamp >>= fun tstamp ->
        return (Mikhailsky.Data.timestamp tstamp)
    | Mutez_t ->
        sample Michelson_base.tez >>= fun tz ->
        return (Mikhailsky.Data.mutez tz)
    | Key_t ->
        sample Crypto_samplers.pk >>= fun pk -> return (Mikhailsky.Data.key pk)
    | Option_t ty ->
        sample Base_samplers.uniform_bool >>= fun b ->
        if b then return Mikhailsky.Data.none
        else generate_data ty >>= fun res -> return (Mikhailsky.Data.some res)
    | Pair_t (lty, rty) ->
        generate_data lty >>= fun lv ->
        generate_data rty >>= fun rv -> return (Mikhailsky.Data.pair lv rv)
    | Or_t (lty, rty) ->
        sample Base_samplers.uniform_bool >>= fun b ->
        if b then generate_data lty >>= fun v -> return (Mikhailsky.Data.left v)
        else generate_data rty >>= fun v -> return (Mikhailsky.Data.right v)
    | List_t _ty -> return (Mikhailsky.Data.list [])
    | Set_t _ty -> return (Mikhailsky.Data.set [])
    | Map_t (_kty, _vty) -> return (Mikhailsky.Data.map [])
    | Lambda_t (dom, range) ->
        invent_term Type.(item dom empty) Type.(item range empty)
        >>= fun code -> return (Mikhailsky.Data.lambda code)

  and invent_term (bef : Type.Stack.t) (aft : Type.Stack.t) :
      Mikhailsky.node list SM.t =
    let open SM in
    install_dummy_stack aft [] >>= fun code ->
    let terms = drop_stack bef code in
    return terms

  and drop_stack (stack : Type.Stack.t) code =
    Mikhailsky.Instructions.dropn (stack_length stack 0) :: code

  and install_dummy_stack (stack : Type.Stack.t) (acc : Mikhailsky.node list) =
    let open SM in
    match stack.node with
    | Empty_t -> return acc
    | Stack_var_t _ ->
        let acc = Mikhailsky.(Instructions.push unit_ty Data.unit) :: acc in
        return acc
    | Item_t (hd, tl) ->
        deterministic @@ instantiate_and_set hd >>= fun hd ->
        (match hd.node with
        | Lambda_t (dom, range) ->
            invent_term Type.(item dom empty) Type.(item range empty)
            >>= fun code ->
            let instr = Mikhailsky.(prim I_LAMBDA [seq code] []) in
            return instr
        | _ ->
            generate_data hd >>= fun term ->
            let ty = Mikhailsky.unparse_ty_exn hd in
            return (Mikhailsky.Instructions.push ty term))
        >>= fun instr -> install_dummy_stack tl (instr :: acc)

  (* Autocomplete Mikhailsky data.
     When encountering a hole, we lookup its type and instantiate
     some random data of the specified type. *)
  let rec complete_data :
      Mikhailsky.node -> Kernel.Path.t -> Mikhailsky.node SM.t =
    let open SM in
    fun node path ->
      match node with
      | Micheline.Int (_, _) | Micheline.String (_, _) | Micheline.Bytes (_, _)
        ->
          return node
      | Micheline.Prim (_, D_Hole, _, _) -> (
          deterministic @@ Inference.M.get_data_annot path >>= fun ty_opt ->
          match ty_opt with
          | None -> cannot_complete_data node path
          | Some ty ->
              deterministic @@ instantiate_and_set ty >>= fun ty ->
              generate_data ty)
      | Micheline.Prim (_, A_Set, [Micheline.Seq (_, elements)], _) ->
          complete_data_list (Kernel.Path.at_index 0 path) 0 elements []
          >>= fun elements ->
          let elements = sort_set_elements elements in
          return (Mikhailsky.Data.set elements)
      | Micheline.Prim (_, A_Map, [Micheline.Seq (_, elements)], _) ->
          complete_data_list (Kernel.Path.at_index 0 path) 0 elements []
          >>= fun elements ->
          let elements = sort_map_elements elements in
          return (Mikhailsky.Data.map elements)
      | Micheline.Prim (_, prim, subterms, _) ->
          complete_data_list path 0 subterms [] >>= fun subterms ->
          return (Mikhailsky.prim prim subterms [])
      | Micheline.Seq (_, subterms) ->
          complete_data_list path 0 subterms [] >>= fun subterms ->
          return (Mikhailsky.seq subterms)

  and complete_data_list path i subterms acc =
    let open SM in
    match subterms with
    | [] -> return (List.rev acc)
    | subterm :: tl ->
        let path' = Kernel.Path.at_index i path in
        complete_data subterm path' >>= fun term ->
        complete_data_list path (i + 1) tl (term :: acc)

  let complete_data typing node rng_state =
    let root_type_opt, _ = Inference.M.get_data_annot Kernel.Path.root typing in
    match root_type_opt with
    | None -> Stdlib.failwith "Autocomp.complete_data: cannot get type of expr"
    | Some ty ->
        let _, typing = Inference.instantiate_base ty typing in
        let result, _ =
          try complete_data node Kernel.Path.root rng_state typing
          with Autocompletion_error (Cannot_complete_data (subterm, path)) ->
            Format.eprintf "Cannot complete data@." ;
            Format.eprintf "at path %s@." (Kernel.Path.to_string path) ;
            Format.eprintf "%a@." Mikhailsky.pp subterm ;
            Stdlib.failwith "in autocomp.ml: unrecoverable failure"
        in
        let typ, _typing =
          try Inference.infer_data_with_state result
          with Inference.Ill_typed_script error ->
            Format.eprintf "%a@." Inference.pp_inference_error error ;
            Format.eprintf "%a@." Mikhailsky.pp result ;
            assert false
        in
        (result, typ)

  (* Autocomplete Mikhailsky code. *)

  let rec complete_code :
      Mikhailsky.node -> Kernel.Path.t -> Mikhailsky.node SM.t =
    let open SM in
    fun node path ->
      match node with
      | Micheline.Int (_, _) | Micheline.String (_, _) | Micheline.Bytes (_, _)
        ->
          return node
      | Micheline.Prim (_, I_Hole, _, _) -> (
          deterministic @@ Inference.M.get_instr_annot path >>= function
          | None -> cannot_complete_code node path
          | Some {bef; aft} ->
              deterministic @@ Inference.instantiate bef >>= fun bef ->
              deterministic @@ Inference.instantiate aft >>= fun aft ->
              invent_term bef aft >>= fun code -> return (Mikhailsky.seq code))
      | Micheline.Prim (_, prim, subterms, _) ->
          complete_code_list path 0 subterms [] >>= fun subterms ->
          return (Mikhailsky.prim prim subterms [])
      | Micheline.Seq (_, subterms) ->
          complete_code_list path 0 subterms [] >>= fun subterms ->
          return (Mikhailsky.seq subterms)

  and complete_code_list path i subterms acc =
    let open SM in
    match subterms with
    | [] -> return (List.rev acc)
    | subterm :: tl ->
        let path' = Kernel.Path.at_index i path in
        complete_code subterm path' >>= fun term ->
        complete_code_list path (i + 1) tl (term :: acc)

  let complete_code typing node rng_state =
    let root_type_opt, _ =
      Inference.M.get_instr_annot Kernel.Path.root typing
    in
    match root_type_opt with
    | None -> Stdlib.failwith "Autocomp.complete_code: cannot get type of expr"
    | Some {bef; aft} ->
        let _, typing = Inference.instantiate bef typing in
        let _, typing = Inference.instantiate aft typing in
        let result, _ =
          try complete_code node Kernel.Path.root rng_state typing with
          | Autocompletion_error (Cannot_complete_code (subterm, path)) ->
              Format.eprintf "Cannot complete code@." ;
              Format.eprintf "at path %s@." (Kernel.Path.to_string path) ;
              Format.eprintf "%a@." Mikhailsky.pp subterm ;
              Stdlib.failwith "in autocomp.ml: unrecoverable failure"
          | _ -> assert false
        in
        let (bef, aft), typing =
          try Inference.infer_with_state result
          with Inference.Ill_typed_script error ->
            Format.eprintf "%a@." Inference.pp_inference_error error ;
            Format.eprintf "%a@." Mikhailsky.pp result ;
            assert false
        in
        let bef, typing = instantiate_and_set_stack bef typing in
        let aft, typing = instantiate_and_set_stack aft typing in
        (result, (bef, aft), typing)
end
