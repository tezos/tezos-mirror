(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context
open Micheline
open Script_typed_ir
open Michelson_v1_primitives
module Unparse_costs = Michelson_v1_gas.Cost_of.Unparsing

type unparsing_mode = Optimized | Readable | Optimized_legacy

(* This part contains the unparsing that does not depend on parsing
   (everything that cannot contain a lambda). The rest is located at
   the end of the file. *)

let unparse_memo_size ~loc memo_size =
  let z = Sapling.Memo_size.unparse_to_z memo_size in
  Int (loc, z)

let rec unparse_ty_and_entrypoints_uncarbonated :
    type a ac loc.
    loc:loc -> (a, ac) ty -> a entrypoints_node -> loc Script.michelson_node =
 fun ~loc ty {nested = nested_entrypoints; at_node} ->
  let name, args =
    match ty with
    | Unit_t -> (T_unit, [])
    | Int_t -> (T_int, [])
    | Nat_t -> (T_nat, [])
    | Signature_t -> (T_signature, [])
    | String_t -> (T_string, [])
    | Bytes_t -> (T_bytes, [])
    | Mutez_t -> (T_mutez, [])
    | Bool_t -> (T_bool, [])
    | Key_hash_t -> (T_key_hash, [])
    | Key_t -> (T_key, [])
    | Timestamp_t -> (T_timestamp, [])
    | Address_t -> (T_address, [])
    | Operation_t -> (T_operation, [])
    | Chain_id_t -> (T_chain_id, [])
    | Never_t -> (T_never, [])
    | Bls12_381_g1_t -> (T_bls12_381_g1, [])
    | Bls12_381_g2_t -> (T_bls12_381_g2, [])
    | Bls12_381_fr_t -> (T_bls12_381_fr, [])
    | Contract_t (ut, _meta) ->
        let t =
          unparse_ty_and_entrypoints_uncarbonated ~loc ut no_entrypoints
        in
        (T_contract, [t])
    | Pair_t (utl, utr, _meta, _) -> (
        let tl =
          unparse_ty_and_entrypoints_uncarbonated ~loc utl no_entrypoints
        in
        let tr =
          unparse_ty_and_entrypoints_uncarbonated ~loc utr no_entrypoints
        in
        (* Fold [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an] *)
        (* Note that the folding does not happen if the pair on the right has an
           annotation because this annotation would be lost *)
        match tr with
        | Prim (_, T_pair, ts, []) -> (T_pair, tl :: ts)
        | _ -> (T_pair, [tl; tr]))
    | Or_t (utl, utr, _meta, _) ->
        let entrypoints_l, entrypoints_r =
          match nested_entrypoints with
          | Entrypoints_None -> (no_entrypoints, no_entrypoints)
          | Entrypoints_Or {left; right} -> (left, right)
        in
        let tl =
          unparse_ty_and_entrypoints_uncarbonated ~loc utl entrypoints_l
        in
        let tr =
          unparse_ty_and_entrypoints_uncarbonated ~loc utr entrypoints_r
        in
        (T_or, [tl; tr])
    | Lambda_t (uta, utr, _meta) ->
        let ta =
          unparse_ty_and_entrypoints_uncarbonated ~loc uta no_entrypoints
        in
        let tr =
          unparse_ty_and_entrypoints_uncarbonated ~loc utr no_entrypoints
        in
        (T_lambda, [ta; tr])
    | Option_t (ut, _meta, _) ->
        let ut =
          unparse_ty_and_entrypoints_uncarbonated ~loc ut no_entrypoints
        in
        (T_option, [ut])
    | List_t (ut, _meta) ->
        let t =
          unparse_ty_and_entrypoints_uncarbonated ~loc ut no_entrypoints
        in
        (T_list, [t])
    | Ticket_t (ut, _meta) ->
        let t = unparse_comparable_ty_uncarbonated ~loc ut in
        (T_ticket, [t])
    | Set_t (ut, _meta) ->
        let t = unparse_comparable_ty_uncarbonated ~loc ut in
        (T_set, [t])
    | Map_t (uta, utr, _meta) ->
        let ta = unparse_comparable_ty_uncarbonated ~loc uta in
        let tr =
          unparse_ty_and_entrypoints_uncarbonated ~loc utr no_entrypoints
        in
        (T_map, [ta; tr])
    | Big_map_t (uta, utr, _meta) ->
        let ta = unparse_comparable_ty_uncarbonated ~loc uta in
        let tr =
          unparse_ty_and_entrypoints_uncarbonated ~loc utr no_entrypoints
        in
        (T_big_map, [ta; tr])
    | Sapling_transaction_t memo_size ->
        (T_sapling_transaction, [unparse_memo_size ~loc memo_size])
    | Sapling_transaction_deprecated_t memo_size ->
        (T_sapling_transaction_deprecated, [unparse_memo_size ~loc memo_size])
    | Sapling_state_t memo_size ->
        (T_sapling_state, [unparse_memo_size ~loc memo_size])
    | Chest_key_t -> (T_chest_key, [])
    | Chest_t -> (T_chest, [])
  in
  let annot =
    match at_node with
    | None -> []
    | Some {name; original_type_expr = _} ->
        [Entrypoint.unparse_as_field_annot name]
  in
  Prim (loc, name, args, annot)

and unparse_comparable_ty_uncarbonated :
    type a loc. loc:loc -> a comparable_ty -> loc Script.michelson_node =
 fun ~loc ty -> unparse_ty_and_entrypoints_uncarbonated ~loc ty no_entrypoints

let unparse_ty_uncarbonated ~loc ty =
  unparse_ty_and_entrypoints_uncarbonated ~loc ty no_entrypoints

let unparse_ty ~loc ctxt ty =
  let open Result_syntax in
  let+ ctxt = Gas.consume ctxt (Unparse_costs.unparse_type ty) in
  (unparse_ty_uncarbonated ~loc ty, ctxt)

let unparse_parameter_ty ~loc ctxt ty ~entrypoints =
  let open Result_syntax in
  let+ ctxt = Gas.consume ctxt (Unparse_costs.unparse_type ty) in
  (unparse_ty_and_entrypoints_uncarbonated ~loc ty entrypoints.root, ctxt)

let serialize_ty_for_error ty =
  (*
    Types are bounded by [Constants.michelson_maximum_type_size], so
    [unparse_ty_uncarbonated] and [strip_locations] are bounded in time.

    It is hence OK to use them in errors that are not caught in the validation
    (only once in apply).
  *)
  unparse_ty_uncarbonated ~loc:() ty |> Micheline.strip_locations

let rec unparse_stack_uncarbonated :
    type a s. (a, s) stack_ty -> Script.expr list = function
  | Bot_t -> []
  | Item_t (ty, rest) ->
      let uty = unparse_ty_uncarbonated ~loc:() ty in
      let urest = unparse_stack_uncarbonated rest in
      strip_locations uty :: urest

let serialize_stack_for_error ctxt stack_ty =
  match Gas.level ctxt with
  | Unaccounted -> unparse_stack_uncarbonated stack_ty
  | Limited _ -> []

let unparse_unit ~loc ctxt () = Ok (Prim (loc, D_Unit, [], []), ctxt)

let unparse_int ~loc ctxt v = Ok (Int (loc, Script_int.to_zint v), ctxt)

let unparse_nat ~loc ctxt v = Ok (Int (loc, Script_int.to_zint v), ctxt)

let unparse_string ~loc ctxt s =
  Ok (String (loc, Script_string.to_string s), ctxt)

let unparse_bytes ~loc ctxt s = Ok (Bytes (loc, s), ctxt)

let unparse_bool ~loc ctxt b =
  Ok (Prim (loc, (if b then D_True else D_False), [], []), ctxt)

let unparse_timestamp ~loc ctxt mode t =
  let open Result_syntax in
  match mode with
  | Optimized | Optimized_legacy ->
      return (Int (loc, Script_timestamp.to_zint t), ctxt)
  | Readable -> (
      let* ctxt = Gas.consume ctxt Unparse_costs.timestamp_readable in
      match Script_timestamp.to_notation t with
      | None -> return (Int (loc, Script_timestamp.to_zint t), ctxt)
      | Some s -> return (String (loc, s), ctxt))

let unparse_address ~loc ctxt mode {destination; entrypoint} =
  let open Result_syntax in
  match mode with
  | Optimized | Optimized_legacy ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.contract_optimized in
      let bytes =
        Data_encoding.Binary.to_bytes_exn
          Data_encoding.(tup2 Destination.encoding Entrypoint.value_encoding)
          (destination, entrypoint)
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.contract_readable in
      let notation =
        Destination.to_b58check destination
        ^ Entrypoint.to_address_suffix entrypoint
      in
      (String (loc, notation), ctxt)

let unparse_contract ~loc ctxt mode typed_contract =
  let destination = Typed_contract.destination typed_contract in
  let entrypoint = Typed_contract.entrypoint typed_contract in
  let address = {destination; entrypoint} in
  unparse_address ~loc ctxt mode address

let unparse_signature ~loc ctxt mode s =
  let open Result_syntax in
  let s = Script_signature.get s in
  match mode with
  | Optimized | Optimized_legacy ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.signature_optimized in
      let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.signature_readable in
      (String (loc, Signature.to_b58check s), ctxt)

let unparse_mutez ~loc ctxt v = Ok (Int (loc, Z.of_int64 (Tez.to_mutez v)), ctxt)

let unparse_key ~loc ctxt mode k =
  let open Result_syntax in
  match mode with
  | Optimized | Optimized_legacy ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.public_key_optimized in
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.public_key_readable in
      (String (loc, Signature.Public_key.to_b58check k), ctxt)

let unparse_key_hash ~loc ctxt mode k =
  let open Result_syntax in
  match mode with
  | Optimized | Optimized_legacy ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.key_hash_optimized in
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.key_hash_readable in
      (String (loc, Signature.Public_key_hash.to_b58check k), ctxt)

(* Operations are only unparsed during the production of execution traces of
   the interpreter. *)
let unparse_operation ~loc ctxt {piop; lazy_storage_diff = _} =
  let open Result_syntax in
  let iop = Apply_internal_results.packed_internal_operation piop in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Apply_internal_results.internal_operation_encoding
      iop
  in
  let+ ctxt = Gas.consume ctxt (Unparse_costs.operation bytes) in
  (Bytes (loc, bytes), ctxt)

let unparse_chain_id ~loc ctxt mode chain_id =
  let open Result_syntax in
  match mode with
  | Optimized | Optimized_legacy ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.chain_id_optimized in
      let bytes =
        Data_encoding.Binary.to_bytes_exn Script_chain_id.encoding chain_id
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let+ ctxt = Gas.consume ctxt Unparse_costs.chain_id_readable in
      (String (loc, Script_chain_id.to_b58check chain_id), ctxt)

let unparse_bls12_381_g1 ~loc ctxt x =
  let open Result_syntax in
  let+ ctxt = Gas.consume ctxt Unparse_costs.bls12_381_g1 in
  let bytes = Script_bls.G1.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_bls12_381_g2 ~loc ctxt x =
  let open Result_syntax in
  let+ ctxt = Gas.consume ctxt Unparse_costs.bls12_381_g2 in
  let bytes = Script_bls.G2.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_bls12_381_fr ~loc ctxt x =
  let open Result_syntax in
  let+ ctxt = Gas.consume ctxt Unparse_costs.bls12_381_fr in
  let bytes = Script_bls.Fr.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_with_data_encoding ~loc ctxt s unparse_cost encoding =
  let open Lwt_result_syntax in
  let*? ctxt = Gas.consume ctxt unparse_cost in
  let bytes = Data_encoding.Binary.to_bytes_exn encoding s in
  return (Bytes (loc, bytes), ctxt)

(* -- Unparsing data of complex types -- *)

type ('ty, 'depth) comb_witness =
  | Comb_Pair : ('t, 'd) comb_witness -> (_ * 't, unit -> 'd) comb_witness
  | Comb_Any : (_, _) comb_witness

let unparse_pair (type r) ~loc unparse_l unparse_r ctxt mode
    (r_comb_witness : (r, unit -> unit -> _) comb_witness) (l, (r : r)) =
  let open Lwt_result_syntax in
  let* l, ctxt = unparse_l ctxt l in
  let+ r, ctxt = unparse_r ctxt r in
  (* Fold combs.
     For combs, three notations are supported:
     - a) [Pair x1 (Pair x2 ... (Pair xn-1 xn) ...)],
     - b) [Pair x1 x2 ... xn-1 xn], and
     - c) [{x1; x2; ...; xn-1; xn}].
     In readable mode, we always use b),
     in optimized mode we use the shortest to serialize:
     - for n=2, [Pair x1 x2],
     - for n=3, [Pair x1 (Pair x2 x3)],
     - for n>=4, [{x1; x2; ...; xn}].
  *)
  let res =
    match (mode, r_comb_witness, r) with
    | Optimized, Comb_Pair _, Micheline.Seq (_, r) ->
        (* Optimized case n > 4 *)
        Micheline.Seq (loc, l :: r)
    | ( Optimized,
        Comb_Pair (Comb_Pair _),
        Prim (_, D_Pair, [x2; Prim (_, D_Pair, [x3; x4], [])], []) ) ->
        (* Optimized case n = 4 *)
        Micheline.Seq (loc, [l; x2; x3; x4])
    | Readable, Comb_Pair _, Prim (_, D_Pair, xs, []) ->
        (* Readable case n > 2 *)
        Prim (loc, D_Pair, l :: xs, [])
    | _ ->
        (* The remaining cases are:
            - Optimized n = 2,
            - Optimized n = 3, and
            - Readable n = 2,
            - Optimized_legacy, any n *)
        Prim (loc, D_Pair, [l; r], [])
  in
  (res, ctxt)

let unparse_or ~loc unparse_l unparse_r ctxt =
  let open Lwt_result_syntax in
  function
  | L l ->
      let+ l, ctxt = unparse_l ctxt l in
      (Prim (loc, D_Left, [l], []), ctxt)
  | R r ->
      let+ r, ctxt = unparse_r ctxt r in
      (Prim (loc, D_Right, [r], []), ctxt)

let unparse_option ~loc unparse_v ctxt =
  let open Lwt_result_syntax in
  function
  | Some v ->
      let+ v, ctxt = unparse_v ctxt v in
      (Prim (loc, D_Some, [v], []), ctxt)
  | None -> return (Prim (loc, D_None, [], []), ctxt)

(* -- Unparsing data of comparable types -- *)

let comb_witness2 :
    type t tc. (t, tc) ty -> (t, unit -> unit -> unit) comb_witness = function
  | Pair_t (_, Pair_t _, _, _) -> Comb_Pair (Comb_Pair Comb_Any)
  | Pair_t _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

let rec unparse_comparable_data_rec :
    type a loc.
    loc:loc ->
    context ->
    unparsing_mode ->
    a comparable_ty ->
    a ->
    (loc Script.michelson_node * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun ~loc ctxt mode ty a ->
    (* No need for stack_depth here. Unlike [unparse_data],
       [unparse_comparable_data] doesn't call [unparse_code].
       The stack depth is bounded by the type depth, currently bounded
       by 1000 (michelson_maximum_type_size). *)
    let*? ctxt =
      Gas.consume ctxt Unparse_costs.unparse_data_cycle
      (* We could have a smaller cost but let's keep it consistent with
         [unparse_data] for now. *)
    in
    match (ty, a) with
    | Unit_t, v -> Lwt.return @@ unparse_unit ~loc ctxt v
    | Int_t, v -> Lwt.return @@ unparse_int ~loc ctxt v
    | Nat_t, v -> Lwt.return @@ unparse_nat ~loc ctxt v
    | String_t, s -> Lwt.return @@ unparse_string ~loc ctxt s
    | Bytes_t, s -> Lwt.return @@ unparse_bytes ~loc ctxt s
    | Bool_t, b -> Lwt.return @@ unparse_bool ~loc ctxt b
    | Timestamp_t, t -> Lwt.return @@ unparse_timestamp ~loc ctxt mode t
    | Address_t, address -> Lwt.return @@ unparse_address ~loc ctxt mode address
    | Signature_t, s -> Lwt.return @@ unparse_signature ~loc ctxt mode s
    | Mutez_t, v -> Lwt.return @@ unparse_mutez ~loc ctxt v
    | Key_t, k -> Lwt.return @@ unparse_key ~loc ctxt mode k
    | Key_hash_t, k -> Lwt.return @@ unparse_key_hash ~loc ctxt mode k
    | Chain_id_t, chain_id ->
        Lwt.return @@ unparse_chain_id ~loc ctxt mode chain_id
    | Pair_t (tl, tr, _, YesYes), pair ->
        let r_witness = comb_witness2 tr in
        let unparse_l ctxt v =
          unparse_comparable_data_rec ~loc ctxt mode tl v
        in
        let unparse_r ctxt v =
          unparse_comparable_data_rec ~loc ctxt mode tr v
        in
        unparse_pair ~loc unparse_l unparse_r ctxt mode r_witness pair
    | Or_t (tl, tr, _, YesYes), v ->
        let unparse_l ctxt v =
          unparse_comparable_data_rec ~loc ctxt mode tl v
        in
        let unparse_r ctxt v =
          unparse_comparable_data_rec ~loc ctxt mode tr v
        in
        unparse_or ~loc unparse_l unparse_r ctxt v
    | Option_t (t, _, Yes), v ->
        let unparse_v ctxt v = unparse_comparable_data_rec ~loc ctxt mode t v in
        unparse_option ~loc unparse_v ctxt v
    | Never_t, _ -> .

let account_for_future_serialization_cost unparsed_data ctxt =
  let open Result_syntax in
  let* ctxt = Gas.consume ctxt (Script.strip_locations_cost unparsed_data) in
  let unparsed_data = Micheline.strip_locations unparsed_data in
  let+ ctxt =
    Gas.consume ctxt (Script.micheline_serialization_cost unparsed_data)
  in
  (unparsed_data, ctxt)

type unparse_code_rec =
  t ->
  stack_depth:int ->
  unparsing_mode ->
  Script.node ->
  ((canonical_location, prim) node * t, error trace) result Lwt.t

module type MICHELSON_PARSER = sig
  val opened_ticket_type :
    Script.location ->
    'a comparable_ty ->
    (address, ('a, Script_int.n Script_int.num) pair) pair comparable_ty
    tzresult

  val parse_packable_ty :
    context ->
    stack_depth:int ->
    legacy:bool ->
    Script.node ->
    (ex_ty * context) tzresult

  val parse_data :
    unparse_code_rec:unparse_code_rec ->
    elab_conf:Script_ir_translator_config.elab_config ->
    stack_depth:int ->
    context ->
    allow_forged_tickets:bool ->
    allow_forged_lazy_storage_id:bool ->
    ('a, 'ac) ty ->
    Script.node ->
    ('a * t) tzresult Lwt.t
end

module Data_unparser (P : MICHELSON_PARSER) = struct
  open Script_tc_errors

  (* -- Unparsing data of any type -- *)
  let rec unparse_data_rec :
      type a ac.
      context ->
      stack_depth:int ->
      unparsing_mode ->
      (a, ac) ty ->
      a ->
      (Script.node * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    fun ctxt ~stack_depth mode ty a ->
      let*? ctxt = Gas.consume ctxt Unparse_costs.unparse_data_cycle in
      let non_terminal_recursion ctxt mode ty a =
        if Compare.Int.(stack_depth > 10_000) then
          tzfail Script_tc_errors.Unparsing_too_many_recursive_calls
        else unparse_data_rec ctxt ~stack_depth:(stack_depth + 1) mode ty a
      in
      let loc = Micheline.dummy_location in
      match (ty, a) with
      | Unit_t, v -> Lwt.return @@ unparse_unit ~loc ctxt v
      | Int_t, v -> Lwt.return @@ unparse_int ~loc ctxt v
      | Nat_t, v -> Lwt.return @@ unparse_nat ~loc ctxt v
      | String_t, s -> Lwt.return @@ unparse_string ~loc ctxt s
      | Bytes_t, s -> Lwt.return @@ unparse_bytes ~loc ctxt s
      | Bool_t, b -> Lwt.return @@ unparse_bool ~loc ctxt b
      | Timestamp_t, t -> Lwt.return @@ unparse_timestamp ~loc ctxt mode t
      | Address_t, address ->
          Lwt.return @@ unparse_address ~loc ctxt mode address
      | Contract_t _, contract ->
          Lwt.return @@ unparse_contract ~loc ctxt mode contract
      | Signature_t, s -> Lwt.return @@ unparse_signature ~loc ctxt mode s
      | Mutez_t, v -> Lwt.return @@ unparse_mutez ~loc ctxt v
      | Key_t, k -> Lwt.return @@ unparse_key ~loc ctxt mode k
      | Key_hash_t, k -> Lwt.return @@ unparse_key_hash ~loc ctxt mode k
      | Operation_t, operation ->
          Lwt.return @@ unparse_operation ~loc ctxt operation
      | Chain_id_t, chain_id ->
          Lwt.return @@ unparse_chain_id ~loc ctxt mode chain_id
      | Bls12_381_g1_t, x -> Lwt.return @@ unparse_bls12_381_g1 ~loc ctxt x
      | Bls12_381_g2_t, x -> Lwt.return @@ unparse_bls12_381_g2 ~loc ctxt x
      | Bls12_381_fr_t, x -> Lwt.return @@ unparse_bls12_381_fr ~loc ctxt x
      | Pair_t (tl, tr, _, _), pair ->
          let r_witness = comb_witness2 tr in
          let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
          let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
          unparse_pair ~loc unparse_l unparse_r ctxt mode r_witness pair
      | Or_t (tl, tr, _, _), v ->
          let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
          let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
          unparse_or ~loc unparse_l unparse_r ctxt v
      | Option_t (t, _, _), v ->
          let unparse_v ctxt v = non_terminal_recursion ctxt mode t v in
          unparse_option ~loc unparse_v ctxt v
      | List_t (t, _), items ->
          let+ items, ctxt =
            List.fold_left_es
              (fun (l, ctxt) element ->
                let+ unparsed, ctxt =
                  non_terminal_recursion ctxt mode t element
                in
                (unparsed :: l, ctxt))
              ([], ctxt)
              items.elements
          in
          (Micheline.Seq (loc, List.rev items), ctxt)
      | Ticket_t (t, _), {ticketer; contents; amount} ->
          (* ideally we would like to allow a little overhead here because it is only used for unparsing *)
          let*? t = P.opened_ticket_type loc t in
          let destination : Destination.t = Contract ticketer in
          let addr = {destination; entrypoint = Entrypoint.default} in
          (unparse_data_rec [@tailcall])
            ctxt
            ~stack_depth
            mode
            t
            (addr, (contents, (amount :> Script_int.n Script_int.num)))
      | Set_t (t, _), set ->
          let+ items, ctxt =
            List.fold_left_es
              (fun (l, ctxt) item ->
                let+ item, ctxt =
                  unparse_comparable_data_rec ~loc ctxt mode t item
                in
                (item :: l, ctxt))
              ([], ctxt)
              (Script_set.fold (fun e acc -> e :: acc) set [])
          in
          (Micheline.Seq (loc, items), ctxt)
      | Map_t (kt, vt, _), map ->
          let items = Script_map.fold (fun k v acc -> (k, v) :: acc) map [] in
          let+ items, ctxt =
            unparse_items_rec
              ctxt
              ~stack_depth:(stack_depth + 1)
              mode
              kt
              vt
              items
          in
          (Micheline.Seq (loc, items), ctxt)
      | Big_map_t (_kt, _vt, _), Big_map {id = Some id; diff = {size; _}; _}
        when Compare.Int.( = ) size 0 ->
          return (Micheline.Int (loc, Big_map.Id.unparse_to_z id), ctxt)
      | Big_map_t (kt, vt, _), Big_map {id = Some id; diff = {map; _}; _} ->
          let items =
            Big_map_overlay.fold (fun _ (k, v) acc -> (k, v) :: acc) map []
          in
          let items =
            (* Sort the items in Michelson comparison order and not in key
               hash order. This code path is only exercised for tracing,
               so we don't bother carbonating this sort operation
               precisely. Also, the sort uses a reverse compare because
               [unparse_items] will reverse the result. *)
            List.sort
              (fun (a, _) (b, _) -> Script_comparable.compare_comparable kt b a)
              items
          in
          (* this can't fail if the original type is well-formed
             because [option vt] is always strictly smaller than [big_map kt vt] *)
          let*? vt = option_t loc vt in
          let+ items, ctxt =
            unparse_items_rec
              ctxt
              ~stack_depth:(stack_depth + 1)
              mode
              kt
              vt
              items
          in
          ( Micheline.Prim
              ( loc,
                D_Pair,
                [Int (loc, Big_map.Id.unparse_to_z id); Seq (loc, items)],
                [] ),
            ctxt )
      | Big_map_t (kt, vt, _), Big_map {id = None; diff = {map; _}; _} ->
          let items =
            Big_map_overlay.fold
              (fun _ (k, v) acc ->
                match v with None -> acc | Some v -> (k, v) :: acc)
              map
              []
          in
          let items =
            (* See note above. *)
            List.sort
              (fun (a, _) (b, _) -> Script_comparable.compare_comparable kt b a)
              items
          in
          let+ items, ctxt =
            unparse_items_rec
              ctxt
              ~stack_depth:(stack_depth + 1)
              mode
              kt
              vt
              items
          in
          (Micheline.Seq (loc, items), ctxt)
      | Lambda_t _, Lam (_, original_code) ->
          unparse_code_rec
            ctxt
            ~stack_depth:(stack_depth + 1)
            mode
            original_code
      | Lambda_t _, LamRec (_, original_code) ->
          let+ body, ctxt =
            unparse_code_rec
              ctxt
              ~stack_depth:(stack_depth + 1)
              mode
              original_code
          in
          (Micheline.Prim (loc, D_Lambda_rec, [body], []), ctxt)
      | Never_t, _ -> .
      | Sapling_transaction_t _, s ->
          let*? ctxt = Gas.consume ctxt (Unparse_costs.sapling_transaction s) in
          let bytes =
            Data_encoding.Binary.to_bytes_exn Sapling.transaction_encoding s
          in
          return (Bytes (loc, bytes), ctxt)
      | Sapling_transaction_deprecated_t _, s ->
          let*? ctxt =
            Gas.consume ctxt (Unparse_costs.sapling_transaction_deprecated s)
          in
          let bytes =
            Data_encoding.Binary.to_bytes_exn
              Sapling.Legacy.transaction_encoding
              s
          in
          return (Bytes (loc, bytes), ctxt)
      | Sapling_state_t _, {id; diff; _} ->
          let*? ctxt = Gas.consume ctxt (Unparse_costs.sapling_diff diff) in
          return
            ( (match diff with
              | {commitments_and_ciphertexts = []; nullifiers = []} -> (
                  match id with
                  | None -> Micheline.Seq (loc, [])
                  | Some id ->
                      let id = Sapling.Id.unparse_to_z id in
                      Micheline.Int (loc, id))
              | diff -> (
                  let diff_bytes =
                    Data_encoding.Binary.to_bytes_exn Sapling.diff_encoding diff
                  in
                  let unparsed_diff = Bytes (loc, diff_bytes) in
                  match id with
                  | None -> unparsed_diff
                  | Some id ->
                      let id = Sapling.Id.unparse_to_z id in
                      Micheline.Prim
                        (loc, D_Pair, [Int (loc, id); unparsed_diff], []))),
              ctxt )
      | Chest_key_t, s ->
          unparse_with_data_encoding
            ~loc
            ctxt
            s
            Unparse_costs.chest_key
            Script_timelock.chest_key_encoding
      | Chest_t, s ->
          unparse_with_data_encoding
            ~loc
            ctxt
            s
            (Unparse_costs.chest
               ~plaintext_size:(Script_timelock.get_plaintext_size s))
            Script_timelock.chest_encoding

  and unparse_items_rec :
      type k v vc.
      context ->
      stack_depth:int ->
      unparsing_mode ->
      k comparable_ty ->
      (v, vc) ty ->
      (k * v) list ->
      (Script.node list * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    fun ctxt ~stack_depth mode kt vt items ->
      List.fold_left_es
        (fun (l, ctxt) (k, v) ->
          let loc = Micheline.dummy_location in
          let* key, ctxt = unparse_comparable_data_rec ~loc ctxt mode kt k in
          let+ value, ctxt =
            unparse_data_rec ctxt ~stack_depth:(stack_depth + 1) mode vt v
          in
          (Prim (loc, D_Elt, [key; value], []) :: l, ctxt))
        ([], ctxt)
        items

  and unparse_code_rec ctxt ~stack_depth mode code =
    let open Lwt_result_syntax in
    let elab_conf = Script_ir_translator_config.make ~legacy:true () in
    let*? ctxt = Gas.consume ctxt Unparse_costs.unparse_instr_cycle in
    let non_terminal_recursion ctxt mode code =
      if Compare.Int.(stack_depth > 10_000) then
        tzfail Unparsing_too_many_recursive_calls
      else unparse_code_rec ctxt ~stack_depth:(stack_depth + 1) mode code
    in
    match code with
    | Prim (loc, I_PUSH, [ty; data], annot) ->
        let*? Ex_ty t, ctxt =
          P.parse_packable_ty
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~legacy:elab_conf.legacy
            ty
        in
        let allow_forged_tickets, allow_forged_lazy_storage_id =
          (false, false)
          (* Forgeable in PUSH data are already forbidden at parsing,
             the only case for which this matters is storing a lambda resulting
             from APPLYing a non-forgeable but this cannot happen either as long
             as all packable values are also forgeable. *)
        in
        let* data, ctxt =
          P.parse_data
            ~unparse_code_rec
            ~elab_conf
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~allow_forged_tickets
            ~allow_forged_lazy_storage_id
            t
            data
        in
        let* data, ctxt =
          unparse_data_rec ctxt ~stack_depth:(stack_depth + 1) mode t data
        in
        return (Prim (loc, I_PUSH, [ty; data], annot), ctxt)
    | Seq (loc, items) ->
        let* items, ctxt =
          List.fold_left_es
            (fun (l, ctxt) item ->
              let+ item, ctxt = non_terminal_recursion ctxt mode item in
              (item :: l, ctxt))
            ([], ctxt)
            items
        in
        return (Micheline.Seq (loc, List.rev items), ctxt)
    | Prim (loc, prim, items, annot) ->
        let* items, ctxt =
          List.fold_left_es
            (fun (l, ctxt) item ->
              let+ item, ctxt = non_terminal_recursion ctxt mode item in
              (item :: l, ctxt))
            ([], ctxt)
            items
        in
        return (Prim (loc, prim, List.rev items, annot), ctxt)
    | (Int _ | String _ | Bytes _) as atom -> return (atom, ctxt)

  let unparse_data ctxt ~stack_depth mode ty v =
    let open Lwt_result_syntax in
    let* unparsed_data, ctxt = unparse_data_rec ctxt ~stack_depth mode ty v in
    Lwt.return (account_for_future_serialization_cost unparsed_data ctxt)

  let unparse_code ctxt ~stack_depth mode v =
    let open Lwt_result_syntax in
    let* unparsed_data, ctxt = unparse_code_rec ctxt ~stack_depth mode v in
    Lwt.return (account_for_future_serialization_cost unparsed_data ctxt)

  let unparse_items ctxt ~stack_depth mode ty vty vs =
    let open Lwt_result_syntax in
    let* unparsed_datas, ctxt =
      unparse_items_rec ctxt ~stack_depth mode ty vty vs
    in
    let*? unparsed_datas, ctxt =
      List.fold_left_e
        (fun (acc, ctxt) unparsed_data ->
          let open Result_syntax in
          let+ unparsed_data, ctxt =
            account_for_future_serialization_cost unparsed_data ctxt
          in
          (unparsed_data :: acc, ctxt))
        ([], ctxt)
        unparsed_datas
    in
    return (List.rev unparsed_datas, ctxt)

  module Internal_for_benchmarking = struct
    let unparse_data = unparse_data_rec

    let unparse_code = unparse_code_rec
  end
end

let unparse_comparable_data ctxt mode ty v =
  let open Lwt_result_syntax in
  let* unparsed_data, ctxt =
    unparse_comparable_data_rec ctxt ~loc:() mode ty v
  in
  Lwt.return (account_for_future_serialization_cost unparsed_data ctxt)
