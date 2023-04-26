(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type error +=
  | (* Permanent *) Unsupported_non_empty_overlay
  | (* Permanent *) Unsupported_type_operation

let () =
  register_error_kind
    `Branch
    ~id:"Unsupported_non_empty_overlay"
    ~title:"Unsupported non empty overlay"
    ~description:"Unsupported big-map value with non-empty overlay"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Unsupported big-map value with non-empty overlay")
    Data_encoding.empty
    (function Unsupported_non_empty_overlay -> Some () | _ -> None)
    (fun () -> Unsupported_non_empty_overlay) ;
  register_error_kind
    `Branch
    ~id:"Unsupported_type_operation"
    ~title:"Unsupported type operation"
    ~description:"Types embedding operations are not supported"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Types embedding operations are not supported")
    Data_encoding.empty
    (function Unsupported_type_operation -> Some () | _ -> None)
    (fun () -> Unsupported_type_operation)

type ex_ticket =
  | Ex_ticket :
      'a Script_typed_ir.comparable_ty * 'a Script_typed_ir.ticket
      -> ex_ticket

module Ticket_inspection = struct
  (* TODO: 1951
     Replace with use of meta-data for ['a ty] type.
     Once ['a ty] values can be extended with custom meta data, this type
     can be removed.
  *)
  (**
      Witness flag for whether a type can be populated by a value containing a
      ticket. [False_ht] must be used only when a value of the type cannot
      contain a ticket.

      This flag is necessary for avoiding ticket collection (see below) to have
      quadratic complexity in the order of: size-of-the-type * size-of-value.

      This type is local to the [Ticket_scanner] module and should not be
      exported.

  *)
  type 'a has_tickets =
    | True_ht : _ Script_typed_ir.ticket has_tickets
    | False_ht : _ has_tickets
    | Pair_ht :
        'a has_tickets * 'b has_tickets
        -> ('a, 'b) Script_typed_ir.pair has_tickets
    | Or_ht :
        'a has_tickets * 'b has_tickets
        -> ('a, 'b) Script_typed_ir.or_ has_tickets
    | Option_ht : 'a has_tickets -> 'a option has_tickets
    | List_ht : 'a has_tickets -> 'a Script_list.t has_tickets
    | Set_ht : 'k has_tickets -> 'k Script_typed_ir.set has_tickets
    | Map_ht :
        'k has_tickets * 'v has_tickets
        -> ('k, 'v) Script_typed_ir.map has_tickets
    | Big_map_ht :
        'k has_tickets * 'v has_tickets
        -> ('k, 'v) Script_typed_ir.big_map has_tickets

  (* Returns whether or not a comparable type embeds tickets. Currently
     this function returns [false] for all input.

     The only reason we keep this code is so that in the future, if tickets were
     ever to be comparable, the compiler would detect a missing pattern match
     case.

     Note that in case tickets are made comparable, this function needs to change
     so that constructors like [Or_t] and [Pair_t] are traversed
     recursively.
  *)
  let has_tickets_of_comparable :
      type a ret.
      a Script_typed_ir.comparable_ty -> (a has_tickets -> ret) -> ret =
   fun key_ty k ->
    let open Script_typed_ir in
    match key_ty with
    | Unit_t -> (k [@ocaml.tailcall]) False_ht
    | Never_t -> (k [@ocaml.tailcall]) False_ht
    | Int_t -> (k [@ocaml.tailcall]) False_ht
    | Nat_t -> (k [@ocaml.tailcall]) False_ht
    | Signature_t -> (k [@ocaml.tailcall]) False_ht
    | String_t -> (k [@ocaml.tailcall]) False_ht
    | Bytes_t -> (k [@ocaml.tailcall]) False_ht
    | Mutez_t -> (k [@ocaml.tailcall]) False_ht
    | Bool_t -> (k [@ocaml.tailcall]) False_ht
    | Key_hash_t -> (k [@ocaml.tailcall]) False_ht
    | Key_t -> (k [@ocaml.tailcall]) False_ht
    | Timestamp_t -> (k [@ocaml.tailcall]) False_ht
    | Chain_id_t -> (k [@ocaml.tailcall]) False_ht
    | Address_t -> (k [@ocaml.tailcall]) False_ht
    | Pair_t (_, _, _, YesYes) -> (k [@ocaml.tailcall]) False_ht
    | Or_t (_, _, _, YesYes) -> (k [@ocaml.tailcall]) False_ht
    | Option_t (_, _, Yes) -> (k [@ocaml.tailcall]) False_ht

  (* Short circuit pairing of two [has_tickets] values.
     If neither left nor right branch contains a ticket, [False_ht] is
     returned. *)
  let pair_has_tickets pair ht1 ht2 =
    match (ht1, ht2) with False_ht, False_ht -> False_ht | _ -> pair ht1 ht2

  let map_has_tickets map ht =
    match ht with False_ht -> False_ht | _ -> map ht

  type ('a, 'r) continuation = 'a has_tickets -> 'r tzresult

  (* Creates a [has_tickets] type-witness value from the given ['a ty].
     The returned value matches the given shape of the [ty] value, except
     it collapses whole branches where no types embed tickets to [False_ht].
  *)
  let rec has_tickets_of_ty :
      type a ac ret.
      (a, ac) Script_typed_ir.ty -> (a, ret) continuation -> ret tzresult =
   fun ty k ->
    let open Script_typed_ir in
    match ty with
    | Ticket_t _ -> (k [@ocaml.tailcall]) True_ht
    | Unit_t -> (k [@ocaml.tailcall]) False_ht
    | Int_t -> (k [@ocaml.tailcall]) False_ht
    | Nat_t -> (k [@ocaml.tailcall]) False_ht
    | Signature_t -> (k [@ocaml.tailcall]) False_ht
    | String_t -> (k [@ocaml.tailcall]) False_ht
    | Bytes_t -> (k [@ocaml.tailcall]) False_ht
    | Mutez_t -> (k [@ocaml.tailcall]) False_ht
    | Key_hash_t -> (k [@ocaml.tailcall]) False_ht
    | Key_t -> (k [@ocaml.tailcall]) False_ht
    | Timestamp_t -> (k [@ocaml.tailcall]) False_ht
    | Address_t -> (k [@ocaml.tailcall]) False_ht
    | Bool_t -> (k [@ocaml.tailcall]) False_ht
    | Pair_t (ty1, ty2, _, _) ->
        (has_tickets_of_pair [@ocaml.tailcall])
          ty1
          ty2
          ~pair:(fun ht1 ht2 -> Pair_ht (ht1, ht2))
          k
    | Or_t (ty1, ty2, _, _) ->
        (has_tickets_of_pair [@ocaml.tailcall])
          ty1
          ty2
          ~pair:(fun ht1 ht2 -> Or_ht (ht1, ht2))
          k
    | Lambda_t (_, _, _) ->
        (* As of H, closures cannot contain tickets because APPLY requires
           a packable type and tickets are not packable. *)
        (k [@ocaml.tailcall]) False_ht
    | Option_t (ty, _, _) ->
        (has_tickets_of_ty [@ocaml.tailcall]) ty (fun ht ->
            let opt_hty = map_has_tickets (fun ht -> Option_ht ht) ht in
            (k [@ocaml.tailcall]) opt_hty)
    | List_t (ty, _) ->
        (has_tickets_of_ty [@ocaml.tailcall]) ty (fun ht ->
            let list_hty = map_has_tickets (fun ht -> List_ht ht) ht in
            (k [@ocaml.tailcall]) list_hty)
    | Set_t (key_ty, _) ->
        (has_tickets_of_comparable [@ocaml.tailcall]) key_ty (fun ht ->
            let set_hty = map_has_tickets (fun ht -> Set_ht ht) ht in
            (k [@ocaml.tailcall]) set_hty)
    | Map_t (key_ty, val_ty, _) ->
        (has_tickets_of_key_and_value [@ocaml.tailcall])
          key_ty
          val_ty
          ~pair:(fun ht1 ht2 -> Map_ht (ht1, ht2))
          k
    | Big_map_t (key_ty, val_ty, _) ->
        (has_tickets_of_key_and_value [@ocaml.tailcall])
          key_ty
          val_ty
          ~pair:(fun ht1 ht2 -> Big_map_ht (ht1, ht2))
          k
    | Contract_t _ -> (k [@ocaml.tailcall]) False_ht
    | Sapling_transaction_t _ -> (k [@ocaml.tailcall]) False_ht
    | Sapling_transaction_deprecated_t _ -> (k [@ocaml.tailcall]) False_ht
    | Sapling_state_t _ -> (k [@ocaml.tailcall]) False_ht
    | Operation_t ->
        (* Operations may contain tickets but they should never be passed
           why we fail in this case. *)
        error Unsupported_type_operation
    | Chain_id_t -> (k [@ocaml.tailcall]) False_ht
    | Never_t -> (k [@ocaml.tailcall]) False_ht
    | Bls12_381_g1_t -> (k [@ocaml.tailcall]) False_ht
    | Bls12_381_g2_t -> (k [@ocaml.tailcall]) False_ht
    | Bls12_381_fr_t -> (k [@ocaml.tailcall]) False_ht
    | Chest_t -> (k [@ocaml.tailcall]) False_ht
    | Chest_key_t -> (k [@ocaml.tailcall]) False_ht

  and has_tickets_of_pair :
      type a ac b bc c ret.
      (a, ac) Script_typed_ir.ty ->
      (b, bc) Script_typed_ir.ty ->
      pair:(a has_tickets -> b has_tickets -> c has_tickets) ->
      (c, ret) continuation ->
      ret tzresult =
   fun ty1 ty2 ~pair k ->
    (has_tickets_of_ty [@ocaml.tailcall]) ty1 (fun ht1 ->
        (has_tickets_of_ty [@ocaml.tailcall]) ty2 (fun ht2 ->
            (k [@ocaml.tailcall]) (pair_has_tickets pair ht1 ht2)))

  and has_tickets_of_key_and_value :
      type k v vc t ret.
      k Script_typed_ir.comparable_ty ->
      (v, vc) Script_typed_ir.ty ->
      pair:(k has_tickets -> v has_tickets -> t has_tickets) ->
      (t, ret) continuation ->
      ret tzresult =
   fun key_ty val_ty ~pair k ->
    (has_tickets_of_comparable [@ocaml.tailcall]) key_ty (fun ht1 ->
        (has_tickets_of_ty [@ocaml.tailcall]) val_ty (fun ht2 ->
            (k [@ocaml.tailcall]) (pair_has_tickets pair ht1 ht2)))

  let has_tickets_of_ty ctxt ty =
    Gas.consume ctxt (Ticket_costs.has_tickets_of_ty_cost ty) >>? fun ctxt ->
    has_tickets_of_ty ty ok >|? fun ht -> (ht, ctxt)
end

module Ticket_collection = struct
  let consume_gas_steps =
    Ticket_costs.consume_gas_steps
      ~step_cost:Ticket_costs.Constants.cost_collect_tickets_step

  type accumulator = ex_ticket list

  type 'a continuation = context -> accumulator -> 'a tzresult Lwt.t

  (* Currently this always returns the original list.

     If comparables are ever extended to support tickets, this function
     needs to be modified. In particular constructors like [Option] and [Pair]
     would have to recurse on their arguments. *)

  let tickets_of_comparable :
      type a ret.
      context ->
      a Script_typed_ir.comparable_ty ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ctxt comp_ty acc k ->
    let open Script_typed_ir in
    match comp_ty with
    | Unit_t -> (k [@ocaml.tailcall]) ctxt acc
    | Never_t -> (k [@ocaml.tailcall]) ctxt acc
    | Int_t -> (k [@ocaml.tailcall]) ctxt acc
    | Nat_t -> (k [@ocaml.tailcall]) ctxt acc
    | Signature_t -> (k [@ocaml.tailcall]) ctxt acc
    | String_t -> (k [@ocaml.tailcall]) ctxt acc
    | Bytes_t -> (k [@ocaml.tailcall]) ctxt acc
    | Mutez_t -> (k [@ocaml.tailcall]) ctxt acc
    | Bool_t -> (k [@ocaml.tailcall]) ctxt acc
    | Key_hash_t -> (k [@ocaml.tailcall]) ctxt acc
    | Key_t -> (k [@ocaml.tailcall]) ctxt acc
    | Timestamp_t -> (k [@ocaml.tailcall]) ctxt acc
    | Chain_id_t -> (k [@ocaml.tailcall]) ctxt acc
    | Address_t -> (k [@ocaml.tailcall]) ctxt acc
    | Pair_t (_, _, _, YesYes) -> (k [@ocaml.tailcall]) ctxt acc
    | Or_t (_, _, _, YesYes) -> (k [@ocaml.tailcall]) ctxt acc
    | Option_t (_, _, Yes) -> (k [@ocaml.tailcall]) ctxt acc

  let tickets_of_set :
      type a ret.
      context ->
      a Script_typed_ir.comparable_ty ->
      a Script_typed_ir.set ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ctxt key_ty _set acc k ->
    consume_gas_steps ctxt ~num_steps:1 >>?= fun ctxt ->
    (* This is only invoked to support any future extensions making tickets
       comparable. *)
    (tickets_of_comparable [@ocaml.tailcall]) ctxt key_ty acc k

  let rec tickets_of_value :
      type a ac ret.
      include_lazy:bool ->
      context ->
      a Ticket_inspection.has_tickets ->
      (a, ac) Script_typed_ir.ty ->
      a ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ~include_lazy ctxt hty ty x acc k ->
    let open Script_typed_ir in
    consume_gas_steps ctxt ~num_steps:1 >>?= fun ctxt ->
    match (hty, ty) with
    | False_ht, _ -> (k [@ocaml.tailcall]) ctxt acc
    | Pair_ht (hty1, hty2), Pair_t (ty1, ty2, _, _) ->
        let l, r = x in
        (tickets_of_value [@ocaml.tailcall])
          ~include_lazy
          ctxt
          hty1
          ty1
          l
          acc
          (fun ctxt acc ->
            (tickets_of_value [@ocaml.tailcall])
              ~include_lazy
              ctxt
              hty2
              ty2
              r
              acc
              k)
    | Or_ht (htyl, htyr), Or_t (tyl, tyr, _, _) -> (
        match x with
        | L v ->
            (tickets_of_value [@ocaml.tailcall])
              ~include_lazy
              ctxt
              htyl
              tyl
              v
              acc
              k
        | R v ->
            (tickets_of_value [@ocaml.tailcall])
              ~include_lazy
              ctxt
              htyr
              tyr
              v
              acc
              k)
    | Option_ht el_hty, Option_t (el_ty, _, _) -> (
        match x with
        | Some x ->
            (tickets_of_value [@ocaml.tailcall])
              ~include_lazy
              ctxt
              el_hty
              el_ty
              x
              acc
              k
        | None -> (k [@ocaml.tailcall]) ctxt acc)
    | List_ht el_hty, List_t (el_ty, _) ->
        let elements = Script_list.to_list x in
        (tickets_of_list [@ocaml.tailcall])
          ctxt
          ~include_lazy
          el_hty
          el_ty
          elements
          acc
          k
    | Set_ht _, Set_t (key_ty, _) ->
        (tickets_of_set [@ocaml.tailcall]) ctxt key_ty x acc k
    | Map_ht (_, val_hty), Map_t (key_ty, val_ty, _) ->
        (tickets_of_comparable [@ocaml.tailcall])
          ctxt
          key_ty
          acc
          (fun ctxt acc ->
            (tickets_of_map [@ocaml.tailcall])
              ctxt
              ~include_lazy
              val_hty
              val_ty
              x
              acc
              k)
    | Big_map_ht (_, val_hty), Big_map_t (key_ty, _, _) ->
        if include_lazy then
          (tickets_of_big_map [@ocaml.tailcall]) ctxt val_hty key_ty x acc k
        else (k [@ocaml.tailcall]) ctxt acc
    | True_ht, Ticket_t (comp_ty, _) ->
        (k [@ocaml.tailcall]) ctxt (Ex_ticket (comp_ty, x) :: acc)

  and tickets_of_list :
      type a ac ret.
      context ->
      include_lazy:bool ->
      a Ticket_inspection.has_tickets ->
      (a, ac) Script_typed_ir.ty ->
      a list ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ctxt ~include_lazy el_hty el_ty elements acc k ->
    consume_gas_steps ctxt ~num_steps:1 >>?= fun ctxt ->
    match elements with
    | elem :: elems ->
        (tickets_of_value [@ocaml.tailcall])
          ~include_lazy
          ctxt
          el_hty
          el_ty
          elem
          acc
          (fun ctxt acc ->
            (tickets_of_list [@ocaml.tailcall])
              ~include_lazy
              ctxt
              el_hty
              el_ty
              elems
              acc
              k)
    | [] -> (k [@ocaml.tailcall]) ctxt acc

  and tickets_of_map :
      type k v vc ret.
      include_lazy:bool ->
      context ->
      v Ticket_inspection.has_tickets ->
      (v, vc) Script_typed_ir.ty ->
      (k, v) Script_typed_ir.map ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ~include_lazy ctxt val_hty val_ty map acc k ->
    let (module M) = Script_map.get_module map in
    consume_gas_steps ctxt ~num_steps:1 >>?= fun ctxt ->
    (* Pay gas for folding over the values *)
    consume_gas_steps ctxt ~num_steps:M.size >>?= fun ctxt ->
    let values = M.OPS.fold (fun _ v vs -> v :: vs) M.boxed [] in
    (tickets_of_list [@ocaml.tailcall])
      ~include_lazy
      ctxt
      val_hty
      val_ty
      values
      acc
      k

  and tickets_of_big_map :
      type k v ret.
      context ->
      v Ticket_inspection.has_tickets ->
      k Script_typed_ir.comparable_ty ->
      (k, v) Script_typed_ir.big_map ->
      accumulator ->
      ret continuation ->
      ret tzresult Lwt.t =
   fun ctxt
       val_hty
       key_ty
       (Big_map {id; diff = {map = _; size}; key_type = _; value_type})
       acc
       k ->
    consume_gas_steps ctxt ~num_steps:1 >>?= fun ctxt ->
    (* Require empty overlay *)
    if Compare.Int.(size > 0) then tzfail Unsupported_non_empty_overlay
    else
      (* Traverse the keys for tickets, although currently keys should never
         contain any tickets. *)
      (tickets_of_comparable [@ocaml.tailcall]) ctxt key_ty acc (fun ctxt acc ->
          (* Accumulate tickets from values of the big-map stored in the context *)
          match id with
          | Some id ->
              let accum (values, ctxt) (_key_hash, exp) =
                Script_ir_translator.parse_data
                  ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
                  ctxt
                  ~allow_forged:true
                  value_type
                  (Micheline.root exp)
                >|=? fun (v, ctxt) -> (v :: values, ctxt)
              in
              Big_map.list_key_values ctxt id >>=? fun (ctxt, exps) ->
              List.fold_left_es accum ([], ctxt) exps >>=? fun (values, ctxt) ->
              (tickets_of_list [@ocaml.tailcall])
                ~include_lazy:true
                ctxt
                val_hty
                value_type
                values
                acc
                k
          | None -> (k [@ocaml.tailcall]) ctxt acc)

  let tickets_of_value ctxt ~include_lazy ht ty x =
    tickets_of_value ctxt ~include_lazy ht ty x [] (fun ctxt ex_tickets ->
        return (ex_tickets, ctxt))
end

type 'a has_tickets =
  | Has_tickets :
      'a Ticket_inspection.has_tickets * ('a, _) Script_typed_ir.ty
      -> 'a has_tickets

let type_has_tickets ctxt ty =
  Ticket_inspection.has_tickets_of_ty ctxt ty >|? fun (has_tickets, ctxt) ->
  (Has_tickets (has_tickets, ty), ctxt)

let tickets_of_value ctxt ~include_lazy (Has_tickets (ht, ty)) =
  Ticket_collection.tickets_of_value ctxt ~include_lazy ht ty

let has_tickets (Has_tickets (ht, _)) =
  match ht with Ticket_inspection.False_ht -> false | _ -> true

let tickets_of_node ctxt ~include_lazy has_tickets expr =
  let (Has_tickets (ht, ty)) = has_tickets in
  match ht with
  | Ticket_inspection.False_ht -> return ([], ctxt)
  | _ ->
      Script_ir_translator.parse_data
        ctxt
        ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
        ~allow_forged:true
        ty
        expr
      >>=? fun (value, ctxt) ->
      tickets_of_value ctxt ~include_lazy has_tickets value

let ex_ticket_size ctxt (Ex_ticket (ty, ticket)) =
  (* type *)
  Script_typed_ir.ticket_t Micheline.dummy_location ty >>?= fun ty ->
  Script_ir_unparser.unparse_ty ~loc:() ctxt ty >>?= fun (ty', ctxt) ->
  let ty_nodes, ty_size = Script_typed_ir_size.node_size ty' in
  let ty_size_cost = Script_typed_ir_size_costs.nodes_cost ~nodes:ty_nodes in
  Gas.consume ctxt ty_size_cost >>?= fun ctxt ->
  (* contents *)
  let val_nodes, val_size = Script_typed_ir_size.value_size ty ticket in
  let val_size_cost = Script_typed_ir_size_costs.nodes_cost ~nodes:val_nodes in
  Gas.consume ctxt val_size_cost >>?= fun ctxt ->
  (* gas *)
  return (Saturation_repr.add ty_size val_size, ctxt)

let ex_token_and_amount_of_ex_ticket
    (Ex_ticket (contents_type, {Script_typed_ir.ticketer; contents; amount})) =
  (Ticket_token.Ex_token {ticketer; contents_type; contents}, amount)
