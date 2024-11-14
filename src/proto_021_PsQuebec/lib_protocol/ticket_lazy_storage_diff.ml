(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

type error += Failed_to_load_big_map_value_type of Big_map.Id.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"Failed_to_load_big_map_value_type"
    ~title:"Failed to load big-map value type"
    ~description:
      "Failed to load big-map value type when computing ticket diffs."
    ~pp:(fun ppf big_map_id ->
      Format.fprintf
        ppf
        "Failed to load big-map value type for big-map-id: '%a'"
        Z.pp_print
        (Big_map.Id.unparse_to_z big_map_id))
    (obj1 (req "big_map_id" Big_map.Id.encoding))
    (function
      | Failed_to_load_big_map_value_type big_map_id -> Some big_map_id
      | _ -> None)
    (fun big_map_id -> Failed_to_load_big_map_value_type big_map_id)

(** Extracts the ticket-token and amount from an ex_ticket value. *)
let token_and_amount ctxt ex_ticket =
  let open Result_syntax in
  let+ ctxt =
    Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
  in
  let token, amount =
    Ticket_scanner.ex_token_and_amount_of_ex_ticket ex_ticket
  in
  ((token, Script_int.(to_zint (amount :> n num))), ctxt)

(** Extracts the ticket-token and amount from an ex_ticket value and returns
  the opposite of the amount. This is used to account for removal of tickets inside
  big maps when either a ticket is taken out of a big map or a whole big map is
  dropped. *)
let neg_token_and_amount ctxt ex_ticket =
  let open Result_syntax in
  let* (token, amount), ctxt = token_and_amount ctxt ex_ticket in
  let+ ctxt = Gas.consume ctxt (Ticket_costs.negate_cost amount) in
  ((token, Z.neg amount), ctxt)

let parse_value_type ctxt value_type =
  Script_ir_translator.parse_big_map_value_ty
    ctxt
    ~legacy:true
    (Micheline.root value_type)

(** Collects all ticket-token balances contained in the given node and prepends
    them to the accumulator [acc]. The given [get_token_and_amount] function
    extracts the ticket-token and amount (either positive or negative) from an
    [ex_ticket] value, depending on whether the diff stems from adding or
    removing a value containing tickets. *)
let collect_token_diffs_of_node ctxt has_tickets node ~get_token_and_amount acc
    =
  let open Lwt_result_syntax in
  let* ex_tickets, ctxt =
    Ticket_scanner.tickets_of_node
      ctxt
      (* It's currently not possible to have nested lazy structures, but this is
         for future proofing. *)
      ~include_lazy:true
      has_tickets
      (Micheline.root node)
  in
  let*? result =
    let open Result_syntax in
    List.fold_left_e
      (fun (acc, ctxt) ticket ->
        let+ item, ctxt = get_token_and_amount ctxt ticket in
        (item :: acc, ctxt))
      (acc, ctxt)
      ex_tickets
  in
  return result

(** A module for keeping track of script-key-hashes. It's used for looking up
    keys for multiple big-map updates referencing the same key.
  *)

module Key_hash_map =
  Carbonated_map.Make
    (struct
      type context = Alpha_context.context

      let consume = Alpha_context.Gas.consume
    end)
    (struct
      type t = Script_expr_hash.t

      let compare = Script_expr_hash.compare

      let compare_cost _ = Ticket_costs.Constants.cost_compare_ticket_hash
    end)

(** Collects all ticket-token diffs from a big-map update and prepends them
    to the accumulator [acc]. *)
let collect_token_diffs_of_big_map_update ctxt ~big_map_id has_tickets
    {Lazy_storage_kind.Big_map.key = _; key_hash; value} already_updated acc =
  let collect_token_diffs_of_node_option ctxt ~get_token_and_amount expr_opt acc
      =
    match expr_opt with
    | Some expr ->
        collect_token_diffs_of_node
          ctxt
          has_tickets
          expr
          ~get_token_and_amount
          acc
    | None -> return (acc, ctxt)
  in
  (* First check if the key-hash has already been updated, in that case pull the
     value from the [already_updated] map. Note that this should not happen with
     the current implementation of big-map overlays as it guarantees that keys
     are unique. The extra check is used for future proofing.
  *)
  let open Lwt_result_syntax in
  let* old_value, ctxt =
    let*? val_opt, ctxt = Key_hash_map.find ctxt key_hash already_updated in
    match val_opt with
    | Some updated_value -> return (updated_value, ctxt)
    | None ->
        (* Load tickets from the old value that was removed. *)
        let+ ctxt, old_value = Big_map.get_opt ctxt big_map_id key_hash in
        (old_value, ctxt)
  in
  let* acc, ctxt =
    collect_token_diffs_of_node_option
      ctxt
      ~get_token_and_amount:neg_token_and_amount
      old_value
      acc
  in
  let*? already_updated, ctxt =
    Key_hash_map.update
      ctxt
      key_hash
      (fun ctxt _ -> Ok (Some value, ctxt))
      already_updated
  in
  (* TODO: #2303
     Avoid re-parsing the value.
     In order to find tickets from the new value, we need to parse it. It would
     be more efficient if the value was already present.
  *)
  let+ tickets, ctxt =
    collect_token_diffs_of_node_option
      ctxt
      ~get_token_and_amount:token_and_amount
      value
      acc
  in
  (tickets, already_updated, ctxt)

(** Collects all ticket-token diffs from a list of big-map updates and prepends
    them to the accumulator [acc]. *)
let collect_token_diffs_of_big_map_updates ctxt big_map_id ~value_type updates
    acc =
  (* TODO: #2303
     Avoid re-parsing the value type.
     We should have the non-serialized version of the value type.
  *)
  let open Lwt_result_syntax in
  let*? Script_typed_ir.Ex_ty value_type, ctxt =
    parse_value_type ctxt value_type
  in
  let*? has_tickets, ctxt = Ticket_scanner.type_has_tickets ctxt value_type in
  let+ acc, _already_updated, ctxt =
    List.fold_left_es
      (fun (acc, already_updated, ctxt) update ->
        collect_token_diffs_of_big_map_update
          ctxt
          ~big_map_id
          has_tickets
          update
          already_updated
          acc)
      (acc, Key_hash_map.empty, ctxt)
      updates
  in
  (acc, ctxt)

(** Given a big-map id, this function collects ticket-token diffs and prepends
    them to the accumulator [acc]. *)
let collect_token_diffs_of_big_map ctxt ~get_token_and_amount big_map_id acc =
  let open Lwt_result_syntax in
  let*? ctxt =
    Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
  in
  let* ctxt, key_val_tys = Big_map.exists ctxt big_map_id in
  match key_val_tys with
  | Some (_key_ty, value_ty) ->
      (* TODO: #2303
         Avoid re-parsing the value type.
         In order to find tickets from the value, we need to parse the value
         type. It would be more efficient if the value preserved.
      *)
      let*? Script_typed_ir.Ex_ty value_type, ctxt =
        parse_value_type ctxt value_ty
      in
      let*? has_tickets, ctxt =
        Ticket_scanner.type_has_tickets ctxt value_type
      in
      (* Iterate over big-map items. *)
      let* ctxt, exprs = Big_map.list_key_values ctxt big_map_id in
      List.fold_left_es
        (fun (acc, ctxt) (_key_hash, node) ->
          collect_token_diffs_of_node
            ctxt
            has_tickets
            node
            ~get_token_and_amount
            acc)
        (acc, ctxt)
        exprs
  | None -> tzfail (Failed_to_load_big_map_value_type big_map_id)

(** Collects ticket-token diffs from a big-map and a list of updates, and
    prepends them to the given accumulator [acc]. *)
let collect_token_diffs_of_big_map_and_updates ctxt big_map_id updates acc =
  let open Lwt_result_syntax in
  let*? ctxt =
    Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
  in
  let* ctxt, key_val_opt = Big_map.exists ctxt big_map_id in
  match key_val_opt with
  | Some (_val, value_type) ->
      collect_token_diffs_of_big_map_updates
        ctxt
        big_map_id
        ~value_type
        updates
        acc
  | None -> tzfail (Failed_to_load_big_map_value_type big_map_id)

(** Inspects the given [Lazy_storage.diffs_item] and prepends all ticket-token
    diffs, resulting from the updates, to the given accumulator [acc]. *)
let collect_token_diffs_of_big_map_diff ctxt diff_item acc =
  let open Lwt_result_syntax in
  let*? ctxt =
    Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
  in
  match diff_item with
  | Lazy_storage.Item (Lazy_storage_kind.Big_map, big_map_id, Remove) ->
      (* Collect all removed tokens from the big-map. *)
      collect_token_diffs_of_big_map
        ctxt
        ~get_token_and_amount:neg_token_and_amount
        big_map_id
        acc
  | Item (Lazy_storage_kind.Big_map, big_map_id, Update {init; updates}) -> (
      match init with
      | Lazy_storage.Existing ->
          (* Collect token diffs from the updates to the big-map. *)
          collect_token_diffs_of_big_map_and_updates ctxt big_map_id updates acc
      | Copy {src} ->
          (* Collect tokens diffs from the source of the copied big-map. *)
          let* acc, ctxt =
            collect_token_diffs_of_big_map
              ctxt
              ~get_token_and_amount:token_and_amount
              src
              acc
          in
          (* Collect token diffs from the updates to the copied big-map. *)
          collect_token_diffs_of_big_map_and_updates ctxt src updates acc
      | Alloc {key_type = _; value_type} ->
          collect_token_diffs_of_big_map_updates
            ctxt
            big_map_id
            ~value_type
            updates
            acc)
  | Item (Sapling_state, _, _) -> return (acc, ctxt)

let ticket_diffs_of_lazy_storage_diff ctxt diffs_items =
  List.fold_left_es
    (fun (acc, ctxt) diff_item ->
      collect_token_diffs_of_big_map_diff ctxt diff_item acc)
    ([], ctxt)
    diffs_items
