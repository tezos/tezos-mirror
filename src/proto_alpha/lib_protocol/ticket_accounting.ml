(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

type error += Invalid_ticket_transfer of {ticketer : string; amount : Z.t}

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"invalid_ticket_transfer"
    ~title:"Invalid ticket transfer"
    ~description:"Invalid ticket transfer detected in ticket balance update."
    ~pp:(fun ppf (ticketer, amount) ->
      Format.fprintf
        ppf
        "Attempted to send %a unit(s) of a ticket created by %s."
        Z.pp_print
        amount
        ticketer)
    (obj2 (req "ticketer" string) (req "amount" z))
    (function
      | Invalid_ticket_transfer {ticketer; amount} -> Some (ticketer, amount)
      | _ -> None)
    (fun (ticketer, amount) -> Invalid_ticket_transfer {ticketer; amount})

module Ticket_token_map = struct
  include Ticket_token_map

  let balance_diff ctxt token map =
    Ticket_token_map.find ctxt token map >|=? fun (amnt_opt, ctxt) ->
    (Option.value ~default:Z.zero amnt_opt, ctxt)

  let merge_overlap ctxt b1 b2 =
    Gas.consume ctxt (Ticket_costs.add_z_cost b1 b2) >|? fun ctxt ->
    (Z.add b1 b2, ctxt)

  let of_list ctxt token_amounts =
    Ticket_token_map.of_list ctxt ~merge_overlap token_amounts

  let add ctxt = Ticket_token_map.merge ctxt ~merge_overlap

  let sub ctxt m1 m2 =
    map
      ctxt
      (fun ctxt _ amount ->
        Gas.consume ctxt (Ticket_costs.negate_cost amount) >|? fun ctxt ->
        (Z.neg amount, ctxt))
      m2
    >>? fun (m2, ctxt) -> add ctxt m1 m2
end

let ticket_balances_of_value ctxt ~include_lazy ty value =
  Ticket_scanner.tickets_of_value ~include_lazy ctxt ty value
  >>=? fun (tickets, ctxt) ->
  List.fold_left_e
    (fun (acc, ctxt) ticket ->
      let token, amount = Ticket_token.token_and_amount_of_ex_ticket ticket in
      Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
      >|? fun ctxt -> ((token, Script_int.to_zint amount) :: acc, ctxt))
    ([], ctxt)
    tickets
  >>?= fun (list, ctxt) -> Ticket_token_map.of_list ctxt list

let update_ticket_balances ctxt ~total_storage_diff token destinations =
  List.fold_left_es
    (fun (tot_storage_diff, ctxt) (owner, delta) ->
      Ticket_balance_key.of_ex_token ctxt ~owner token
      >>=? fun (key_hash, ctxt) ->
      Ticket_balance.adjust_balance ctxt key_hash ~delta
      >>=? fun (storage_diff, ctxt) ->
      Gas.consume ctxt (Ticket_costs.add_z_cost total_storage_diff storage_diff)
      >>?= fun ctxt -> return (Z.add tot_storage_diff storage_diff, ctxt))
    (total_storage_diff, ctxt)
    destinations

let invalid_ticket_transfer_error
    ~ticket_token:
      (Ticket_token.Ex_token {ticketer; contents_type = _; contents = _})
    ~amount =
  Invalid_ticket_transfer {ticketer = Contract.to_b58check ticketer; amount}

let update_ticket_balances_for_self_contract ctxt ~self ticket_diffs =
  List.fold_left_es
    (fun (total_storage_diff, ctxt) (ticket_token, amount) ->
      (* Diff is valid iff either:
         - the balance has decreased (delta <= 0), or
         - the ticket-token was created by the [self] contract. *)
      let is_valid_balance_update =
        let (Ticket_token.Ex_token {ticketer; _}) = ticket_token in
        Compare.Z.(amount <= Z.zero) || Contract.equal ticketer self
      in
      error_unless
        is_valid_balance_update
        (invalid_ticket_transfer_error ~ticket_token ~amount)
      >>?= fun () ->
      update_ticket_balances
        ctxt
        ~total_storage_diff
        ticket_token
        [(Destination.Contract self, amount)])
    (Z.zero, ctxt)
    ticket_diffs

let ticket_diffs_of_lazy_storage_diff ctxt ~storage_type_has_tickets
    lazy_storage_diff =
  (* Only scan lazy-diffs for tickets in case the storage contains tickets. *)
  if Ticket_scanner.has_tickets storage_type_has_tickets then
    Ticket_lazy_storage_diff.ticket_diffs_of_lazy_storage_diff
      ctxt
      lazy_storage_diff
    >>=? fun (diffs, ctxt) -> Ticket_token_map.of_list ctxt diffs
  else return (Ticket_token_map.empty, ctxt)

(* TODO #2465
   Move the docs from HackMd to [docs/alpha] folder.
   The documentation referenced here should be moved to a permanent place and
   the comment below should be updated.
*)

(** Description here:
    https://hackmd.io/lutm_5JNRVW-nNFSFkCXLQ?view#Implementation

   - [old_storage_strict] the amount S_1^{strict} of ticket-tokens in the strict part of
     the old storage.

   - [new_storage_strict] the amount S_2^{strict} of ticket-tokens in the strict part of the
      new storage.

   - [lazy_storage_diff] the amount S_{\delta}^{lazy} of ticket-tokens added to the lazy part of
      the storage.

   - [arg_tickets] the amount I of ticket-tokens contained in the incoming
     arguments.

    Calculating the spending budget:
     - additions = new_storage_strict + lazy_storage_diff
     - subtractions = old_storage_strict + arg_tickets
     - delta = additions - subtractions
 *)
let ticket_diffs ctxt ~arg_type_has_tickets ~storage_type_has_tickets ~arg
    ~old_storage ~new_storage ~lazy_storage_diff =
  (* Collect ticket-token balances of the incoming parameters. *)
  ticket_balances_of_value ctxt ~include_lazy:true arg_type_has_tickets arg
  >>=? fun (arg_tickets, ctxt) ->
  ticket_diffs_of_lazy_storage_diff
    ctxt
    ~storage_type_has_tickets
    lazy_storage_diff
  >>=? fun (lazy_storage_diff, ctxt) ->
  ticket_balances_of_value
    ctxt
    ~include_lazy:false
    storage_type_has_tickets
    old_storage
  >>=? fun (old_storage_strict, ctxt) ->
  ticket_balances_of_value
    ctxt
    ~include_lazy:false
    storage_type_has_tickets
    new_storage
  >>=? fun (new_storage_strict, ctxt) ->
  (* Subtractions *)
  Ticket_token_map.add ctxt old_storage_strict arg_tickets
  >>?= fun (subtractions, ctxt) ->
  (* Additions *)
  Ticket_token_map.add ctxt new_storage_strict lazy_storage_diff
  >>?= fun (additions, ctxt) ->
  Lwt.return (Ticket_token_map.sub ctxt additions subtractions)

let update_ticket_balances ctxt ~self ~ticket_diffs operations =
  let validate_spending_budget ctxt
      (Ticket_token.Ex_token {ticketer; _} as ticket_token) amount =
    if Contract.equal ticketer self then
      (* It's okay to send any amount of ticket-tokens minted by the current
         contract (self). Hence tickets stored by their ticketer are not
         stored in the ticket table and don't need to be updated here. *)
      return (true, ctxt)
    else
      Ticket_token_map.balance_diff ctxt ticket_token ticket_diffs
      >|=? fun (balance_diff, ctxt) ->
      (* The balance-diff represents the number of units of a ticket-token,
         that is changed for the [self] contract. A negative diff means that
         an amount of ticket-tokens were not saved in the storage and are
         eligible for transfer to another contract.

         For example, if 5 units of a ticket-token "Alice Red" were pulled from
         the storage, the corresponding diff is -5. That means at most 5 units
         of "Alice Red" can be transferred. Any amount exceeding that would
         result in a validation error.
      *)
      (Compare.Z.(Script_int.to_zint amount <= Z.neg balance_diff), ctxt)
  in
  (* Collect diffs from operations *)
  Ticket_operations_diff.ticket_diffs_of_operations ctxt operations
  >>=? fun (ticket_op_diffs, ctxt) ->
  (* Update balances for self-contract. *)
  Ticket_token_map.to_list ctxt ticket_diffs >>?= fun (ticket_diffs, ctxt) ->
  update_ticket_balances_for_self_contract ctxt ~self ticket_diffs
  >>=? fun (total_storage_diff, ctxt) ->
  (* Update balances for operations. *)
  List.fold_left_es
    (fun (total_storage_diff, ctxt)
         {Ticket_operations_diff.ticket_token; total_amount; destinations} ->
      (* Verify that we are able to spend the given amount of ticket-tokens. *)
      validate_spending_budget ctxt ticket_token total_amount
      >>=? fun (is_valid_balance_update, ctxt) ->
      error_unless
        is_valid_balance_update
        (invalid_ticket_transfer_error
           ~ticket_token
           ~amount:(Script_int.to_zint total_amount))
      >>?= fun () ->
      List.fold_left_e
        (fun (acc, ctxt) (token, amount) ->
          (* Consume some gas for for traversing the list. *)
          Gas.consume ctxt Ticket_costs.Constants.cost_collect_tickets_step
          >|? fun ctxt -> ((token, Script_int.to_zint amount) :: acc, ctxt))
        ([], ctxt)
        destinations
      >>?= fun (destinations, ctxt) ->
      update_ticket_balances ctxt ~total_storage_diff ticket_token destinations)
    (total_storage_diff, ctxt)
    ticket_op_diffs
