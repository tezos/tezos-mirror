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

type ticket_transfer = {
  destination : Destination.t;
  tickets : Ticket_scanner.ex_ticket list;
}

type ticket_token_diff = {
  ticket_token : Ticket_token.ex_token;
  total_amount : Script_int.n Script_int.num;
  destinations : (Destination.t * Ticket_amount.t) list;
}

type error += Failed_to_get_script of Contract.t | Contract_not_originated

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"Failed_to_get_script"
    ~title:"Failed to get script for contract"
    ~description:
      "Failed to get script for contract when scanning operations for tickets"
    ~pp:(fun ppf contract ->
      Format.fprintf
        ppf
        "Failed to get script for contract %a"
        Contract.pp
        contract)
    (obj1 (req "contract" Contract.encoding))
    (function Failed_to_get_script c -> Some c | _ -> None)
    (fun c -> Failed_to_get_script c) ;
  register_error_kind
    `Permanent
    ~id:"contract_not_originated"
    ~title:"Contract not originated"
    ~description:"Non originated contract detected in ticket update."
    ~pp:(fun ppf () -> Format.fprintf ppf "Contract was not pre-originated")
    unit
    (function Contract_not_originated -> Some () | _ -> None)
    (fun () -> Contract_not_originated)

(** A carbonated map where the keys are destination (contract or tx_rollup). *)
module Destination_map =
  Carbonated_map.Make
    (struct
      type context = Alpha_context.context

      let consume = Alpha_context.Gas.consume
    end)
    (struct
      type t = Destination.t

      let compare = Destination.compare

      (* TODO: #2667
         Change cost-function to one for comparing destinations.
         Not expected to have any performance impact but we should update for
         completeness.
      *)
      let compare_cost _ = Ticket_costs.Constants.cost_compare_key_contract
    end)

(** A module for mapping ticket-tokens to a map of contract destinations and
    amounts. The values specify how to distribute the spending of a ticket-token
    across different contracts.

    In the example below, there is a total of 4 Token1 ticket-tokens
    transferred: three units are sent to contract K1 and one unit to K2.
    Additionally, there are 12 units of Token2 sent to K2, K7 and K8. And one
    unit of Token3 sent to K1.
      {
          Token1 -> { K1 -> 3, K2 -> 1 }
          Token2 -> { K2 -> 1, K7 -> 10, K8 -> 1}
          Token3 -> { K1 -> 1 }
      }
*)
module Ticket_token_map = struct
  include Ticket_token_map

  (** Adds a ticket-token with a destination and an amount to the map.
      The layout of the map parameter is as described above. Its type is:

       (n num Destination_map.t) Ticket_token_map.t

      As explained above, the inner map expresses a list of destination
      contracts and outgoing amount pairs.

      Invariant:
        - The internal contract-indexed map cannot be empty.

   *)
  let add ctxt ~ticket_token ~destination ~(amount : Ticket_amount.t) map =
    Ticket_token_map.update
      ctxt
      ticket_token
      (fun ctxt old_val ->
        match old_val with
        | None ->
            (* Create a new map with a single contract-and amount pair. *)
            let map = Destination_map.singleton destination amount in
            ok (Some map, ctxt)
        | Some destination_map ->
            (* Update the inner contract map *)
            let update ctxt prev_amt_opt =
              match prev_amt_opt with
              | Some (prev_amount : Ticket_amount.t) ->
                  Gas.consume
                    ctxt
                    Script_int.(
                      Ticket_costs.add_int_cost
                        (prev_amount :> n num)
                        (amount :> n num))
                  >|? fun ctxt ->
                  (Some (Ticket_amount.add prev_amount amount), ctxt)
              | None -> ok (Some amount, ctxt)
            in
            Destination_map.update ctxt destination update destination_map
            >|? fun (destination_map, ctxt) -> (Some destination_map, ctxt))
      map
end

let tickets_of_transaction ctxt ~destination ~parameters_ty ~parameters =
  Ticket_scanner.type_has_tickets ctxt parameters_ty
  >>?= fun (has_tickets, ctxt) ->
  Ticket_scanner.tickets_of_value ~include_lazy:true ctxt has_tickets parameters
  >>=? fun (tickets, ctxt) -> return (Some {destination; tickets}, ctxt)

(** Extract tickets of an origination operation by scanning the storage. *)
let tickets_of_origination ctxt ~preorigination ~storage_type ~storage =
  (* Extract any tickets from the storage. Note that if the type of the contract
     storage does not contain tickets, storage is not scanned. *)
  Ticket_scanner.type_has_tickets ctxt storage_type
  >>?= fun (has_tickets, ctxt) ->
  Ticket_scanner.tickets_of_value ctxt ~include_lazy:true has_tickets storage
  >|=? fun (tickets, ctxt) ->
  let destination = Destination.Contract (Originated preorigination) in
  (Some {tickets; destination}, ctxt)

let tickets_of_operation ctxt
    (Script_typed_ir.Internal_operation {sender = _; operation; nonce = _}) =
  match operation with
  | Transaction_to_implicit _ -> return (None, ctxt)
  | Transaction_to_implicit_with_ticket
      {
        destination;
        ticket;
        ticket_ty = Script_typed_ir.Ticket_t (ty, _);
        unparsed_ticket = _;
        amount = _;
      } ->
      return
        ( Some
            {
              destination = Destination.Contract (Implicit destination);
              tickets = [Ex_ticket (ty, ticket)];
            },
          ctxt )
  | Transaction_to_smart_contract
      {
        amount = _;
        unparsed_parameters = _;
        entrypoint = _;
        destination;
        location = _;
        parameters_ty;
        parameters;
      } ->
      tickets_of_transaction
        ctxt
        ~destination:(Destination.Contract (Originated destination))
        ~parameters_ty
        ~parameters
  | Transaction_to_sc_rollup
      {
        destination;
        entrypoint = _;
        parameters_ty;
        parameters;
        unparsed_parameters = _;
      } ->
      (* Note that zero-amount tickets to a rollup is not permitted. *)
      tickets_of_transaction
        ctxt
        ~destination:(Destination.Sc_rollup destination)
        ~parameters_ty
        ~parameters
  | Transaction_to_zk_rollup
      {
        destination;
        unparsed_parameters = _;
        parameters_ty = Pair_t (Ticket_t (ty, _), Bytes_t, _, _);
        parameters = ticket, _op;
      } ->
      let ex_ticket = Ticket_scanner.Ex_ticket (ty, ticket) in
      return
        ( Some
            {
              destination = Destination.Zk_rollup destination;
              tickets = [ex_ticket];
            },
          ctxt )
  | Origination
      {
        delegate = _;
        code = _;
        unparsed_storage = _;
        credit = _;
        preorigination;
        storage_type;
        storage;
      } ->
      tickets_of_origination ctxt ~preorigination ~storage_type ~storage
  | Delegation _ | Event _ -> return (None, ctxt)

let add_transfer_to_token_map ctxt token_map {destination; tickets} =
  List.fold_left_es
    (fun (token_map, ctxt) ticket ->
      let ticket_token, amount =
        Ticket_scanner.ex_token_and_amount_of_ex_ticket ticket
      in
      Ticket_token_map.add ctxt ~ticket_token ~destination ~amount token_map)
    (token_map, ctxt)
    tickets

let ticket_token_map_of_operations ctxt ops =
  List.fold_left_es
    (fun (token_map, ctxt) op ->
      tickets_of_operation ctxt op >>=? fun (res, ctxt) ->
      match res with
      | Some ticket_trans ->
          add_transfer_to_token_map ctxt token_map ticket_trans
      | None -> return (token_map, ctxt))
    (Ticket_token_map.empty, ctxt)
    ops

(** Traverses a list of operations and scans for tickets. *)
let ticket_diffs_of_operations ctxt operations =
  ticket_token_map_of_operations ctxt operations >>=? fun (token_map, ctxt) ->
  Ticket_token_map.fold_e
    ctxt
    (fun ctxt acc ticket_token destination_map ->
      (* Calculate the total amount of outgoing units for the current
         ticket-token. *)
      Destination_map.fold_e
        ctxt
        (fun ctxt total_amount _destination (amount : Ticket_amount.t) ->
          Gas.consume
            ctxt
            Script_int.(
              Ticket_costs.add_int_cost total_amount (amount :> n num))
          >|? fun ctxt ->
          (Script_int.(add_n total_amount (amount :> n num)), ctxt))
        Script_int.zero_n
        destination_map
      >>? fun (total_amount, ctxt) ->
      Destination_map.to_list ctxt destination_map
      >|? fun (destinations, ctxt) ->
      ({ticket_token; total_amount; destinations} :: acc, ctxt))
    []
    token_map
  |> Lwt.return
