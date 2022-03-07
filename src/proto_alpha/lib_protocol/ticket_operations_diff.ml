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
  destinations : (Destination.t * Script_int.n Script_int.num) list;
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
module Destination_map = Carbonated_map.Make (struct
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
  let add ctxt ~ticket_token ~destination ~amount map =
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
              | Some prev_amount ->
                  Gas.consume
                    ctxt
                    (Ticket_costs.add_int_cost prev_amount amount)
                  >|? fun ctxt ->
                  (Some (Script_int.add_n prev_amount amount), ctxt)
              | None -> ok (Some amount, ctxt)
            in
            Destination_map.update ctxt destination update destination_map
            >|? fun (destination_map, ctxt) -> (Some destination_map, ctxt))
      map
end

let parse_and_cache_script ctxt ~destination ~get_non_cached_script =
  Script_cache.find ctxt destination >>=? fun (ctxt, _cache_key, cached) ->
  match cached with
  | Some (_script, ex_script) -> return (ex_script, ctxt)
  | None ->
      get_non_cached_script ctxt >>=? fun (script, ctxt) ->
      Script_ir_translator.parse_script
        ctxt
        ~legacy:true
        ~allow_forged_in_storage:true
        script
      >>=? fun (ex_script, ctxt) ->
      (* Add the parsed script to the script-cache in order to avoid having to
         re-parse when applying the operation at a later stage. *)
      let (size, cost) = Script_ir_translator.script_size ex_script in
      Gas.consume ctxt cost >>?= fun ctxt ->
      Script_cache.insert ctxt destination (script, ex_script) size
      >>?= fun ctxt -> return (ex_script, ctxt)

let cast_transaction_parameter (type a ac b bc) ctxt location
    (entry_arg_ty : (a, ac) Script_typed_ir.ty)
    (parameters_ty : (b, bc) Script_typed_ir.ty) (parameters : b) :
    (a * context) tzresult Lwt.t =
  Gas_monad.run
    ctxt
    (Script_ir_translator.ty_eq
       ~error_details:Informative
       location
       entry_arg_ty
       parameters_ty)
  >>?= fun (res, ctxt) ->
  res >>?= fun Script_ir_translator.Eq -> return ((parameters : a), ctxt)

let tickets_of_transaction ctxt ~destination ~entrypoint ~location
    ~parameters_ty ~parameters =
  match Contract.is_implicit destination with
  | Some _ -> return (None, ctxt)
  | None ->
      (* TODO: #2653
         Avoid having to load the script from the cache.
         This is currently in place to avoid regressions for type-checking
         errors. We should be able to remove it.
      *)
      parse_and_cache_script
        ctxt
        ~destination
        ~get_non_cached_script:(fun ctxt ->
          (* Look up the script from the context. *)
          Contract.get_script ctxt destination >>=? fun (ctxt, script_opt) ->
          match script_opt with
          | None -> fail (Failed_to_get_script destination)
          | Some script -> return (script, ctxt))
      >>=? fun ( Script_ir_translator.Ex_script
                   (Script {arg_type; entrypoints; _}),
                 ctxt ) ->
      (* Find the entrypoint type for the given entrypoint. *)
      Gas_monad.run
        ctxt
        (Script_ir_translator.find_entrypoint
           ~error_details:Informative
           arg_type
           entrypoints
           entrypoint)
      >>?= fun (res, ctxt) ->
      res >>?= fun (Ex_ty_cstr (entry_arg_ty, _f)) ->
      Ticket_scanner.type_has_tickets ctxt entry_arg_ty
      >>?= fun (has_tickets, ctxt) ->
      (* Check that the parameter's type matches that of the entry-point, and
         cast the parameter if this is the case. *)
      cast_transaction_parameter
        ctxt
        location
        entry_arg_ty
        parameters_ty
        parameters
      >>=? fun (parameters, ctxt) ->
      Ticket_scanner.tickets_of_value
        ~include_lazy:true
        ctxt
        has_tickets
        parameters
      >>=? fun (tickets, ctxt) ->
      return (Some {destination = Contract destination; tickets}, ctxt)

(** Extract tickets of an origination operation by scanning the storage. *)
let tickets_of_origination ctxt ~preorigination
    (Script_typed_ir.Script
      {
        storage_type;
        storage;
        code = _;
        arg_type = _;
        views = _;
        entrypoints = _;
        code_size = _;
      }) =
  (* Extract any tickets from the storage. Note that if the type of the contract
     storage does not contain tickets, storage is not scanned. *)
  Ticket_scanner.type_has_tickets ctxt storage_type
  >>?= fun (has_tickets, ctxt) ->
  Ticket_scanner.tickets_of_value ctxt ~include_lazy:true has_tickets storage
  >|=? fun (tickets, ctxt) ->
  (Some {tickets; destination = Destination.Contract preorigination}, ctxt)

let tickets_of_operation ctxt
    (Script_typed_ir.Internal_operation {source = _; operation; nonce = _}) =
  match operation with
  | Transaction
      {
        transaction =
          {
            amount = _;
            parameters = _;
            entrypoint;
            destination = Destination.Contract destination;
          };
        location;
        parameters_ty;
        parameters;
      } ->
      tickets_of_transaction
        ctxt
        ~destination
        ~entrypoint
        ~location
        ~parameters_ty
        ~parameters
  | Transaction
      {
        transaction =
          {
            destination = Destination.Tx_rollup tx_rollup_dest;
            parameters = _;
            entrypoint;
            amount = _;
          };
        location = _;
        parameters_ty;
        parameters;
      } ->
      if Entrypoint.(entrypoint = Tx_rollup.deposit_entrypoint) then
        Tx_rollup_parameters.get_deposit_parameters parameters_ty parameters
        >>?= fun {ex_ticket; l2_destination = _} ->
        return
          ( Some
              {
                destination = Destination.Tx_rollup tx_rollup_dest;
                tickets = [ex_ticket];
              },
            ctxt )
      else return (None, ctxt)
  | Origination
      {
        origination = {delegate = _; script = _; credit = _};
        preorigination;
        script;
      } ->
      tickets_of_origination ctxt ~preorigination script
  | Delegation _ -> return (None, ctxt)

let add_transfer_to_token_map ctxt token_map {destination; tickets} =
  List.fold_left_es
    (fun (token_map, ctxt) ticket ->
      let (ticket_token, amount) =
        Ticket_token.token_and_amount_of_ex_ticket ticket
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
  Ticket_token_map.fold
    ctxt
    (fun ctxt acc ticket_token destination_map ->
      (* Calculate the total amount of outgoing units for the current
         ticket-token. *)
      Destination_map.fold
        ctxt
        (fun ctxt total_amount _destination amount ->
          Gas.consume ctxt (Ticket_costs.add_int_cost total_amount amount)
          >|? fun ctxt -> (Script_int.add_n total_amount amount, ctxt))
        Script_int.zero_n
        destination_map
      >>? fun (total_amount, ctxt) ->
      Destination_map.to_list ctxt destination_map
      >|? fun (destinations, ctxt) ->
      ({ticket_token; total_amount; destinations} :: acc, ctxt))
    []
    token_map
  |> Lwt.return
