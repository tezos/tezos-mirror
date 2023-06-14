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

(** A carbonated map where the keys are [Ticket_hash.t] values. *)
module Ticket_token_map =
  Carbonated_map.Make
    (struct
      type context = Alpha_context.context

      let consume = Gas.consume
    end)
    (struct
      type t = Ticket_hash.t

      let compare = Ticket_hash.compare

      let compare_cost _ = Ticket_costs.Constants.cost_compare_ticket_hash
    end)

(** Conceptually a map from [Ticket_token.ex_token] to values. Since
    ticket-tokens are expensive to compare we use [Ticket_hash.t] keys instead,
    and store the ticket-token along with the value.  *)
type 'a t = (Ticket_token.ex_token * 'a) Ticket_token_map.t

let empty = Ticket_token_map.empty

let key_of_ticket_token ctxt (Ticket_token.Ex_token {ticketer; _} as token) =
  (* We use the [ticket_balance_key] function for generating a key-hash
     for comparing tokens. Since an owner contract is required we use [ticketer]
     but any dummy value would work as long as it's consistent.
  *)
  Ticket_balance_key.of_ex_token
    ctxt
    ~owner:(Destination.Contract ticketer)
    token

let update ctxt key f m =
  key_of_ticket_token ctxt key >>=? fun (key_hash, ctxt) ->
  let f ctxt val_opt =
    (match val_opt with
    | Some (_tkn, value) -> f ctxt (Some value)
    | None -> f ctxt None)
    >|? fun (val_opt, ctxt) -> (Option.map (fun v -> (key, v)) val_opt, ctxt)
  in
  Ticket_token_map.update ctxt key_hash f m |> Lwt.return

let fold_e ctxt f =
  Ticket_token_map.fold_e ctxt (fun ctxt acc _key_hash (tkn, value) ->
      f ctxt acc tkn value)

let fold_es ctxt f =
  Ticket_token_map.fold_es ctxt (fun ctxt acc _key_hash (tkn, value) ->
      f ctxt acc tkn value)

let find ctxt ticket_token map =
  key_of_ticket_token ctxt ticket_token >>=? fun (key_hash, ctxt) ->
  Ticket_token_map.find ctxt key_hash map >>?= fun (val_opt, ctxt) ->
  return (Option.map snd val_opt, ctxt)

let lift_merge_overlap merge_overlap ctxt (tkn1, v1) (_tkn2, v2) =
  merge_overlap ctxt v1 v2 >|? fun (v, ctxt) -> ((tkn1, v), ctxt)

let of_list ctxt ~merge_overlap token_values =
  List.fold_left_es
    (fun (map, ctxt) (token, value) ->
      key_of_ticket_token ctxt token >>=? fun (key_hash, ctxt) ->
      Lwt.return
        (Ticket_token_map.update
           ctxt
           key_hash
           (fun ctxt old_val ->
             match old_val with
             | None -> ok (Some (token, value), ctxt)
             | Some old ->
                 lift_merge_overlap merge_overlap ctxt old (token, value)
                 >|? fun (x, ctxt) -> (Some x, ctxt))
           map))
    (Ticket_token_map.empty, ctxt)
    token_values

let map_e ctxt f =
  Ticket_token_map.map_e ctxt (fun ctxt _key (tkn, value) ->
      f ctxt tkn value >|? fun (new_value, ctxt) -> ((tkn, new_value), ctxt))

let to_list ctxt map =
  Ticket_token_map.to_list ctxt map >>? fun (list, ctxt) ->
  (* Consume gas for traversing the list again and remove the key-hash. *)
  Gas.consume
    ctxt
    (Carbonated_map_costs.fold_cost ~size:(Ticket_token_map.size map))
  >|? fun ctxt -> (List.map snd list, ctxt)

let merge ctxt ~merge_overlap =
  Ticket_token_map.merge ctxt ~merge_overlap:(lift_merge_overlap merge_overlap)

let to_ticket_receipt ctxt ~owner ticket_token_map =
  let open Lwt_result_syntax in
  Ticket_token_map.fold_es
    ctxt
    (fun ctxt acc _ticket_hash (ex_ticket, amount) ->
      if Z.(equal amount zero) then return (acc, ctxt)
      else
        let* ticket_token, ctxt =
          Ticket_token_unparser.unparse ctxt ex_ticket
        in
        let update =
          Ticket_receipt.{ticket_token; updates = [{account = owner; amount}]}
        in
        return (update :: acc, ctxt))
    []
    ticket_token_map
