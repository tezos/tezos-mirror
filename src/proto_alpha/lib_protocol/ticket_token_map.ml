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
module Ticket_token_map = Carbonated_map.Make (struct
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
  Ticket_balance_key.ticket_balance_key ctxt ~owner:ticketer token

let update ctxt key f m =
  key_of_ticket_token ctxt key >>=? fun (key_hash, ctxt) ->
  let f ctxt val_opt =
    (match val_opt with
    | Some (_tkn, value) -> f ctxt (Some value)
    | None -> f ctxt None)
    >|? fun (val_opt, ctxt) -> (Option.map (fun v -> (key, v)) val_opt, ctxt)
  in
  Ticket_token_map.update ctxt key_hash f m |> Lwt.return

let fold ctxt f =
  Ticket_token_map.fold ctxt (fun ctxt acc _key_hash (tkn, value) ->
      f ctxt acc tkn value)
