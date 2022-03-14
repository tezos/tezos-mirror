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

(* In the ticket balance table, credit the ticket [ticket] to the owner [contract]. *)
let add_ticket_balance contract ctxt ticket =
  let (token, amount) = Ticket_token.token_and_amount_of_ex_ticket ticket in
  Ticket_balance_key.of_ex_token ctxt ~owner:contract token
  >>=? fun (hash, ctxt) ->
  Ticket_balance.adjust_balance ctxt hash ~delta:(Script_int.to_zint amount)
  >|=? fun ((_added_size : Z.t), ctxt) -> ctxt

let update_contract_tickets ctxt contract =
  (* We could fetch and parse code and storage separately, and extract tickets
     only when storage type may have tickets, but there is little performance
     benefit in that for added complexity. Most of the cost comes from fetching
     code, which is required for accessing storage type. *)
  Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
  match script with
  | None -> return ctxt
  | Some script ->
      Script_ir_translator.parse_script
        ctxt
        ~legacy:true
        ~allow_forged_in_storage:true
        script
      >>=? fun (ex_script, ctxt) ->
      let (Ex_script {storage_type; storage; _}) = ex_script in
      Ticket_scanner.type_has_tickets ctxt storage_type
      >>?= fun (has_tickets, ctxt) ->
      Ticket_scanner.tickets_of_value
        ctxt
        ~include_lazy:true
        has_tickets
        storage
      >>=? fun (tickets, ctxt) ->
      List.fold_left_es
        (add_ticket_balance (Destination.Contract contract))
        ctxt
        tickets

let is_originated contract =
  match Contract.is_originated contract with Some _ -> true | _ -> false

let init ctxt =
  Contract.list ctxt >>= fun contracts ->
  (* Implicit accounts cannot own tickets, so we filter them out. *)
  let contracts = List.filter is_originated contracts in
  List.fold_left_es update_contract_tickets ctxt contracts
