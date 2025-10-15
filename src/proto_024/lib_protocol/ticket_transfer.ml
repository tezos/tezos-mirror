(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Margiold <contact@marigold.dev>                        *)
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

let parse_ticket ~consume_deserialization_gas ~ticketer ~contents ~ty ctxt =
  let open Lwt_result_syntax in
  let*? ty, ctxt =
    Script.force_decode_in_context ~consume_deserialization_gas ctxt ty
  in
  let*? contents, ctxt =
    Script.force_decode_in_context ~consume_deserialization_gas ctxt contents
  in
  let*? Ex_comparable_ty contents_type, ctxt =
    Script_ir_translator.parse_comparable_ty ctxt (Micheline.root ty)
  in
  let* contents, ctxt =
    Script_ir_translator.parse_comparable_data
      ctxt
      contents_type
      (Micheline.root contents)
  in
  let token = Ticket_token.Ex_token {ticketer; contents_type; contents} in
  return (ctxt, token)

let parse_ticket_and_operation ~consume_deserialization_gas ~ticketer ~contents
    ~ty ~sender ~destination ~entrypoint ~amount ctxt =
  let open Lwt_result_syntax in
  let* ( ctxt,
         (Ticket_token.Ex_token {contents; contents_type; ticketer} as token) )
      =
    parse_ticket ~consume_deserialization_gas ~ticketer ~contents ~ty ctxt
  in
  let*? ticket_ty =
    Script_typed_ir.ticket_t Micheline.dummy_location contents_type
  in
  let ticket = Script_typed_ir.{ticketer; contents; amount} in
  let* unparsed_parameters, ctxt =
    Script_ir_translator.unparse_data ctxt Optimized ticket_ty ticket
  in
  let*? ctxt, nonce = fresh_internal_nonce ctxt in
  let op =
    Script_typed_ir.Internal_operation
      {
        sender;
        nonce;
        operation =
          Transaction_to_smart_contract
            {
              amount = Tez.zero;
              unparsed_parameters;
              destination;
              entrypoint;
              location = Micheline.dummy_location;
              parameters_ty = ticket_ty;
              parameters = ticket;
            };
      }
  in
  return (ctxt, token, op)

let transfer_ticket_with_hashes ctxt ~sender_hash ~dst_hash
    (qty : Ticket_amount.t) =
  let qty = Script_int.(to_zint (qty :> n num)) in
  let open Lwt_result_syntax in
  let* sender_storage_diff, ctxt =
    Ticket_balance.adjust_balance ctxt sender_hash ~delta:(Z.neg qty)
  in
  let* dst_storage_diff, ctxt =
    Ticket_balance.adjust_balance ctxt dst_hash ~delta:qty
  in
  let* diff, ctxt =
    Ticket_balance.adjust_storage_space
      ctxt
      ~storage_diff:(Z.add sender_storage_diff dst_storage_diff)
  in
  return (ctxt, diff)

let transfer_ticket ctxt ~sender ~dst ex_token qty =
  let open Lwt_result_syntax in
  let* sender_hash, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:sender ex_token
  in
  let* dst_hash, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:dst ex_token
  in
  transfer_ticket_with_hashes ctxt ~sender_hash ~dst_hash qty

let transfer_tickets_in_parameters ctxt parameter parameter_ty ~source ~dst =
  let open Lwt_result_syntax in
  let*? has_tickets, ctxt = Ticket_scanner.type_has_tickets ctxt parameter_ty in
  let* tickets, ctxt =
    Ticket_scanner.tickets_of_value
      ~include_lazy:true
      ctxt
      has_tickets
      parameter
  in
  let* ctxt, ticket_receipt, paid_storage_diff =
    List.fold_left_es
      (fun (ctxt, ticket_receipt_acc, paid_storage_diff_acc) ticket ->
        let ticket_token, amount =
          Ticket_scanner.ex_token_and_amount_of_ex_ticket ticket
        in
        let* ctxt, paid_storage_diff =
          transfer_ticket ctxt ~sender:source ~dst ticket_token amount
        in
        let* ticket_token, ctxt =
          Ticket_token_unparser.unparse ctxt ticket_token
        in
        let amount = Script_int.(to_zint (amount :> n num)) in
        let ticket_receipt_item =
          Ticket_receipt.
            {
              ticket_token;
              updates = [{account = source; amount = Z.neg amount}];
              (* We only handle negation from the [source] in this receipt.
                 The addition to [dst] will be taken into account in the receipts
                 generated from the subsequent contract execution. *)
            }
        in
        return
          ( ctxt,
            ticket_receipt_item :: ticket_receipt_acc,
            Z.add paid_storage_diff_acc paid_storage_diff ))
      (ctxt, [], Z.zero)
      tickets
  in
  return (ctxt, ticket_receipt, paid_storage_diff)
