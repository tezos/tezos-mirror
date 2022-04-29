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

let parse_ticket ~consume_deserialization_gas ~ticketer ~contents ~ty ctxt =
  Script.force_decode_in_context ~consume_deserialization_gas ctxt ty
  >>?= fun (ty, ctxt) ->
  Script.force_decode_in_context ~consume_deserialization_gas ctxt contents
  >>?= fun (contents, ctxt) ->
  Script_ir_translator.parse_comparable_ty ctxt (Micheline.root ty)
  >>?= fun (Ex_comparable_ty contents_type, ctxt) ->
  Script_ir_translator.parse_comparable_data
    ctxt
    contents_type
    (Micheline.root contents)
  >>=? fun (contents, ctxt) ->
  return @@ (ctxt, Ticket_token.Ex_token {ticketer; contents_type; contents})

let parse_ticket_and_operation ~consume_deserialization_gas ~ticketer ~contents
    ~ty ~source ~destination ~entrypoint ~amount ctxt =
  Script.force_decode_in_context ~consume_deserialization_gas ctxt ty
  >>?= fun (ty, ctxt) ->
  Script.force_decode_in_context ~consume_deserialization_gas ctxt contents
  >>?= fun (contents, ctxt) ->
  Script_ir_translator.parse_comparable_ty ctxt (Micheline.root ty)
  >>?= fun (Ex_comparable_ty contents_type, ctxt) ->
  Script_ir_translator.parse_comparable_data
    ctxt
    contents_type
    (Micheline.root contents)
  >>=? fun (contents, ctxt) ->
  let ticket_token =
    Ticket_token.Ex_token {ticketer; contents_type; contents}
  in
  Option.value_e
    ~error:
      (Error_monad.trace_of_error
     @@ Tx_rollup_errors.Internal_error
          "Ticket quantity is negative, this can't happen because it comes \
           from a qty.")
    Script_int.(is_nat @@ of_zint amount)
  >>?= fun amount_node ->
  Script_typed_ir.ticket_t Micheline.dummy_location contents_type
  >>?= fun ticket_ty ->
  let ticket = Script_typed_ir.{ticketer; contents; amount = amount_node} in
  Script_ir_translator.unparse_data ctxt Optimized ticket_ty ticket
  >>=? fun (parameters_expr, ctxt) ->
  Gas.consume ctxt (Script.strip_locations_cost parameters_expr)
  >>?= fun ctxt ->
  let unparsed_parameters =
    Script.lazy_expr (Micheline.strip_locations parameters_expr)
  in
  fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
  let op =
    Script_typed_ir.Internal_operation
      {
        source;
        nonce;
        operation =
          Transaction
            {
              amount = Tez.zero;
              unparsed_parameters;
              destination;
              entrypoint;
              location = Micheline.location parameters_expr;
              parameters_ty = ticket_ty;
              parameters = ticket;
            };
      }
  in
  return (ctxt, ticket_token, op)

let make_withdraw_order ctxt tx_rollup ex_ticket claimer amount =
  Ticket_balance_key.of_ex_token ctxt ~owner:(Tx_rollup tx_rollup) ex_ticket
  >>=? fun (tx_rollup_ticket_hash, ctxt) ->
  let withdrawal =
    Tx_rollup_withdraw.{claimer; ticket_hash = tx_rollup_ticket_hash; amount}
  in
  return (ctxt, withdrawal)

let transfer_ticket_with_hashes ctxt ~src_hash ~dst_hash qty =
  Ticket_balance.adjust_balance ctxt src_hash ~delta:(Z.neg qty)
  >>=? fun (src_storage_diff, ctxt) ->
  Ticket_balance.adjust_balance ctxt dst_hash ~delta:qty
  >>=? fun (dst_storage_diff, ctxt) ->
  Ticket_balance.adjust_storage_space
    ctxt
    ~storage_diff:(Z.add src_storage_diff dst_storage_diff)
  >>=? fun (diff, ctxt) -> return (ctxt, diff)

let transfer_ticket ctxt ~src ~dst ex_token qty =
  Ticket_balance_key.of_ex_token ctxt ~owner:src ex_token
  >>=? fun (src_hash, ctxt) ->
  Ticket_balance_key.of_ex_token ctxt ~owner:dst ex_token
  >>=? fun (dst_hash, ctxt) ->
  transfer_ticket_with_hashes ctxt ~src_hash ~dst_hash qty
