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
  let token = Ticket_token.Ex_token {ticketer; contents_type; contents} in
  return (ctxt, token)

let parse_ticket_and_operation ~consume_deserialization_gas ~ticketer ~contents
    ~ty ~sender ~destination ~entrypoint ~amount ctxt =
  parse_ticket ~consume_deserialization_gas ~ticketer ~contents ~ty ctxt
  >>=? fun ( ctxt,
             (Ticket_token.Ex_token {contents; contents_type; ticketer} as
             token) ) ->
  Script_typed_ir.ticket_t Micheline.dummy_location contents_type
  >>?= fun ticket_ty ->
  let ticket = Script_typed_ir.{ticketer; contents; amount} in
  Script_ir_translator.unparse_data ctxt Optimized ticket_ty ticket
  >>=? fun (unparsed_parameters, ctxt) ->
  fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
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
  Ticket_balance.adjust_balance ctxt sender_hash ~delta:(Z.neg qty)
  >>=? fun (sender_storage_diff, ctxt) ->
  Ticket_balance.adjust_balance ctxt dst_hash ~delta:qty
  >>=? fun (dst_storage_diff, ctxt) ->
  Ticket_balance.adjust_storage_space
    ctxt
    ~storage_diff:(Z.add sender_storage_diff dst_storage_diff)
  >>=? fun (diff, ctxt) -> return (ctxt, diff)

let transfer_ticket ctxt ~sender ~dst ex_token qty =
  Ticket_balance_key.of_ex_token ctxt ~owner:sender ex_token
  >>=? fun (sender_hash, ctxt) ->
  Ticket_balance_key.of_ex_token ctxt ~owner:dst ex_token
  >>=? fun (dst_hash, ctxt) ->
  transfer_ticket_with_hashes ctxt ~sender_hash ~dst_hash qty
