(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* This function extracts nodes of:
    - Ticketer
    - Type of content
    - Content
    - Owner
   to generate at ticket-balance key-hash.
*)
let ticket_balance_key ctxt ~owner
    (Ticket_token.Ex_token {ticketer; contents_type; contents}) =
  let loc = Micheline.dummy_location in
  Script_ir_translator.unparse_comparable_ty ~loc ctxt contents_type
  >>?= fun (cont_ty_unstripped, ctxt) ->
  (* We strip the annotations from the content type in order to map
     tickets with the same content type, but with different annotations, to the
     same hash. *)
  Gas.consume ctxt (Script.strip_annotations_cost cont_ty_unstripped)
  >>?= fun ctxt ->
  let typ = Script.strip_annotations cont_ty_unstripped in
  let ticketer_address =
    Script_typed_ir.{contract = ticketer; entrypoint = Entrypoint.default}
  in
  let owner_address =
    Script_typed_ir.{contract = owner; entrypoint = Entrypoint.default}
  in
  Script_ir_translator.unparse_data
    ctxt
    Script_ir_translator.Optimized_legacy
    Script_typed_ir.address_t
    ticketer_address
  >>=? fun (ticketer, ctxt) ->
  Script_ir_translator.unparse_comparable_data
    ~loc
    ctxt
    Script_ir_translator.Optimized_legacy
    contents_type
    contents
  >>=? fun (contents, ctxt) ->
  Script_ir_translator.unparse_data
    ctxt
    Script_ir_translator.Optimized_legacy
    Script_typed_ir.address_t
    owner_address
  >>=? fun (owner, ctxt) ->
  Lwt.return (Ticket_hash.make ctxt ~ticketer ~typ ~contents ~owner)
