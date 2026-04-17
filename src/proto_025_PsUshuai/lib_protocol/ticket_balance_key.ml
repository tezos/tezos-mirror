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

let make ctxt ~owner ~ticketer ~contents_type ~contents =
  let open Lwt_result_syntax in
  let ticketer_address =
    Script_typed_ir.
      {destination = Contract ticketer; entrypoint = Entrypoint.default}
  in
  let owner_address =
    Script_typed_ir.{destination = owner; entrypoint = Entrypoint.default}
  in
  let* ticketer, ctxt =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_unparser.Optimized_legacy
      Script_typed_ir.address_t
      ticketer_address
  in
  let* owner, ctxt =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_unparser.Optimized_legacy
      Script_typed_ir.address_t
      owner_address
  in
  Lwt.return
  @@ Ticket_hash.make
       ctxt
       ~ticketer:(Micheline.root ticketer)
       ~ty:contents_type
       ~contents
       ~owner:(Micheline.root owner)

(* This function extracts nodes of:
   - Ticketer
   - Type of content
   - Content
   - Owner
       to generate at ticket-balance key-hash.*)
let of_ex_token ctxt ~owner
    (Ticket_token.Ex_token {ticketer; contents_type; contents}) =
  let open Lwt_result_syntax in
  let loc = Micheline.dummy_location in
  let*? cont_ty_unstripped, ctxt =
    Script_ir_unparser.unparse_ty ~loc ctxt contents_type
  in
  (* We strip the annotations from the content type in order to map
     tickets with the same content type, but with different annotations, to the
     same hash. *)
  let*? ctxt =
    Gas.consume ctxt (Script.strip_annotations_cost cont_ty_unstripped)
  in
  let ty = Script.strip_annotations cont_ty_unstripped in
  let* contents, ctxt =
    Script_ir_unparser.unparse_comparable_data
      ctxt
      Script_ir_unparser.Optimized_legacy
      contents_type
      contents
  in
  make
    ctxt
    ~owner
    ~ticketer
    ~contents_type:ty
    ~contents:(Micheline.root contents)
