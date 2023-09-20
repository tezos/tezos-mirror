(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4167

   Disclaimer:
   A more natural place to place [unparse] would be in [Ticket_token] module.
   But unfortunantly, we could not put it there due to circular dependency.
   The root cause of this circular dependency is the dependency
   from [Script_ir_translator] to [Apply_internal_result], but removing this
   dependency would require a relatively large refactor. *)

let unparse ctxt (Ticket_token.Ex_token {ticketer; contents_type; contents}) =
  let open Lwt_result_syntax in
  let open Script_ir_unparser in
  let* contents, ctxt =
    unparse_comparable_data ctxt Optimized_legacy contents_type contents
  in
  let*? ty_unstripped, ctxt =
    unparse_ty ~loc:Micheline.dummy_location ctxt contents_type
  in
  let*? ctxt = Gas.consume ctxt (Script.strip_annotations_cost ty_unstripped) in
  let ty = Script.strip_annotations ty_unstripped in
  let*? ctxt = Gas.consume ctxt (Script.strip_locations_cost ty) in
  let contents_type = Micheline.strip_locations ty in
  let ticket_token = Ticket_token.{ticketer; contents_type; contents} in
  return (ticket_token, ctxt)
