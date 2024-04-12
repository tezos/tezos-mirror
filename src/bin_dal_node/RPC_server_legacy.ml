(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_dal_node_services

let handle_slot_pages ctxt (((), level), slot_index) () () =
  let open Lwt_result_syntax in
  let*? {cryptobox; _} = Node_context.get_ready ctxt in
  let node_store = Node_context.get_store ctxt in
  let*! res =
    Slot_manager.get_commitment_by_published_level_and_index
      ~level
      ~slot_index
      node_store
  in
  match res with
  | Ok commitment ->
      let* pages =
        Slot_manager.get_slot_pages cryptobox node_store commitment
      in
      return_some pages
  | Error `Not_found -> return_none
  | Error (`Decoding_failed _) as res -> Errors.to_tzresult (Lwt.return res)

let register_show_slot_pages ctxt dir =
  Tezos_rpc.Directory.opt_register
    dir
    Services.slot_pages
    (handle_slot_pages ctxt)
