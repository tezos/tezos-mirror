(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

(* Conveniences to construct RPC directory
   against a subcontext of the Node_context *)

let get_head store =
  let open Lwt_result_syntax in
  let* head = Node_context.last_processed_head_opt store in
  match head with
  | None -> failwith "No head"
  | Some {header = {block_hash; _}; _} -> return block_hash

let get_finalized node_ctxt =
  let open Lwt_result_syntax in
  let* level = Node_context.get_finalized_level node_ctxt in
  Node_context.hash_of_level node_ctxt level

let get_last_cemented (node_ctxt : _ Node_context.t) =
  protect @@ fun () ->
  let lcc = Reference.get node_ctxt.lcc in
  Node_context.hash_of_level
    node_ctxt
    (Alpha_context.Raw_level.to_int32 lcc.level)

let block_of_prefix node_ctxt block =
  match block with
  | `Head -> get_head node_ctxt
  | `Hash b -> return b
  | `Level l -> Node_context.hash_of_level node_ctxt l
  | `Finalized -> get_finalized node_ctxt
  | `Cemented -> get_last_cemented node_ctxt
