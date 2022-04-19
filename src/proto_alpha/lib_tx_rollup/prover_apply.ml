(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module Prover_apply = Protocol.Tx_rollup_l2_apply.Make (Context.Prover_context)

type proof = Protocol.Tx_rollup_l2_proof.t

let proof_size =
  Data_encoding.Binary.length Protocol.Tx_rollup_l2_proof.encoding

let apply_message ctxt parameters message =
  let open Lwt_result_syntax in
  let f tree =
    Context.Prover_context.Syntax.catch
      (Prover_apply.apply_message tree parameters message)
      (fun (tree, result) ->
        Lwt.return Context.{tree; result = Inbox.Interpreted result})
      (fun err ->
        Lwt.return
          Context.
            {tree; result = Inbox.Discarded [Environment.wrap_tzerror err]})
  in
  let* (proof, result) = Context.produce_proof ctxt f in
  return (proof, result)
