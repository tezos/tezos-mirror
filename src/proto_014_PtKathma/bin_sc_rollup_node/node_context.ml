(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
open Alpha_context

type t = {
  cctxt : Protocol_client_context.full;
  rollup_address : Sc_rollup.t;
  operator : Signature.Public_key_hash.t;
  initial_level : Raw_level.t;
  block_finality_time : int;
  kind : Sc_rollup.Kind.t;
  fee_parameter : Injection.fee_parameter;
}

let get_operator_keys node_ctxt =
  let open Lwt_result_syntax in
  let+ _, pk, sk = Client_keys.get_key node_ctxt.cctxt node_ctxt.operator in
  (node_ctxt.operator, pk, sk)

let init (cctxt : Protocol_client_context.full) rollup_address operator
    fee_parameter =
  let open Lwt_result_syntax in
  let* initial_level =
    Plugin.RPC.Sc_rollup.initial_level
      cctxt
      (cctxt#chain, cctxt#block)
      rollup_address
  in
  let+ kind =
    Plugin.RPC.Sc_rollup.kind cctxt (cctxt#chain, cctxt#block) rollup_address ()
  in
  {
    cctxt;
    rollup_address;
    operator;
    initial_level;
    kind;
    block_finality_time = 2;
    fee_parameter;
  }
