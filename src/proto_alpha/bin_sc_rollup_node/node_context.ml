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
  l1_ctxt : Layer1.t;
  rollup_address : Sc_rollup.t;
  operators : Configuration.operators;
  genesis_info : Sc_rollup.Commitment.genesis_info;
  block_finality_time : int;
  kind : Sc_rollup.Kind.t;
  fee_parameter : Injection.fee_parameter;
  protocol_constants : Constants.t;
  loser_mode : Loser_mode.t;
  store : Store.t;
  context : Context.index;
}

let get_operator node_ctxt purpose =
  Configuration.Operator_purpose_map.find purpose node_ctxt.operators

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2901
   The constants are retrieved from the latest tezos block. These constants can
   be different from the ones used at the creation at the rollup because of a
   protocol amendment that modifies some of them. This need to be fixed when the
   rollup nodes will be able to handle the migration of protocol.
*)
let retrieve_constants cctxt =
  Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)

let init (cctxt : Protocol_client_context.full) l1_ctxt rollup_address kind
    operators fee_parameter ~loser_mode store context =
  let open Lwt_result_syntax in
  let+ protocol_constants = retrieve_constants cctxt in
  {
    cctxt;
    l1_ctxt;
    rollup_address;
    operators;
    genesis_info = l1_ctxt.Layer1.genesis_info;
    kind;
    block_finality_time = 2;
    fee_parameter;
    protocol_constants;
    loser_mode;
    store;
    context;
  }

let checkout_context node_ctxt block_hash =
  let open Lwt_result_syntax in
  let*! context_hash = Store.Contexts.find node_ctxt.store block_hash in
  let*? context_hash =
    match context_hash with
    | None ->
        error (Sc_rollup_node_errors.Cannot_checkout_context (block_hash, None))
    | Some context_hash -> ok context_hash
  in
  let*! ctxt = Context.checkout node_ctxt.context context_hash in
  match ctxt with
  | None ->
      tzfail
        (Sc_rollup_node_errors.Cannot_checkout_context
           (block_hash, Some (Context.hash_to_raw_string context_hash)))
  | Some ctxt -> return ctxt
