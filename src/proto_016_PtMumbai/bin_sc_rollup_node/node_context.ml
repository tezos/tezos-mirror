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

type lcc = {commitment : Sc_rollup.Commitment.Hash.t; level : Raw_level.t}

type 'a t = {
  cctxt : Protocol_client_context.full;
  dal_cctxt : Dal_node_client.cctxt;
  data_dir : string;
  l1_ctxt : Layer1.t;
  rollup_address : Sc_rollup.t;
  operators : Configuration.operators;
  genesis_info : Sc_rollup.Commitment.genesis_info;
  injector_retention_period : int;
  block_finality_time : int;
  kind : Sc_rollup.Kind.t;
  fee_parameters : Configuration.fee_parameters;
  protocol_constants : Constants.t;
  loser_mode : Loser_mode.t;
  store : 'a Store.t;
  context : 'a Context.index;
  mutable lcc : lcc;
  mutable lpc : Sc_rollup.Commitment.t option;
}

type rw = [`Read | `Write] t

type ro = [`Read] t

let get_operator node_ctxt purpose =
  Configuration.Operator_purpose_map.find purpose node_ctxt.operators

let is_operator node_ctxt pkh =
  Configuration.Operator_purpose_map.exists
    (fun _ operator -> Tezos_crypto.Signature.Public_key_hash.(operator = pkh))
    node_ctxt.operators

let get_fee_parameter node_ctxt purpose =
  Configuration.Operator_purpose_map.find purpose node_ctxt.fee_parameters
  |> Option.value ~default:(Configuration.default_fee_parameter ~purpose ())

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2901
   The constants are retrieved from the latest tezos block. These constants can
   be different from the ones used at the creation at the rollup because of a
   protocol amendment that modifies some of them. This need to be fixed when the
   rollup nodes will be able to handle the migration of protocol.
*)
let retrieve_constants cctxt =
  Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)

let get_last_cemented_commitment (cctxt : Protocol_client_context.full)
    rollup_address =
  let open Lwt_result_syntax in
  let+ commitment, level =
    Plugin.RPC.Sc_rollup.last_cemented_commitment_hash_with_level
      cctxt
      (cctxt#chain, `Head 0)
      rollup_address
  in
  {commitment; level}

let get_last_published_commitment (cctxt : Protocol_client_context.full)
    rollup_address operator =
  let open Lwt_result_syntax in
  let*! res =
    Plugin.RPC.Sc_rollup.staked_on_commitment
      cctxt
      (cctxt#chain, `Head 0)
      rollup_address
      operator
  in
  match res with
  | Error trace
    when TzTrace.fold
           (fun exists -> function
             | Environment.Ecoproto_error Sc_rollup_errors.Sc_rollup_not_staked
               ->
                 true
             | _ -> exists)
           false
           trace ->
      return_none
  | Error trace -> fail trace
  | Ok None -> return_none
  | Ok (Some (_staked_hash, staked_commitment)) -> return_some staked_commitment

let init (cctxt : Protocol_client_context.full) dal_cctxt ~data_dir l1_ctxt
    rollup_address kind operators fee_parameters ~loser_mode store context =
  let open Lwt_result_syntax in
  let publisher = Configuration.Operator_purpose_map.find Publish operators in
  let* protocol_constants = retrieve_constants cctxt
  and* lcc = get_last_cemented_commitment cctxt rollup_address
  and* lpc =
    Option.filter_map_es
      (get_last_published_commitment cctxt rollup_address)
      publisher
  in
  return
    {
      cctxt;
      dal_cctxt;
      data_dir;
      l1_ctxt;
      rollup_address;
      operators;
      genesis_info = l1_ctxt.Layer1.genesis_info;
      lcc;
      lpc;
      kind;
      injector_retention_period = 0;
      block_finality_time = 2;
      fee_parameters;
      protocol_constants;
      loser_mode;
      store;
      context;
    }

let checkout_context node_ctxt block_hash =
  let open Lwt_result_syntax in
  let*! l2_block = Store.L2_blocks.find node_ctxt.store block_hash in
  let*? context_hash =
    match l2_block with
    | None ->
        error (Sc_rollup_node_errors.Cannot_checkout_context (block_hash, None))
    | Some {header = {context; _}; _} -> ok context
  in
  let*! ctxt = Context.checkout node_ctxt.context context_hash in
  match ctxt with
  | None ->
      tzfail
        (Sc_rollup_node_errors.Cannot_checkout_context
           (block_hash, Some context_hash))
  | Some ctxt -> return ctxt

let metadata node_ctxt =
  let address = node_ctxt.rollup_address in
  let origination_level = node_ctxt.genesis_info.Sc_rollup.Commitment.level in
  Sc_rollup.Metadata.{address; origination_level}

let dal_enabled node_ctxt =
  node_ctxt.protocol_constants.parametric.dal.feature_enable

let readonly (node_ctxt : _ t) =
  {
    node_ctxt with
    store = Store.readonly node_ctxt.store;
    context = Context.readonly node_ctxt.context;
  }

type 'a delayed_write = ('a, rw) Delayed_write_monad.t

let get_full_l2_block {store; _} block_hash =
  let open Lwt_syntax in
  let* block = Store.L2_blocks.get store block_hash in
  let* inbox = Store.Inboxes.get store block.header.inbox_hash
  and* {messages; _} = Store.Messages.get store block.header.inbox_witness
  and* commitment =
    Option.map_s (Store.Commitments.get store) block.header.commitment_hash
  in
  return {block with content = {Sc_rollup_block.inbox; messages; commitment}}
