(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol_client_context

type Layer1.block += Block of Alpha_block_services.block_info

let fetch cctxt ?metadata ?chain ?block () =
  let open Lwt_result_syntax in
  let+ block = Alpha_block_services.info cctxt ?metadata ?chain ?block () in
  Block block

let extract_header = function
  | Block block -> block.header.shell
  | _ ->
      invalid_arg ("Internal error: Block is not of protocol " ^ Protocol.name)

let fetch_tezos_block l1_ctxt hash =
  let open Lwt_result_syntax in
  let+ block = Layer1.fetch_tezos_block fetch extract_header l1_ctxt hash in
  match block with
  | Block block -> block
  | _ ->
      Format.kasprintf
        invalid_arg
        "Internal error: Block %a is not of protocol %s"
        Block_hash.pp
        hash
        Protocol.name

let prefetch_tezos_blocks = Layer1.prefetch_tezos_blocks fetch extract_header

let get_last_cemented_commitment (cctxt : #Client_context.full) rollup_address :
    Node_context.lcc tzresult Lwt.t =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let rollup_address = Sc_rollup_proto_types.Address.of_octez rollup_address in
  let+ commitment, level =
    Plugin.RPC.Sc_rollup.last_cemented_commitment_hash_with_level
      cctxt
      (cctxt#chain, `Head 0)
      rollup_address
  in
  {
    Node_context.commitment =
      Sc_rollup_proto_types.Commitment_hash.to_octez commitment;
    level = Protocol.Alpha_context.Raw_level.to_int32 level;
  }

let get_last_published_commitment (cctxt : #Client_context.full) rollup_address
    operator =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let rollup_address = Sc_rollup_proto_types.Address.of_octez rollup_address in
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
             | Environment.Ecoproto_error
                 Protocol.Sc_rollup_errors.Sc_rollup_not_staked ->
                 true
             | _ -> exists)
           false
           trace ->
      return_none
  | Error trace -> fail trace
  | Ok None -> return_none
  | Ok (Some (_staked_hash, staked_commitment)) ->
      return_some (Sc_rollup_proto_types.Commitment.to_octez staked_commitment)

let get_kind cctxt rollup_address =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let rollup_address = Sc_rollup_proto_types.Address.of_octez rollup_address in
  let+ kind =
    RPC.Sc_rollup.kind cctxt (cctxt#chain, cctxt#block) rollup_address ()
  in
  Sc_rollup_proto_types.Kind.to_octez kind

let genesis_inbox cctxt ~genesis_level =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let+ inbox =
    Plugin.RPC.Sc_rollup.inbox cctxt (cctxt#chain, `Level genesis_level)
  in
  Sc_rollup_proto_types.Inbox.to_octez inbox

let constants_of_parametric
    Protocol.Alpha_context.Constants.Parametric.
      {
        minimal_block_delay;
        delay_increment_per_round;
        sc_rollup = {challenge_window_in_blocks; commitment_period_in_blocks; _};
        dal = {feature_enable; attestation_lag; number_of_slots; _};
        _;
      } =
  let open Protocol.Alpha_context in
  Rollup_constants.
    {
      minimal_block_delay = Period.to_seconds minimal_block_delay;
      delay_increment_per_round = Period.to_seconds delay_increment_per_round;
      sc_rollup =
        {
          challenge_window_in_blocks;
          commitment_period_in_blocks;
          reveal_activation_level = None;
        };
      dal = {feature_enable; attestation_lag; number_of_slots};
    }

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2901
   The constants are retrieved from the latest tezos block. These constants can
   be different from the ones used at the creation at the rollup because of a
   protocol amendment that modifies some of them. This need to be fixed when the
   rollup nodes will be able to handle the migration of protocol.
*)
let retrieve_constants ?(block = `Head 0) cctxt =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let+ {parametric; _} =
    Protocol.Constants_services.all cctxt (cctxt#chain, block)
  in
  constants_of_parametric parametric

let retrieve_genesis_info cctxt rollup_address =
  let open Lwt_result_syntax in
  let open Protocol.Alpha_context in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let+ {level; commitment_hash} =
    RPC.Sc_rollup.genesis_info
      cctxt
      (cctxt#chain, `Head 0)
      (Sc_rollup_proto_types.Address.of_octez rollup_address)
  in
  Node_context.
    {
      level = Raw_level.to_int32 level;
      commitment_hash =
        Sc_rollup_proto_types.Commitment_hash.to_octez commitment_hash;
    }
