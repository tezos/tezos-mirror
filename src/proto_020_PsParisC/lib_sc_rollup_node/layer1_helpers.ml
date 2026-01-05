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

let get_last_published_commitment ?(allow_unstake = true)
    (cctxt : #Client_context.full) rollup_address operator =
  let open Lwt_result_syntax in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let*? operator = Signature.Of_V_latest.get_public_key_hash operator in
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
    when allow_unstake
         && TzTrace.fold
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
        sc_rollup =
          {
            challenge_window_in_blocks;
            commitment_period_in_blocks;
            reveal_activation_level;
            max_number_of_stored_cemented_commitments;
            max_active_outbox_levels;
            _;
          };
        dal =
          {
            feature_enable;
            attestation_lag;
            number_of_slots;
            cryptobox_parameters;
            _;
          };
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
          reveal_activation_level =
            Some
              (Sc_rollup_proto_types.Constants.reveal_activation_level_to_octez
                 reveal_activation_level);
          max_number_of_stored_cemented_commitments;
          max_active_outbox_levels = Int32.to_int max_active_outbox_levels;
        };
      dal =
        {
          feature_enable;
          attestation_lag;
          attestation_lags = [];
          number_of_slots;
          cryptobox_parameters;
        };
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
    RPC.Sc_rollup.genesis_info cctxt (cctxt#chain, `Head 0) rollup_address
  in
  Node_context.
    {
      level = Raw_level.to_int32 level;
      commitment_hash =
        Sc_rollup_proto_types.Commitment_hash.to_octez commitment_hash;
    }

let get_boot_sector block_hash (node_ctxt : _ Node_context.t) =
  let open Protocol in
  let open Alpha_context in
  let open Lwt_result_syntax in
  let exception Found_boot_sector of string in
  let* block = fetch_tezos_block node_ctxt.l1_ctxt block_hash in
  let missing_boot_sector () =
    failwith "Boot sector not found in Tezos block %a" Block_hash.pp block_hash
  in
  Lwt.catch
    (fun () ->
      let apply (type kind) accu ~source:_ (operation : kind manager_operation)
          (result : kind Apply_results.successful_manager_operation_result) =
        match (operation, result) with
        | ( Sc_rollup_originate {boot_sector; _},
            Sc_rollup_originate_result {address; _} )
          when node_ctxt.config.sc_rollup_address = address ->
            raise (Found_boot_sector boot_sector)
        | _ -> accu
      in
      let apply_internal (type kind) accu ~source:_
          (_operation : kind Apply_internal_results.internal_operation)
          (_result :
            kind Apply_internal_results.successful_internal_operation_result) =
        accu
      in
      let*? () =
        Layer1_services.(
          process_applied_manager_operations
            (Ok ())
            block.operations
            {apply; apply_internal})
      in
      missing_boot_sector ())
    (function
      | Found_boot_sector boot_sector -> return boot_sector
      | _ -> missing_boot_sector ())

let find_whitelist cctxt ?block rollup_address =
  let open Lwt_result_syntax in
  let block = match block with Some b -> `Hash (b, 0) | None -> `Head 0 in
  let* whitelist =
    Plugin.RPC.Sc_rollup.whitelist
      (new Protocol_client_context.wrap_full (cctxt :> Client_context.full))
      (cctxt#chain, block)
      rollup_address
  in
  return
  @@ Option.map
       (List.map Tezos_crypto.Signature.Of_V1.public_key_hash)
       whitelist

let find_last_whitelist_update cctxt rollup_address =
  let open Lwt_result_syntax in
  let* last_whitelist_update =
    Plugin.RPC.Sc_rollup.last_whitelist_update
      (new Protocol_client_context.wrap_full (cctxt :> Client_context.full))
      (cctxt#chain, `Head 0)
      rollup_address
  in
  Option.map
    (fun Protocol.Alpha_context.Sc_rollup.Whitelist.
           {message_index; outbox_level}
       ->
      (message_index, Protocol.Alpha_context.Raw_level.to_int32 outbox_level))
    last_whitelist_update
  |> return

let get_commitment cctxt rollup_address commitment_hash =
  let open Lwt_result_syntax in
  let+ commitment =
    Plugin.RPC.Sc_rollup.commitment
      (new Protocol_client_context.wrap_full (cctxt :> Client_context.full))
      (cctxt#chain, `Head 0)
      rollup_address
      commitment_hash
  in
  Sc_rollup_proto_types.Commitment.to_octez commitment

let get_balance_mutez cctxt ?block pkh =
  let open Lwt_result_syntax in
  let block = match block with Some b -> `Hash (b, 0) | None -> `Head 0 in
  let cctxt =
    new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
  in
  let*? pkh = Signature.Of_V_latest.get_public_key_hash pkh in
  let+ balance =
    Protocol.Contract_services.balance cctxt (cctxt#chain, block) (Implicit pkh)
  in
  Protocol.Alpha_context.Tez.to_mutez balance
