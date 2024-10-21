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

open Protocol
open Alpha_context

let wrap = Environment.wrap_tzresult

module Plugin = struct
  module Proto = Registerer.Registered

  type block_info = Protocol_client_context.Alpha_block_services.block_info

  type attested_indices = Bitset.t

  let parametric_constants chain block ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Protocol.Constants_services.parametric cpctxt (chain, block)

  let get_constants chain block ctxt =
    let open Lwt_result_syntax in
    let* parametric = parametric_constants chain block ctxt in
    let {
      Constants.Parametric.feature_enable;
      incentives_enable;
      number_of_slots;
      attestation_lag;
      attestation_threshold;
      cryptobox_parameters;
    } =
      parametric.dal
    in
    return
      {
        Dal_plugin.feature_enable;
        incentives_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold;
        cryptobox_parameters;
        sc_rollup_challenge_window_in_blocks =
          parametric.sc_rollup.challenge_window_in_blocks;
        commitment_period_in_blocks =
          parametric.sc_rollup.commitment_period_in_blocks;
        dal_attested_slots_validity_lag =
          parametric.sc_rollup.reveal_activation_level
            .dal_attested_slots_validity_lag;
        blocks_per_cycle = parametric.blocks_per_cycle;
      }

  let block_info ?chain ?block ~metadata ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Protocol_client_context.Alpha_block_services.info
      cpctxt
      ?chain
      ?block
      ~metadata
      ()

  let block_shell_header (block_info : block_info) = block_info.header.shell

  let get_round fitness =
    let open Result_syntax in
    let* round = Fitness.round_from_raw fitness |> wrap in
    return @@ Round.to_int32 round

  (* Turn the given value of type {!Protocol.Apply_operation_result.operation_result}
     into a value of type {!Dal_plugin.operation_application_result}. *)
  let status_of_result = function
    | Protocol.Apply_operation_result.Applied _ -> Dal_plugin.Succeeded
    | _ -> Dal_plugin.Failed

  let get_published_slot_headers (block : block_info) =
    let open Lwt_result_syntax in
    let open Protocol.Alpha_context in
    let apply_internal acc ~source:_ _op _res = acc in
    let apply (type kind) acc ~source:_ (op : kind manager_operation)
        (result : (kind, _, _) Protocol.Apply_operation_result.operation_result)
        =
      match op with
      | Dal_publish_commitment operation ->
          (operation.slot_index, operation.commitment, status_of_result result)
          :: acc
      | _ -> acc
    in
    Layer1_services.(
      process_manager_operations [] block.operations {apply; apply_internal})
    |> List.map_es (fun (slot_index, commitment, status) ->
           let published_level = block.header.shell.level in
           let slot_index = Dal.Slot_index.to_int slot_index in
           return Dal_plugin.({published_level; slot_index; commitment}, status))

  let get_committee ctxt ~level =
    let open Lwt_result_syntax in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let*? level = Raw_level.of_int32 level |> wrap in
    let+ pkh_to_shards =
      Plugin.RPC.Dal.dal_shards cpctxt (`Main, `Head 0) ~level ()
    in
    List.fold_left
      (fun acc ({delegate; indexes} : Plugin.RPC.Dal.S.shards_assignment) ->
        Signature.Public_key_hash.Map.add delegate indexes acc)
      Signature.Public_key_hash.Map.empty
      pkh_to_shards

  let attested_slot_headers (block : block_info) =
    let open Result_syntax in
    let* metadata =
      Option.to_result
        block.metadata
        ~none:
          (TzTrace.make @@ Layer1_services.Cannot_read_block_metadata block.hash)
    in
    return (metadata.protocol_data.dal_attestation :> Bitset.t)

  let is_attested attestation slot_index =
    match Bitset.mem attestation slot_index with Ok b -> b | Error _ -> false

  (* Section of helpers for Skip lists *)

  module Skip_list = struct
    type cell = Dal.Slots_history.t

    type hash = Dal.Slots_history.Pointer_hash.t

    let cell_encoding = Dal.Slots_history.encoding

    let hash_encoding = Dal.Slots_history.Pointer_hash.encoding

    let cell_equal = Dal.Slots_history.equal

    let hash_equal = Dal.Slots_history.Pointer_hash.equal

    let cell_hash = Dal.Slots_history.hash

    (* This function returns the list of cells of the DAL skip list constructed
       at the level of the block whose info are given. For that, it calls the
       {!Plugin.RPC.Dal.skip_list_cells_of_level} RPC that directly retrieves
       the list of cells from the L1 context. In case the entry in the context
       is not initialized yet, the empty list is returned.

       Compared to the old implementation in {!cells_of_level_legacy}, this
       version is much simpler as it doesn't redo the computations. On the
       contrary, the legacy version accesses published DAL commitments and the
       attestation bitset in the block's metadata, and then reconstructs the
       cells of the skip list by calling the appropriate DAL function in the
       protocol. *)

    let cells_of_level (block_info : block_info) ctxt ~dal_constants
        ~pred_publication_level_dal_constants:_ =
      let open Lwt_result_syntax in
      let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
      let attested_level = block_info.header.shell.level in
      let published_level =
        Int32.sub
          attested_level
          (Int32.of_int dal_constants.Dal_plugin.attestation_lag)
      in
      (* 1. There are no cells for [published_level = 0]. *)
      if published_level <= 0l then return []
      else
        Plugin.RPC.Dal.skip_list_cells_of_level
          cpctxt
          (`Main, `Level attested_level)
          ()
  end

  module RPC = struct
    let directory skip_list_cells_store =
      RPC_directory.directory skip_list_cells_store
  end
end

let () = Dal_plugin.register (module Plugin)
