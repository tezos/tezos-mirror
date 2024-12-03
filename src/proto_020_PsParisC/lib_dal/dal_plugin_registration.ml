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

  type dal_attestation = Bitset.t

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

  let get_dal_content_of_attestations (block : block_info) =
    let open Protocol.Alpha_context in
    match List.hd block.operations with
    | None ->
        (* that should be unreachable, as there are 4 operation passes *) []
    | Some consensus_ops ->
        List.filter_map
          (fun Protocol_client_context.Alpha_block_services.
                 {receipt; protocol_data; _} ->
            let delegate_opt =
              match receipt with
              | Receipt (Operation_metadata {contents; _}) -> (
                  match contents with
                  | Single_result (Attestation_result {delegate; _}) ->
                      Some delegate
                  | _ -> None)
              | Empty | Too_large | Receipt No_operation_metadata -> None
            in
            match protocol_data with
            | Operation_data
                {
                  contents =
                    Single (Attestation {consensus_content; dal_content; _});
                  _;
                } ->
                Some
                  ( Slot.to_int consensus_content.slot,
                    delegate_opt,
                    (Option.map (fun d -> d.attestation) dal_content
                      :> Bitset.t option) )
            | _ -> None)
          consensus_ops

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

  let dal_attestation (block : block_info) =
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

    (*
      This function mimics what the protocol does in
      {!Dal_slot_storage.finalize_pending_slot_headers}. Given a block_info at
      some level L, an RPC context, the DAL constants for level L, and for level
      L - attestation_lag - 1, the this function computes the cells produced by the
      DAL skip list during the level L using:

       - The information telling which slot headers were waiting for attestation
       at level [L - attestation_lag];

       - The bitset of attested slots at level [L] in the block's metadata.

      It is assumed that at level L the DAL is enabled.

      The ordering of the elements in the returned list is not relevant.
    *)
    let cells_of_level (block_info : block_info) ctxt ~dal_constants
        ~pred_publication_level_dal_constants =
      let open Lwt_result_syntax in
      (* 0. Let's call [attested_level] the block's level. *)
      let attested_level = block_info.header.shell.level in
      let published_level =
        Int32.sub
          attested_level
          (Int32.of_int dal_constants.Dal_plugin.attestation_lag)
      in
      (* 1. There are no cells for [published_level = 0]. *)
      if published_level <= 0l then return []
      else
        let* feature_enable, prev_number_of_slots =
          if published_level = 1l then
            (* For this level, cannot retrieve the constants (as [pred
               publication_level = 0]), but dummy values will suffice. *)
            return (false, 0)
          else
            let* prev_constants =
              Lazy.force pred_publication_level_dal_constants
            in
            return
              ( prev_constants.Dal_plugin.feature_enable,
                prev_constants.number_of_slots )
        in
        let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
        (* 2. We retrieve the last cell of the DAL skip list from the context,
           if any. It's the one stored in the context at [attested_level -
           1]. If no cell is stored yet, we return the genesis cell. *)
        let* previous_cell =
          let* previous_cell_opt =
            (* Should not be negative as [attestation_lag > 0]. *)
            let prev_level = Int32.pred attested_level in
            Plugin.RPC.Dal.dal_commitments_history
              cpctxt
              (`Main, `Level prev_level)
          in
          return
          @@ Option.value previous_cell_opt ~default:Dal.Slots_history.genesis
        in
        let* attested_slot_headers =
          if not feature_enable then
            (* There are no published headers, because the DAL was not enabled,
               and therefore there are no attested headers. *)
            return []
          else
            (* 3. We retrieve the slot headers published at level [level -
               attestation_lag] from the context. *)
            let* published_slot_headers =
              if published_level = 1l then return []
              else
                Plugin.RPC.Dal.dal_published_slot_headers
                  cpctxt
                  (`Main, `Level published_level)
                  ()
            in
            (* 4. We retrieve the bitset of attested slots at level [level]. *)
            let* attested_slots =
              let*? metadata =
                Option.to_result
                  block_info.metadata
                  ~none:
                    (TzTrace.make
                   @@ Layer1_services.Cannot_read_block_metadata block_info.hash
                    )
              in
              return metadata.protocol_data.dal_attestation
            in
            let is_slot_attested slot =
              Dal.Attestation.is_attested
                attested_slots
                slot.Dal.Slot.Header.id.index
            in
            (* 5. We filter the list of slot headers published at [level -
               attestation_lag] and keep only those attested at level [level]. *)
            let attested_slot_headers, _attested_slots_bitset =
              Dal.Slot.compute_attested_slot_headers
                ~is_slot_attested
                published_slot_headers
            in
            return attested_slot_headers
        in
        let*? published_level =
          Raw_level.of_int32 published_level |> Environment.wrap_tzresult
        in
        (* 6. Starting from the [previous_cell], we insert the successive cells
           of level [level] in the skip list thanks to function
           {!add_confirmed_slot_headers}. The function is fed with an empty
           history cache, so the returned [cache] contains exactly the cells
           produced for this [level]. *)
        let*? _last_cell, cache =
          let capacity =
            Int64.of_int
            @@ max prev_number_of_slots dal_constants.number_of_slots
          in
          Dal.Slots_history.add_confirmed_slot_headers
            previous_cell
            (Dal.Slots_history.History_cache.empty ~capacity)
            published_level
            (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997

               Not resilient to DAL parameters change. *)
            ~number_of_slots:dal_constants.number_of_slots
            attested_slot_headers
          |> Environment.wrap_tzresult
        in
        (* 7. We finally export and return the cells alongside their hashes as a
           list. *)
        let last_cells =
          let open Dal.Slots_history.History_cache in
          view cache |> Map.bindings
        in
        return last_cells
  end

  module RPC = struct
    let directory skip_list_cells_store =
      RPC_directory.directory skip_list_cells_store
  end
end

let () = Dal_plugin.register (module Plugin)
