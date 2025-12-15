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

  type slot_availability = Bitset.t

  type attestation_operation = Kind.attestation Alpha_context.operation

  type tb_slot = int

  let tb_slot_to_int tb_slot = tb_slot

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
        Tezos_dal_node_services.Types.feature_enable;
        incentives_enable;
        dynamic_lag_enable = false;
        number_of_slots;
        attestation_lag;
        attestation_lags = [attestation_lag];
        attestation_threshold;
        traps_fraction = Q.(1 // 1000);
        (* not used in proto_021 *)
        cryptobox_parameters;
        sc_rollup_challenge_window_in_blocks =
          parametric.sc_rollup.challenge_window_in_blocks;
        commitment_period_in_blocks =
          parametric.sc_rollup.commitment_period_in_blocks;
        dal_attested_slots_validity_lag =
          parametric.sc_rollup.reveal_activation_level
            .dal_attested_slots_validity_lag;
        blocks_per_cycle = parametric.blocks_per_cycle;
        minimal_block_delay = Period.to_seconds parametric.minimal_block_delay;
      }

  type error += DAL_accusation_not_available | DAL_publication_not_available

  let () =
    register_error_kind
      `Permanent
      ~id:"dal_accusation_not_available_quebec"
      ~title:"DAL accusation not available on Quebec"
      ~description:"DAL accusation is not available in protocol Quebec."
      ~pp:(fun fmt () ->
        Format.fprintf fmt "DAL accusation is not available in protocol Quebec")
      Data_encoding.unit
      (function DAL_accusation_not_available -> Some () | _ -> None)
      (fun () -> DAL_accusation_not_available)

  let () =
    register_error_kind
      `Permanent
      ~id:"dal_publications_not_available_quebec"
      ~title:"DAL publication not available on Quebec"
      ~description:
        "Publication from DAL node is not implemented in protocol Quebec"
      ~pp:(fun fmt () ->
        Format.fprintf
          fmt
          "Publication from DAL node is not implemented in protocol Quebec")
      Data_encoding.unit
      (function DAL_publication_not_available -> Some () | _ -> None)
      (fun () -> DAL_publication_not_available)

  let inject_entrapment_evidence _cctxt ~attested_level:_ _attestation
      ~slot_index:_ ~shard:_ ~proof:_ ~tb_slot:_ =
    let open Lwt_result_syntax in
    (* This is supposed to be dead code, but we implement a fallback to be defensive. *)
    fail [DAL_accusation_not_available]

  let publish _cctxt ~block_level:_ ~source:_ ~slot_index:_ ~commitment:_
      ~commitment_proof:_ ~src_sk:_ () =
    let open Lwt_result_syntax in
    (* This is supposed to be dead code, but we implement a fallback to be defensive. *)
    fail [DAL_publication_not_available]

  let block_info ?chain ?block ~operations_metadata ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Protocol_client_context.Alpha_block_services.info
      cpctxt
      ?chain
      ?block
      ~metadata:operations_metadata
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

  let get_published_slot_headers ~block_level ctxt =
    let open Lwt_result_syntax in
    let open Protocol.Alpha_context in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let* operations =
      Protocol_client_context.Alpha_block_services.Operations.operations
        cpctxt
        ~block:(`Level block_level)
        ~metadata:`Always
        ()
    in
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
    Lwt_result.map
      (List.filter_map (function
        | header, Dal_plugin.Succeeded -> Some header
        | _, Dal_plugin.Failed -> None))
      (Layer1_services.(
         process_manager_operations [] operations {apply; apply_internal})
      |> List.map_es (fun (slot_index, commitment, status) ->
             let published_level = block_level in
             let slot_index = Dal.Slot_index.to_int slot_index in
             return
               Dal_plugin.({published_level; slot_index; commitment}, status)))

  let get_attestations ~block_level ctxt =
    let open Lwt_result_syntax in
    let open Protocol.Alpha_context in
    let open Protocol_client_context.Alpha_block_services in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let* consensus_ops =
      Protocol_client_context.Alpha_block_services.Operations.operations_in_pass
        cpctxt
        ~block:(`Level block_level)
        ~metadata:`Never
        0
    in
    return
    @@ List.filter_map
         (fun operation ->
           let (Operation_data operation_data) = operation.protocol_data in
           match operation_data.contents with
           | Single (Attestation attestation) ->
               let packed_operation : Kind.attestation Alpha_context.operation =
                 {
                   Alpha_context.shell = operation.shell;
                   protocol_data = operation_data;
                 }
               in
               let tb_slot = Slot.to_int attestation.consensus_content.slot in
               let dal_attestation : dal_attestation option =
                 Option.map
                   (fun x -> (x.attestation :> dal_attestation))
                   attestation.dal_content
               in
               Some (tb_slot, packed_operation, dal_attestation)
           | _ -> None)
         consensus_ops

  let get_committees ctxt ~level =
    let open Lwt_result_syntax in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let*? level = Raw_level.of_int32 level |> wrap in
    let+ pkh_to_shards =
      Plugin.RPC.Dal.dal_shards cpctxt (`Main, `Head 0) ~level ()
    in
    List.fold_left
      (fun acc ({delegate; indexes} : Plugin.RPC.Dal.S.shards_assignment) ->
        let delegate = Tezos_crypto.Signature.Of_V1.public_key_hash delegate in
        let tb_slot =
          (* Associating delegate public key hash to Tenderbake attestation
               slot can be done by matching the first DAL slot index of a given
               attester as there is a DAL protocol invariant that enforces that
               the first DAL slot index corresponds to the TB attestation slot
               index of a give attester. *)
          match List.hd indexes with
          | Some slot -> slot
          | None ->
              (* We assume that the set indexes associated to a delegate is
                   never empty. *)
              assert false
        in
        Tezos_crypto.Signature.Public_key_hash.Map.add
          delegate
          (indexes, tb_slot)
          acc)
      Tezos_crypto.Signature.Public_key_hash.Map.empty
      pkh_to_shards

  let slot_availability (block : block_info) =
    let open Result_syntax in
    let* metadata =
      Option.to_result
        block.metadata
        ~none:
          (TzTrace.make @@ Layer1_services.Cannot_read_block_metadata block.hash)
    in
    return (metadata.protocol_data.dal_attestation :> Bitset.t)

  let is_baker_attested attestation slot_index =
    match Bitset.mem attestation slot_index with Ok b -> b | Error _ -> false

  let is_protocol_attested slot_availability slot_index =
    match Bitset.mem slot_availability slot_index with
    | Ok b -> b
    | Error _ -> false

  let number_of_attested_slots attestation = Bitset.hamming_weight attestation

  let is_delegate ctxt ~pkh =
    let open Lwt_result_syntax in
    let*? pkh = Signature.Of_V_latest.get_public_key_hash pkh in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    (* We just want to know whether <pkh> is a delegate. We call
       'context/delegates/<pkh>/deactivated' just because it should be cheaper
       than calling 'context/delegates/<pkh>/' (called [Delegate.info]). *)
    let*! res =
      Plugin.Alpha_services.Delegate.deactivated cpctxt (`Main, `Head 0) pkh
    in
    return @@ match res with Ok _deactivated -> true | Error _ -> false

  (* Section of helpers for Skip lists *)

  module Skip_list = struct
    type cell = Dal.Slots_history.t

    type hash = Dal.Slots_history.Pointer_hash.t

    let cell_encoding = Dal.Slots_history.encoding

    let hash_encoding = Dal.Slots_history.Pointer_hash.encoding

    let cell_equal = Dal.Slots_history.equal

    let hash_equal = Dal.Slots_history.Pointer_hash.equal

    let cell_hash = Dal.Slots_history.hash

    (* The feature is not available for this protocol. *)
    let back_pointer _cell ~index:_ = Error ()

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
    let cells_of_level ~attested_level ctxt ~dal_constants
        ~pred_publication_level_dal_constants =
      let open Lwt_result_syntax in
      (* 0. For Quebec, block_info is still needed to reconstruct the cells
         of the skip list. Now that Rio is activated, we don't expect
         [cells_of_level] to be actively called. *)
      let* block_info =
        block_info
          ctxt
          ~block:(`Level attested_level)
          ~operations_metadata:`Never
      in
      let attestation_lag =
        dal_constants.Tezos_dal_node_services.Types.attestation_lag
      in
      let published_level =
        Int32.sub attested_level (Int32.of_int attestation_lag)
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
              ( prev_constants.Tezos_dal_node_services.Types.feature_enable,
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
        (* 7. Export and return the skip list cells along with their hashes and
           slot indices. *)
        let module HC = Dal.Slots_history.History_cache in
        let module PHM = Dal.Slots_history.Pointer_hash.Map in
        (* 7-A. The [cache] contains exactly [number_of_slots] cells for the
           current level. *)
        let last_cells =
          (* 7-B. Retrieve the list of (hash, cell) pairs from the cache.  The
             list is ordered by hash and contains [number_of_slots] elements. *)
          HC.view cache |> HC.Map.bindings
        in
        let ordered_hashes_by_insertion =
          (* 7-C. To retrieve the cells in insertion (i.e., slot index) order,
             we rely on [HC.Internal_for_tests.keys], which preserves the
             original insertion order.

             This is acceptable since:
             - The cache size is small (equal to [number_of_slots]).
             - This function is not expected to run in practice, as Rio is
             enabled on all networks.
             - We don't have another way of doing so in Quebec without relying
             on this function. *)
          HC.Internal_for_tests.keys cache
        in
        let last_cells_map =
          (* Convert the list of (hash, cell) into a map for fast lookup. *)
          List.to_seq last_cells |> PHM.of_seq
        in
        let last_cells_ordered_by_insertion =
          (* 7-D. Reconstruct the final list in slot index order.
             Each element includes the cell, its hash, and the associated slot index. *)
          List.mapi
            (fun slot_index hash ->
              match PHM.find hash last_cells_map with
              | None ->
                  (* This should never happen: all hashes from [ordered_hashes_by_insertion]
                     must exist in [last_cells_map]. *)
                  assert false
              | Some cell -> (hash, cell, slot_index, attestation_lag))
            ordered_hashes_by_insertion
        in
        return last_cells_ordered_by_insertion

    let slot_header_of_cell _cell = (* Not implemented for Quebec *) None

    let proto_attestation_status _cell = (* Not implemented for Quebec *) None
  end

  module RPC = struct
    let directory skip_list_cells_store =
      RPC_directory.directory skip_list_cells_store
  end
end

let () = Dal_plugin.register (module Plugin)
