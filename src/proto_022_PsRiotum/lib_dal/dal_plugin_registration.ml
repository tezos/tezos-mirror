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

  type dal_attestations = Environment.Bitset.t

  type slot_availability = Environment.Bitset.t

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
      minimal_participation_ratio = _;
      rewards_ratio = _;
      traps_fraction;
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
        traps_fraction;
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

  (* We choose a previous offset (5 blocks from head) to ensure that the
     injected operation is branched from a valid
     predecessor. Denunciation operations can be emitted when the
     consensus is under attack and may occur so you want to inject the
     operation from a block which is considered "final". *)
  let get_block_offset ~offset level =
    let open Lwt_syntax in
    let offset_int32 = Raw_level.of_int32_exn (Int32.of_int offset) in
    let level_with_offset = Raw_level.diff level offset_int32 in
    if Compare.Int32.(level_with_offset >= 0l) then return (`Head offset)
    else return (`Head 0)

  let inject_entrapment_evidence cctxt ~attested_level
      (operation : attestation_operation) ~slot_index ~lag_index:_ ~shard ~proof
      ~tb_slot:_ =
    let open Lwt_result_syntax in
    let cpctxt = new Protocol_client_context.wrap_rpc_context cctxt in
    let chain = `Main in
    let*? attested_level =
      Raw_level.of_int32 attested_level |> Environment.wrap_tzresult
    in
    let*! block = get_block_offset ~offset:5 attested_level in
    let* {dal = {number_of_slots; _}; _} =
      (* Let's use Head. In practice the number of slots of `Head will
         be greater. If the slot index is not valid, in any case it
         will be caught by the forge or during the injection. *)
      Protocol.Constants_services.parametric cpctxt (chain, `Head 0)
    in
    (* If the number of slots changes between two protocol, the call
       could fail while it should succeed. This is a corner case to
       solve in the future (or just apply a retry policy). *)
    let*? slot_index =
      Dal.Slot_index.of_int ~number_of_slots slot_index
      |> Environment.wrap_tzresult
    in
    let* block_hash =
      Protocol_client_context.Alpha_block_services.hash cctxt ~chain ~block ()
    in
    let shard_with_proof = Dal.Shard_with_proof.{shard; proof} in
    let protocol_data = operation.protocol_data in
    match operation.protocol_data.contents with
    | Single (Attestation _) ->
        let attestation : Kind.attestation Alpha_context.operation =
          {shell = operation.shell; protocol_data}
        in
        let* bytes =
          Plugin.RPC.Forge.dal_entrapment_evidence
            cpctxt
            (chain, block)
            ~branch:block_hash
            ~attestation
            ~slot_index
            ~shard_with_proof
        in
        let bytes = Signature.concat bytes Signature.zero in
        let* _op_hash = Shell_services.Injection.operation cctxt ~chain bytes in
        return_unit

  type error += DAL_publication_not_available

  let () =
    register_error_kind
      `Permanent
      ~id:"dal_publications_not_available_rio"
      ~title:"DAL publication not available on Rio"
      ~description:
        "Publication from DAL node is not implemented in protocol Rio"
      ~pp:(fun fmt () ->
        Format.fprintf
          fmt
          "Publication from DAL node is not implemented in protocol Rio")
      Data_encoding.unit
      (function DAL_publication_not_available -> Some () | _ -> None)
      (fun () -> DAL_publication_not_available)

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
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let* slot_headers =
      Plugin.RPC.Dal.dal_published_slot_headers
        cpctxt
        (`Main, `Level block_level)
        ()
    in
    return
    @@ List.map
         (fun Dal.Slot.Header.{id = {published_level; index}; commitment} ->
           Dal_plugin.
             {
               published_level = Raw_level.to_int32 published_level;
               slot_index = Dal.Slot_index.to_int index;
               commitment;
             })
         slot_headers

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
               let dal_attestation : dal_attestations option =
                 Option.map
                   (fun x -> (x.attestation :> dal_attestations))
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
    return (metadata.protocol_data.dal_attestation :> Environment.Bitset.t)

  let is_baker_attested attestation ~number_of_slots:_ ~number_of_lags:_
      ~lag_index:_ slot_index =
    match Environment.Bitset.mem attestation slot_index with
    | Ok b -> b
    | Error _ -> false

  let is_protocol_attested slot_availability ~number_of_slots:_
      ~number_of_lags:_ ~lag_index:_ slot_index =
    match Environment.Bitset.mem slot_availability slot_index with
    | Ok b -> b
    | Error _ -> false

  let number_of_attested_slots attestation =
    Environment.Bitset.cardinal attestation

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

    let cells_of_level ~attested_level ctxt ~dal_constants
        ~pred_publication_level_dal_constants:_ =
      let open Lwt_result_syntax in
      let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
      let attestation_lag =
        dal_constants.Tezos_dal_node_services.Types.attestation_lag
      in
      let published_level =
        Int32.sub attested_level (Int32.of_int attestation_lag)
      in
      (* 1. There are no cells for [published_level = 0]. *)
      if published_level <= 0l then return []
      else
        let+ cells =
          Plugin.RPC.Dal.skip_list_cells_of_level
            cpctxt
            (`Main, `Level attested_level)
            ()
        in
        (* 2. For other levels, fetch the cells and retrieve the slot indices
           from the cells' content. *)
        List.map
          (fun (hash, cell) ->
            let slot_index =
              Dal.(
                Slots_history.(content cell |> content_id).index
                |> Slot_index.to_int)
            in
            (hash, cell, slot_index, attestation_lag))
          cells

    let slot_header_of_cell cell =
      match Dal.Slots_history.(content cell) with
      | Dal.Slots_history.Unpublished _ ->
          None (* Cannot get a header if nothing is published. *)
      | Published {header = {id; commitment}; _} ->
          Some
            Dal_plugin.
              {
                published_level = Raw_level.to_int32 id.published_level;
                slot_index = Dal.Slot_index.to_int id.index;
                commitment;
              }

    let proto_attestation_status =
      let legacy_attestation_lag = 8 in
      fun cell ->
        Option.some
        @@
        match Dal.Slots_history.(content cell) with
        | Dal.Slots_history.Unpublished _ -> `Unpublished
        | Published {is_proto_attested; _} ->
            if is_proto_attested then `Attested legacy_attestation_lag
            else `Unattested
  end

  module RPC = struct
    let directory skip_list_cells_store =
      RPC_directory.directory skip_list_cells_store
  end
end

let () = Dal_plugin.register (module Plugin)
