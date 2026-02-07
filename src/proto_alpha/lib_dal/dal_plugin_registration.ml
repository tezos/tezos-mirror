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

  type dal_attestations = Alpha_context.Dal.Attestations.t

  type slot_availability = Alpha_context.Dal.Slot_availability.t

  type attestation_operation =
    | Op : 'a Kind.consensus Alpha_context.operation -> attestation_operation

  type tb_slot = Slot.t

  let tb_slot_to_int tb_slot = Slot.to_int tb_slot

  type error +=
    | Attested_level_mismatch of {
        attested_level : int32;
        published_level : int32;
        attestation_lag : int;
      }
    | Unexpected_RPC_outcome of string

  let () =
    Protocol_client_context.register_error_kind
      `Permanent
      ~id:"Attested_level_mismatch"
      ~title:"Mismatch between computed and given attested level"
      ~description:
        "Mismatch between the given attested level and the computed one from \
         published_level and the lag"
      ~pp:(fun ppf (attested_level, published_level, attestation_lag) ->
        Format.fprintf
          ppf
          "Mismatch between the given attested level %ld and the computed one \
           as %ld + %d."
          attested_level
          published_level
          attestation_lag)
      Data_encoding.(
        obj3
          (req "attested_level" int32)
          (req "published_level" int32)
          (req "attestation_lag" int8))
      (function
        | Attested_level_mismatch
            {attested_level; published_level; attestation_lag} ->
            Some (attested_level, published_level, attestation_lag)
        | _ -> None)
      (fun (attested_level, published_level, attestation_lag) ->
        Attested_level_mismatch
          {attested_level; published_level; attestation_lag})

  let () =
    Protocol_client_context.register_error_kind
      `Permanent
      ~id:"Unexpected_RPC_outcome"
      ~title:"Unexpected outcome from L1 RPC"
      ~description:"An RPC to the L1 node gave an unexpected answer"
      ~pp:(fun ppf rpc_string ->
        Format.fprintf
          ppf
          "The RPC %s to the L1 node gave an unexpected answer"
          rpc_string)
      Data_encoding.(obj1 (req "rpc" string))
      (function Unexpected_RPC_outcome s -> Some s | _ -> None)
      (fun s -> Unexpected_RPC_outcome s)

  let parametric_constants chain block ctxt =
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    Plugin.Constants_services.parametric cpctxt (chain, block)

  let get_constants chain block ctxt =
    let open Lwt_result_syntax in
    let* parametric = parametric_constants chain block ctxt in
    let {
      Constants.Parametric.feature_enable;
      incentives_enable;
      dynamic_lag_enable;
      number_of_slots;
      attestation_lag;
      attestation_lags;
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
        dynamic_lag_enable;
        number_of_slots;
        attestation_lag;
        attestation_lags;
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
      (Op attestation : attestation_operation) ~slot_index ~shard ~proof
      ~tb_slot =
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
      Plugin.Constants_services.parametric cpctxt (chain, `Head 0)
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
    let* bytes =
      Plugin.RPC.Forge.dal_entrapment_evidence
        cpctxt
        (chain, block)
        ~branch:block_hash
        ~consensus_slot:tb_slot
        ~attestation
        ~slot_index
        ~shard_with_proof
    in
    let bytes = Signature.concat bytes Signature.zero in
    let* _op_hash = Shell_services.Injection.operation cctxt ~chain bytes in
    return_unit

  let publish cctxt ~block_level ~source ~slot_index ~commitment
      ~commitment_proof ~src_sk () =
    let open Lwt_result_syntax in
    let chain = `Main in
    let block = `Level block_level in
    let cpctxt = new Protocol_client_context.wrap_rpc_context cctxt in
    let* {dal = {number_of_slots; _}; _} =
      Plugin.Constants_services.parametric cpctxt (chain, block)
    in
    let*? slot_index =
      Dal.Slot_index.of_int ~number_of_slots slot_index
      |> Environment.wrap_tzresult
    in
    let* block_hash =
      Protocol_client_context.Alpha_block_services.hash cctxt ~chain ~block ()
    in
    let*? source = Signature.Of_V_latest.get_public_key_hash source in
    let* counter =
      let* pcounter =
        Plugin.Alpha_services.Contract.counter cpctxt (chain, `Head 0) source
      in
      return (Manager_counter.succ pcounter)
    in
    let operation =
      Dal_publish_commitment {slot_index; commitment; commitment_proof}
    in
    let operation =
      (* A dry-run of the "publish dal commitment" command for each tz kinds outputs:
         - tz1: fees of 513µtz and 1333 gas consumed
         - tz2: fees of 514µtz and 1318 gas consumed
         - tz3: fees of 543µtz and 1607 gas consumed
         - tz4: fees of 700µtz and 2837 gas consumed
         We added a margin to it.
         The storage limit value has been selected purely arbitrarily
         (it worked with this value when I experimented, so I did not change it). *)
      Alpha_context.(
        Manager_operation
          {
            source;
            fee = Tez.of_mutez_exn (Int64.of_int 750);
            counter;
            operation;
            gas_limit = Gas.Arith.integral_of_int_exn 3000;
            storage_limit = Z.of_int 100;
          })
    in
    let bytes =
      Data_encoding.Binary.to_bytes_exn
        Operation.unsigned_encoding
        ({branch = block_hash}, Contents_list (Single operation))
    in
    let*? src_sk = Signature.Of_V_latest.get_secret_key src_sk in
    let bytes =
      Signature.append ~watermark:Signature.Generic_operation src_sk bytes
    in
    Shell_services.Injection.operation cctxt ~chain bytes

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
    Lwt.return @@ Result.map List.flatten
    @@ List.map_e
         (fun operation ->
           let (Operation_data operation_data) = operation.protocol_data in
           match operation_data.contents with
           | Single (Attestation attestation) ->
               let packed_operation =
                 Op
                   {
                     Alpha_context.shell = operation.shell;
                     protocol_data = operation_data;
                   }
               in
               let tb_slot = attestation.consensus_content.slot in
               let dal_attestations =
                 Option.map
                   (fun dal_content ->
                     (dal_content.attestations :> dal_attestations))
                   attestation.dal_content
               in
               Ok [(tb_slot, packed_operation, dal_attestations)]
           | Single (Attestations_aggregate {committee; _}) ->
               let packed_operation =
                 Op
                   {
                     Alpha_context.shell = operation.shell;
                     protocol_data = operation_data;
                   }
               in
               let slots_and_dal_attestations =
                 List.map
                   (fun (slot, dal_content_opt) ->
                     ( slot,
                       Option.map
                         (fun dal_content ->
                           (dal_content.attestations :> dal_attestations))
                         dal_content_opt ))
                   committee
               in
               Ok
                 (List.map
                    (fun (tb_slot, dal_attestations) ->
                      (tb_slot, packed_operation, dal_attestations))
                    slots_and_dal_attestations)
           | _ -> Ok [])
         consensus_ops

  let get_committees ctxt ~level =
    let open Lwt_result_syntax in
    let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
    let*? level = Raw_level.of_int32 level |> wrap in
    let* pkh_to_shards =
      Plugin.RPC.Dal.dal_shards cpctxt (`Main, `Head 0) ~level ()
    in
    let* pkh_to_tb_slot =
      let* res =
        Plugin.RPC.Attestation_rights.get
          cpctxt
          (`Main, `Head 0)
          ~levels:[level]
      in
      match res with
      | [{delegates_rights; _}] -> return delegates_rights
      | _ ->
          tzfail
            (Unexpected_RPC_outcome
               (Resto.Path.to_string
                  Plugin.RPC.Attestation_rights.S.attestation_path))
    in
    let pkh_to_tb_slot_map =
      List.fold_left
        (fun map Plugin.RPC.Attestation_rights.{delegate; first_slot; _} ->
          Signature.Public_key_hash.Map.add delegate first_slot map)
        Signature.Public_key_hash.Map.empty
        pkh_to_tb_slot
    in
    List.fold_left_es
      (fun acc ({delegate; indexes} : Plugin.RPC.Dal.S.shards_assignment) ->
        let* tb_slot =
          match
            Signature.Public_key_hash.Map.find delegate pkh_to_tb_slot_map
          with
          | Some slot -> return (Slot.to_int slot)
          | None ->
              (* A delegate with assigned DAL shards also has attestation rights. *)
              tzfail
                (Unexpected_RPC_outcome
                   (Resto.Path.to_string
                      Tezos_rpc.Path.(Plugin.RPC.Dal.path / "shards")))
        in
        let delegate = Tezos_crypto.Signature.Of_V3.public_key_hash delegate in
        return
        @@ Tezos_crypto.Signature.Public_key_hash.Map.add
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
    return metadata.protocol_data.dal_slot_availability

  let is_baker_attested attestations ~number_of_slots ~number_of_lags ~lag_index
      slot_index =
    match Dal.Slot_index.of_int_opt ~number_of_slots slot_index with
    | None -> false
    | Some slot_index ->
        Dal.Attestations.is_attested
          attestations
          ~number_of_slots
          ~number_of_lags
          ~lag_index
          slot_index

  let is_protocol_attested slot_availability ~number_of_slots ~number_of_lags
      ~lag_index slot_index =
    match Dal.Slot_index.of_int_opt ~number_of_slots slot_index with
    | None -> false
    | Some slot_index ->
        Alpha_context.Dal.Slot_availability.is_attested
          slot_availability
          ~number_of_slots
          ~number_of_lags
          ~lag_index
          slot_index

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

    let back_pointer cell ~index =
      Ok (Dal.Slots_history.back_pointer cell ~index)

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

    let cells_of_level ~attested_level ctxt ~dal_constants:_
        ~pred_publication_level_dal_constants:_ =
      let open Lwt_result_syntax in
      let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
      let* cells =
        Plugin.RPC.Dal.skip_list_cells_of_level
          cpctxt
          (`Main, `Level attested_level)
          ()
      in
      (* With dynamic lags, cells at a given block level can have different
         attested levels. We return all cells and let the DAL node handle them. *)
      let module H = Dal.Slots_history in
      List.map_es
        (fun (hash, cell) ->
          let cell_id = H.(content cell |> content_id) in
          let slot_id = cell_id.H.header_id in
          let slot_index = Dal.Slot_index.to_int slot_id.index in
          let attestation_lag =
            H.attestation_lag_value cell_id.attestation_lag
          in
          return (hash, cell, slot_index, attestation_lag))
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

    let proto_attestation_status cell =
      Option.some
      @@
      match Dal.Slots_history.(content cell) with
      | Dal.Slots_history.Unpublished _ -> `Unpublished
      | Published {is_proto_attested; attestation_lag; _} ->
          let lag = Dal.Slots_history.attestation_lag_value attestation_lag in
          if is_proto_attested then `Attested lag else `Unattested
  end

  module RPC = struct
    let directory skip_list_cells_store =
      RPC_directory.directory skip_list_cells_store
  end
end

let () = Dal_plugin.register (module Plugin)
