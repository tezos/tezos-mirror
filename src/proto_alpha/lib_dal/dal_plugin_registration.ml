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
      (fun acc (pkh, s) -> Signature.Public_key_hash.Map.add pkh s acc)
      Signature.Public_key_hash.Map.empty
      pkh_to_shards

  let attested_slot_headers (block : block_info) ~number_of_slots =
    let open Result_syntax in
    let* metadata =
      Option.to_result
        block.metadata
        ~none:
          (TzTrace.make @@ Layer1_services.Cannot_read_block_metadata block.hash)
    in
    let confirmed_slots = metadata.protocol_data.dal_attestation in
    let* all_slots =
      Dal.Slot_index.slots_range
        ~number_of_slots
        ~lower:0
        ~upper:(number_of_slots - 1)
      |> wrap
    in
    List.filter (Dal.Attestation.is_attested confirmed_slots) all_slots
    |> Dal.Slot_index.to_int_list |> return

  (* Section of helpers for Skip lists *)

  module Skip_list = struct
    type cell = Dal.Slots_history.t

    type hash = Dal.Slots_history.Pointer_hash.t

    let cell_encoding = Dal.Slots_history.encoding

    let hash_encoding = Dal.Slots_history.Pointer_hash.encoding

    let cell_equal = Dal.Slots_history.equal

    let hash_equal = Dal.Slots_history.Pointer_hash.equal

    let cell_hash = Dal.Slots_history.hash

    let empty_cache = Dal.Slots_history.History_cache.empty ~capacity:0L

    (*
      This function mimics what the protocol does in
      {!Dal_slot_storage.finalize_pending_slot_headers}. Given a block_info and
      an RPC context, this function computes the cells produced by the DAL skip
      list during the level L of block_info using:

       - The information telling which slot headers were waiting for attestation
       at level [L - attestation_lag];

       - The bitset of attested slots at level [L] in the block's metadata.

      The ordering of the elements in the returned list is not relevant.
    *)
    let cells_of_level (block_info : block_info) ctxt =
      let open Lwt_result_syntax in
      (* 0. Let's call [level] the block's level. *)
      let level = block_info.header.shell.level in
      let* dal_constants = get_constants `Main (`Level level) ctxt in
      let published_level =
        Int32.sub level (Int32.of_int dal_constants.attestation_lag)
      in
      (* 1. Before it's possible to attest the slots at the first published
         levels, the list of cells is empty. *)
      if published_level < 0l then return []
      else
        let cpctxt = new Protocol_client_context.wrap_rpc_context ctxt in
        (* 2. We retrieve the slot headers published at level [level -
           attestation_lag] from the context. *)
        let* published_slot_headers =
          let*? published_level =
            Raw_level.of_int32 published_level |> Environment.wrap_tzresult
          in
          Plugin.RPC.Dal.dal_published_slot_headers
            cpctxt
            (`Main, `Level level)
            ~level:published_level
            ()
        in
        (* 3. We retrieve the last cell of the DAL skip list from the context,
           if any. It's the one stored in the context at [level - 1]. If no cell
           is stored yet, we return the genesis cell. *)
        let* previous_cell =
          let+ previous_cell_opt =
            (* Should not be negative as attestation_lag > 0. *)
            let prev_level = Int32.pred level in
            Plugin.RPC.Dal.dal_confirmed_slots_history
              cpctxt
              (`Main, `Level prev_level)
          in
          Option.value previous_cell_opt ~default:Dal.Slots_history.genesis
        in
        (* 4. We retrieve the bitset of attested slots at level [level]. *)
        let* attested_slots =
          let*? metadata =
            Option.to_result
              block_info.metadata
              ~none:
                (TzTrace.make
               @@ Layer1_services.Cannot_read_block_metadata block_info.hash)
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
        (* 6. Starting from the [previous_cell], we insert the successive cells
           of level [level] in the skip list thanks to function
           {!add_confirmed_slot_headers}. The function is fed with an empty
           history cache, so the returned [cache] contains exactly the cells
           produced for this [level]. *)
        let*? level = Raw_level.of_int32 level |> Environment.wrap_tzresult in
        let*? _last_cell, cache =
          Dal.Slots_history.add_confirmed_slot_headers
            previous_cell
            empty_cache
            level
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
end

let () = Dal_plugin.register (module Plugin)
