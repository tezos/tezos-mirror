(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Every function of this file should check the feature flag. *)

open Alpha_context
open Dal_errors

(* Use this function to select the pkh used in the DAL committee. As long as an
   epoch does not span across multiple cycles, we could use as well the pkh of
   the consensus key. *)
let pkh_of_consensus_key (consensus_key : Consensus_key.pk) =
  consensus_key.delegate

let validate_attestations ctxt level slot consensus_key attestations =
  let open Lwt_result_syntax in
  let*? () = Dal.assert_feature_enabled ctxt in
  let number_of_slots = Constants.dal_number_of_slots ctxt in
  let number_of_lags = Constants.dal_number_of_lags ctxt in
  let maximum_size =
    Dal.Attestations.expected_max_size_in_bits ~number_of_slots ~number_of_lags
  in
  let size = Dal.Attestations.occupied_size_in_bits attestations in
  let*? () =
    error_unless
      Compare.Int.(size <= maximum_size)
      (Dal_attestation_size_limit_exceeded {maximum_size; got = size})
  in
  let number_of_shards = Constants.dal_number_of_shards ctxt in
  (* TODO-DL: check for each lag index *)
  fail_when
    (Compare.Int.(Slot.to_int slot >= number_of_shards)
    && not (Dal.Attestations.is_empty attestations))
    (let attester = pkh_of_consensus_key consensus_key in
     Dal_data_availibility_attester_not_in_committee {attester; level; slot})

let apply_attestations ctxt ~delegate ~attested_level attestations ~tb_slot
    ~committee_level_to_shard_count =
  let open Result_syntax in
  let* () = Dal.assert_feature_enabled ctxt in
  let ctxt =
    Dal.only_if_incentives_enabled
      ctxt
      ~default:(fun ctxt -> ctxt)
      (fun ctxt ->
        Dal.Attestations.record_attestation ctxt ~tb_slot attestations)
  in
  return
    (Dal.Attestations.record_number_of_attested_shards
       ctxt
       ~delegate
       ~attested_level
       attestations
       committee_level_to_shard_count)

(* This function should fail if we don't want the operation to be
   propagated over the L1 gossip network. Because this is a manager
   operation, there are already checks to ensure the source of
   operation has enough fees. Among the various checks, there are
   checks that cannot fail unless the source of the operation is
   malicious (or if there is a bug). In that case, it is better to
   ensure fees will be taken. *)
let validate_publish_commitment ctxt _operation =
  Dal.assert_feature_enabled ctxt

let apply_publish_commitment ctxt operation ~source =
  let open Result_syntax in
  let* ctxt = Gas.consume ctxt Dal_costs.cost_Dal_publish_commitment in
  let number_of_slots = Constants.dal_number_of_slots ctxt in
  let* ctxt, cryptobox = Dal.make ctxt in
  let current_level = (Level.current ctxt).level in
  let* slot_header =
    Dal.Operations.Publish_commitment.slot_header
      ~cryptobox
      ~number_of_slots
      ~current_level
      operation
  in
  let* ctxt = Dal.Slot.register_slot_header ctxt slot_header ~source in
  return (ctxt, slot_header)

let record_participation ctxt delegate tb_slot slot_availability =
  let open Lwt_result_syntax in
  let*? () = Dal.assert_feature_enabled ctxt in
  Dal.only_if_incentives_enabled
    ctxt
    ~default:(fun ctxt -> return ctxt)
    (fun ctxt ->
      let attestation_lags = Constants.dal_attestation_lags ctxt in
      let number_of_lags = List.length attestation_lags in
      let number_of_slots = Constants.dal_number_of_slots ctxt in
      let number_of_slots_attested_by_delegate =
        match
          Slot.Map.find_opt tb_slot (Dal.Attestations.attestations ctxt)
        with
        | None -> 0
        | Some delegate_attestations ->
            (* Note: there is no double-counting because [dal_attestations] are
                 different in each block. *)
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/8218
                 This is not sufficient (too restrictive) for dynamic lags: we
                 need to aggregate the baker's attestations during the whole
                 attestation window, and then perform the intersection with the
                 protocol-attested slots.  *)
            Dal.Slot_availability.(
              intersection
                slot_availability
                delegate_attestations
                ~number_of_slots
                ~attestation_lags
              |> number_of_attested_slots ~number_of_lags)
      in
      let number_of_protocol_attested_slots =
        Dal.Slot_availability.number_of_attested_slots
          slot_availability
          ~number_of_lags
      in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/8218
           Similarly to the above, [number_of_protocol_attested_slots] may
           include slots at levels which are not attestable by the delegate
           because he's not in the committee? *)
      Delegate.record_dal_participation
        ctxt
        ~delegate
        ~number_of_slots_attested_by_delegate
        ~number_of_protocol_attested_slots)

let finalisation ctxt =
  let open Lwt_result_syntax in
  Dal.only_if_feature_enabled
    ctxt
    ~default:(fun ctxt -> return (ctxt, Dal.Slot_availability.empty))
    (fun ctxt ->
      let*! ctxt = Dal.Slot.finalize_current_slot_headers ctxt in
      (* The fact that slots confirmation is done at finalization is very
         important for the assumptions made by the Dal refutation game. In fact:
         - {!Dal.Slot.finalize_current_slot_headers} updates the Dal skip list
         at block finalization, by inserting newly confirmed slots;
         - {!Sc_rollup.Game.initial}, called when applying a manager operation
         that starts a refutation game, makes a snapshot of the Dal skip list
         to use it as a reference if the refutation proof involves a Dal input.

         If confirmed Dal slots are inserted into the skip list during operations
         application, adapting how refutation games are made might be needed
         to e.g.,
         - use the same snapshotted skip list as a reference by L1 and rollup-node;
         - disallow proofs involving pages of slots that have been confirmed at the
           level where the game started.
      *)
      let+ ctxt, slot_availability =
        Dal.Slot.finalize_pending_slot_headers ctxt
      in
      (ctxt, slot_availability))
