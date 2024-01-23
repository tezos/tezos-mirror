(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let amplify (shard_store : Store.Shards.t) commitment node_ctxt =
  let open Lwt_result_syntax in
  match Node_context.get_status node_ctxt with
  | Starting ->
      (* The cryptobox is not yet available so we cannot reconstruct
         slots yet. *)
      return_unit
  | Ready {cryptobox; _} ->
      let dal_parameters = Cryptobox.parameters cryptobox in
      let number_of_shards = dal_parameters.number_of_shards in
      let redundancy_factor = dal_parameters.redundancy_factor in
      let number_of_needed_shards = number_of_shards / redundancy_factor in
      let* number_of_already_stored_shards =
        Store.Shards.count_values shard_store commitment
      in
      (* There are two situations where we don't want to reconstruct:
         if we don't have enough shards or if we have more shards than
         needed to reconstruct, because in this case a reconstruction
         should have already been started previously. *)
      if number_of_already_stored_shards <> number_of_needed_shards then
        return_unit
      else
        (* We have enough shards to reconstruct the whole slot. *)
        let shards =
          Store.Shards.read_all shard_store commitment ~number_of_shards
          |> Seq_s.filter_map (function
                 | _, index, Ok share -> Some Cryptobox.{index; share}
                 | _ -> None)
        in
        let* _polynomial =
          Slot_manager_legacy.polynomial_from_shards_lwt
            cryptobox
            shards
            ~number_of_needed_shards
        in
        return_unit
