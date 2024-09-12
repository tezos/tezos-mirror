(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding.Helpers

(*
   The sequencer signals follow the format:

   [
    [
      [ level_1 ; [ slot_indice_1; slot_indice_2; ...] ];
      [ level_2 ; [ slot_indice_1; slot_indice_2; ...] ];
      ....
    ]
    signature;
   ]
*)

(* Given an (int * int32) list of slot indices associated to levels, this
   function returns an associative list of type (int32 * int list) list, where
   each level is mapped to a list of slot indices. It's guaranteed that the list
   of slots indices is not empty and doesn't have duplicates. *)
let compact_levels_slots ~slot_ids =
  let module MI = Map.Make (Int32) in
  let module SI = Set.Make (Int) in
  let levels_to_slots =
    List.fold_left
      (fun levels_to_slots (slot_index, level) ->
        let slots_of_level =
          MI.find_opt level levels_to_slots |> Option.value ~default:SI.empty
        in
        MI.add level (SI.add slot_index slots_of_level) levels_to_slots)
      MI.empty
      slot_ids
  in
  MI.fold
    (fun level slots_level accu -> (level, SI.elements slots_level) :: accu)
    levels_to_slots
    []
  (* Keep the increasing for level ordering *)
  |> List.rev

let create ~cctxt ~sequencer_key ~smart_rollup_address ~slot_ids =
  let open Lwt_result_syntax in
  let open Rlp in
  let encoded_levels_to_slots =
    List
      (compact_levels_slots ~slot_ids
      |> List.map (fun (published_level, slots) ->
             let slots = List.map (fun idx -> Value (encode_int idx)) slots in
             List [Value (encode_i32_le published_level); List slots]))
  in
  let rlp_unsigned_signal = encoded_levels_to_slots |> encode in
  let* signature = Client_keys.sign cctxt sequencer_key rlp_unsigned_signal in
  let signature_bytes = Signature.to_bytes signature in
  (* Encode the signals fields and its signature. *)
  let rlp_sequencer_signal =
    List [encoded_levels_to_slots; Value signature_bytes]
    |> encode |> Bytes.to_string
  in
  `External
    Message_format.(
      frame_message
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
        Sequencer_signal
        rlp_sequencer_signal)
  |> return
