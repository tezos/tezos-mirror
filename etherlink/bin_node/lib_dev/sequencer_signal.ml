(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding.Helpers

(* The sequencer signal follows the format:
    [ slot_index, <- 1 bytes
      published_level, <- 4 bytes
    ]
*)

let create ~cctxt ~sequencer_key ~smart_rollup_address ~slot_index
    ~published_level =
  let open Lwt_result_syntax in
  let open Rlp in
  let slot_index = Value (encode_u8 slot_index) in
  let published_level = Value (encode_i32_le published_level) in
  let rlp_unsigned_signal = List [slot_index; published_level] |> encode in
  let* signature = Client_keys.sign cctxt sequencer_key rlp_unsigned_signal in
  let signature_bytes = Signature.to_bytes signature in
  (* Encode the signals fields and its signature. *)
  let rlp_sequencer_signal =
    List [slot_index; published_level; Value signature_bytes]
    |> encode |> Bytes.to_string
  in
  `External
    ("\000" (* Framed protocol *) ^ smart_rollup_address
    ^ "\004"
    ^ (* Sequencer signal *)
    rlp_sequencer_signal)
  |> return
