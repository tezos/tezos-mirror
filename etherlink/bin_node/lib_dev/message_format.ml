(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let framing_protocol = "\000"

(* The hard limit is 4096 but it needs to add the external message tag. *)
let max_input_size = 4095

let framing_protocol_tag_size = String.length framing_protocol

let smart_rollup_address_size = 20

let tag_size = 1

let header_size =
  framing_protocol_tag_size + smart_rollup_address_size + tag_size

let usable_size_in_message = max_input_size - header_size

type message_kind =
  | Simple_transaction
  | New_chunked_transaction
  | Transaction_chunk
  | Blueprint_chunk
  | Sequencer_signal

let message_kind_tag = function
  | Simple_transaction -> "\000"
  | New_chunked_transaction -> "\001"
  | Transaction_chunk -> "\002"
  | Blueprint_chunk -> "\003"
  | Sequencer_signal -> "\004"

let frame_message smart_rollup_address kind bytes =
  framing_protocol ^ smart_rollup_address ^ message_kind_tag kind ^ bytes

type dal_message_kind = Blueprint_chunk

let dal_message_kind_tag = function Blueprint_chunk -> "\001"

let frame_dal_message kind bytes = dal_message_kind_tag kind ^ bytes
