(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let now_bytes () =
  let now = Ptime_clock.now () in
  let now = Ptime.to_rfc3339 now in
  let timestamp = Time.Protocol.of_notation_exn now in
  Time.Protocol.to_seconds timestamp
  |> Z.of_int64 |> Z.to_bits |> Bytes.of_string

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6692
   Allow large blueprints in the kernel.
   For now, we only produce one chunk.
*)
let make_blueprint_chunks ~transactions =
  let open Rlp in
  let messages =
    List
      (List.map
         (fun transaction ->
           let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
           List
             [
               Value (Bytes.of_string tx_hash_str);
               List
                 [
                   Value (Bytes.of_string "\001");
                   Value (Bytes.of_string transaction);
                 ];
             ])
         transactions)
  in
  let timestamp = Value (now_bytes ()) in
  List [messages; timestamp] |> encode

let create ~smart_rollup_address ~number ~transactions =
  let open Rlp in
  let number = Value (encode_u256_le number) in
  let chunk = make_blueprint_chunks ~transactions in
  let rlp_sequencer_blueprint =
    List [Value chunk; number] |> encode |> Bytes.to_string
  in
  [
    "\001" (* External message *) ^ "\000"
    (* Framed protocol *) ^ smart_rollup_address
    ^ "\003"
    ^ (* Sequencer blueprint *)
    rlp_sequencer_blueprint;
  ]
