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

let create ~smart_rollup_address ~number ~transactions =
  let timestamp = Rlp.Value (now_bytes ()) in
  let messages =
    Rlp.List
      (List.map
         (fun transaction -> Rlp.Value (Bytes.of_string transaction))
         transactions)
  in
  let number = Rlp.Value (encode_u256_le number) in
  let rlp_blueprint =
    Rlp.List [messages; timestamp; number] |> Rlp.encode |> Bytes.to_string
  in

  [
    "\001" (* External message *) ^ "\000"
    (* Framed protocol *) ^ smart_rollup_address
    ^ "\003"
    ^ (* Blueprint *)
    rlp_blueprint;
  ]
