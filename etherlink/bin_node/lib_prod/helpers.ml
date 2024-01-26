(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let now () =
  let now = Ptime_clock.now () in
  let now = Ptime.to_rfc3339 now in
  Time.Protocol.of_notation_exn now

let timestamp_to_bytes timestamp =
  let seconds = Time.Protocol.to_seconds timestamp in
  let buffer = Bytes.make 8 '\000' in
  Bytes.set_int64_le buffer 0 seconds ;
  buffer
