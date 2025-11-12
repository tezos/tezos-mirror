(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Duration = struct
  type t = float (* seconds *)

  let of_ns s = Int64.to_float s /. 1_000_000.

  let seconds s = s

  let minutes s = seconds s /. 60.

  let hours s = minutes s /. 60.
end

type t = {
  count : int;
  total_duration : Duration.t;
  average_duration : Duration.t;
}

let make tests =
  let count = List.length tests in
  let total_duration =
    List.fold_left
      (fun acc test -> Int64.add acc (Record.duration_ns test))
      0L
      tests
    |> Duration.of_ns
  in
  {count; total_duration; average_duration = total_duration /. float count}
