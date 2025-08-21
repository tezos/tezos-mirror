(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let get_cpu_count () =
  match open_in "/proc/cpuinfo" with
  | exception Sys_error msg ->
      echo
        "Warning: failed to open /proc/cpuinfo (%s). Consider passing --jobs."
        msg ;
      1
  | ch ->
      Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
      let rec loop count =
        match input_line ch with
        | exception End_of_file -> max 1 count
        | exception Sys_error msg ->
            echo
              "Warning: failed to read /proc/cpuinfo (%s). Consider passing \
               --jobs."
              msg ;
            max 1 count
        | line -> (
            match str_split_once line ':' with
            | None -> loop count
            | Some (key, _value) -> (
                match key |> String.trim |> String.lowercase_ascii with
                | "processor" -> loop (count + 1)
                | _ -> loop count))
      in
      loop 0
