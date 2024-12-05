(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Driver_kind = struct
  type t = OpenTelemetry | Prometheus | Text | Json

  let of_string s =
    match String.lowercase_ascii s with
    | "opentelemetry" -> OpenTelemetry
    | "prometheus" -> Prometheus
    | "text" -> Text
    | "json" -> Json
    | s -> failwith (Printf.sprintf "'%s' is not a known driver kind" s)
end

type t = Driver_kind.t list

let empty = []

let is_empty t = t = []

let of_list = Fun.id

let of_string s =
  String.split_on_char ';' s |> List.map String.trim
  |> List.map Driver_kind.of_string

let mem driver (t : t) = List.mem driver t

let exists = List.exists
