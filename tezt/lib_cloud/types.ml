(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Os = struct
  type t = Cos | Debian

  let default = Cos

  let of_string_exn = function
    | "cos" -> Cos
    | "debian" -> Debian
    | _ -> invalid_arg "Os.of_string"

  let of_string_opt str =
    try of_string_exn str |> Option.some with Invalid_argument _ -> None

  let to_string = function Cos -> "cos" | Debian -> "debian"

  let typ = Clap.typ ~name:"os" ~dummy:Cos ~parse:of_string_opt ~show:to_string

  let encoding =
    let open Data_encoding in
    conv to_string of_string_exn string
end
