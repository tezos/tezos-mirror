(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type pattern = {string : string; regex : Re.re}

let pp_pattern fmt {string; _} = Format.pp_print_string fmt string

let protocol_pattern =
  let string = {|[a-z]+[0-9]*|} in
  {string; regex = Re.Perl.(compile @@ re ("^" ^ string ^ "$"))}

module Stabilise = struct
  let run _git_dir _protocol_source _protocol_target = ()
end

module Snapshot = struct
  let target_pattern =
    let string = {|[a-z]+_[0-9][0-9][0-9]|} in
    {string; regex = Re.Perl.(compile @@ re ("^" ^ string ^ "$"))}

  let run _git_dir _protocol_source _protocol_target = ()
end

module Hash = struct
  let run _git_dir _from = Log.warning "Updating hash is under development"
end
