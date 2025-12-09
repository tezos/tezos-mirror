(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type PRINTER = sig
  val ln : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val errorln : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end

type t = (module PRINTER)

let lterm_printer term : (module PRINTER) =
  (module struct
    open LTerm_style

    let ln fmt =
      Format.kasprintf
        (fun l -> LTerm.fprintls term LTerm_text.(eval [S l]))
        fmt

    let errorln fmt =
      Format.kasprintf
        (fun l -> LTerm.fprintls term LTerm_text.(eval [B_fg lred; S l; E_fg]))
        fmt
  end)

let format_printer f : (module PRINTER) =
  (module struct
    open Format

    let ln fmt = kasprintf (fun str -> Lwt.return (fprintf f "%s@." str)) fmt

    let errorln fmt =
      kasprintf (fun str -> Lwt.return (fprintf f "%s@." str)) fmt
  end)

let ln (module P : PRINTER) = P.ln

let errorln (module P : PRINTER) = P.errorln
