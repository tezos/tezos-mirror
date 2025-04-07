(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module permits to log using colors when the output is a terminal. It
    is base on [Format].

    Some semantic tags are defined to handle ANSI color and blinking:
    - <yellow>
    - <red>
    - <blue>
    - <green>
    - <cyan>
    - <magenta>
    - <blinking>
*)

(* Same as [Format.printf]. *)
val printf : ('a, Format.formatter, unit) format -> 'a

(* Same as [Format.eprintf]. *)
val eprintf : ('a, Format.formatter, unit) format -> 'a

(* Same as [Format.printf] but add a newline at the end. *)
val printfln : ('a, Format.formatter, unit) format -> 'a

(* Same as [Format.eprintf] but add a newline at the end. *)
val eprintfln : ('a, Format.formatter, unit) format -> 'a

(* [error ?name msg] prints ["Error:"] followed by [msg] in red and a newline
   in the standard error.
   If [~name] is provided [msg] is prefixed by [~name] in yellow. *)
val error : ?name:string -> ('a, Format.formatter, unit) format -> 'a

(* [warning msg] prints ["Warning:"] followed by [msg] in yellow and a newline
   in the standard error. *)
val warning : ('a, Format.formatter, unit) format -> 'a

(* Same as [printfln] but prints in blue. *)
val blue : ('a, Format.formatter, unit) format -> 'a

(* Same as [printfln] but prints in green. *)
val green : ('a, Format.formatter, unit) format -> 'a

(* Same as [printfln] but prints in cyan. *)
val cyan : ('a, Format.formatter, unit) format -> 'a

(* Same as [printfln] but prints in yellow. *)
val yellow : ('a, Format.formatter, unit) format -> 'a

(* Same as [printfln] but prints in magenta. *)
val magenta : ('a, Format.formatter, unit) format -> 'a
