(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Output = struct
  type t = Null | Stdout | Stderr | File of string | Syslog of Syslog.facility

  let to_string : t -> string = function
    | Null -> "/dev/null"
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | File fp -> fp
    | Syslog Auth -> "syslog:auth"
    | Syslog Authpriv -> "syslog:authpriv"
    | Syslog Cron -> "syslog:cron"
    | Syslog Daemon -> "syslog:daemon"
    | Syslog FTP -> "syslog:ftp"
    | Syslog Kernel -> "syslog:kernel"
    | Syslog Local0 -> "syslog:local0"
    | Syslog Local1 -> "syslog:local1"
    | Syslog Local2 -> "syslog:local2"
    | Syslog Local3 -> "syslog:local3"
    | Syslog Local4 -> "syslog:local4"
    | Syslog Local5 -> "syslog:local5"
    | Syslog Local6 -> "syslog:local6"
    | Syslog Local7 -> "syslog:local7"
    | Syslog LPR -> "syslog:lpr"
    | Syslog Mail -> "syslog:mail"
    | Syslog News -> "syslog:news"
    | Syslog Syslog -> "syslog:syslog"
    | Syslog User -> "syslog:user"
    | Syslog UUCP -> "syslog:uucp"
    | Syslog NTP -> "syslog:ntp"
    | Syslog Security -> "syslog:security"
    | Syslog Console -> "syslog:console"

  let of_string : string -> t = function
    | "/dev/null" | "null" -> Null
    | "stdout" -> Stdout
    | "stderr" -> Stderr
    | "syslog:auth" -> Syslog Auth
    | "syslog:authpriv" -> Syslog Authpriv
    | "syslog:cron" -> Syslog Cron
    | "syslog:daemon" -> Syslog Daemon
    | "syslog:ftp" -> Syslog FTP
    | "syslog:kernel" -> Syslog Kernel
    | "syslog:local0" -> Syslog Local0
    | "syslog:local1" -> Syslog Local1
    | "syslog:local2" -> Syslog Local2
    | "syslog:local3" -> Syslog Local3
    | "syslog:local4" -> Syslog Local4
    | "syslog:local5" -> Syslog Local5
    | "syslog:local6" -> Syslog Local6
    | "syslog:local7" -> Syslog Local7
    | "syslog:lpr" -> Syslog LPR
    | "syslog:mail" -> Syslog Mail
    | "syslog:news" -> Syslog News
    | "syslog:syslog" -> Syslog Syslog
    | "syslog:user" -> Syslog User
    | "syslog:uucp" -> Syslog UUCP
    | "syslog:ntp" -> Syslog NTP
    | "syslog:security" -> Syslog Security
    | "syslog:console" -> Syslog Console
    (* | s when start_with "syslog:" FIXME error or warning. *)
    | fp ->
        (* TODO check absolute path *)
        File fp

  let encoding =
    let open Data_encoding in
    conv to_string of_string string

  let of_string str =
    try Some (Data_encoding.Json.destruct encoding (`String str))
    with _ -> None

  let to_string output =
    match Data_encoding.Json.construct encoding output with
    | `String res -> res
    | #Data_encoding.json -> assert false

  let pp fmt output = Format.fprintf fmt "%s" (to_string output)
end

type cfg = {
  output : Output.t;
  default_level : Internal_event.level;
  rules : string option;
  colors : bool;
  advertise_levels : bool;
}

let create_cfg ?(output = Output.Stderr)
    ?(default_level = Internal_event.Notice) ?(colors = true)
    ?(advertise_levels = false) ?rules () =
  {output; default_level; rules; colors; advertise_levels}

let default_cfg = create_cfg ()

let cfg_encoding =
  let open Data_encoding in
  conv
    (fun {output; default_level; rules; colors; advertise_levels} ->
      (output, default_level, colors, rules, advertise_levels))
    (fun (output, default_level, colors, rules, advertise_levels) ->
      {output; default_level; rules; colors; advertise_levels})
    (obj5
       (dft
          "output"
          ~description:
            "Output for the logging function. Either 'stdout', 'stderr' or the \
             name of a log file ."
          Output.encoding
          default_cfg.output)
       (dft
          "level"
          ~description:
            "Verbosity level: one of 'fatal', 'error', 'warn','notice', \
             'info', 'debug'."
          Internal_event.Level.encoding
          default_cfg.default_level)
       (dft
          "colors"
          ~description:"Enables light coloring in logs."
          Data_encoding.bool
          default_cfg.colors)
       (opt
          "rules"
          ~description:
            "Fine-grained logging instructions. Same format as described in \
             `octez-node run --help`, DEBUG section. In the example below, \
             sections 'p2p' and all sections starting by 'client' will have \
             their messages logged up to the debug level, whereas the rest of \
             log sections will be logged up to the notice level."
          string)
       (dft
          "advertises_level"
          ~description:
            "When `true`, advertises the level of an event in addition to its \
             contents."
          bool
          default_cfg.advertise_levels))
