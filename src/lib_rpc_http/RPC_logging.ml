(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <marcin.pastudzki@tqtezos.com> *)
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

let section = Internal_event.Section.make_sanitized ["rpc"]

module Event_def = struct
  type t = {level : Internal_event.level; message : string}

  let name = "rpc_http_event"

  let doc = "A logging event emitted by the RPC HTTP server."

  let pp ~short:_ fmt {message; _} = Format.fprintf fmt "%s" message

  let encoding =
    let open Data_encoding in
    let enc : (Internal_event.level * string) encoding =
      obj2 (req "level" Internal_event.Level.encoding) (req "message" string)
    in
    conv
      (fun t -> (t.level, t.message))
      (fun (level, message) -> {level; message})
      enc

  let level {level; _} = level

  let section = Some section
end

module Event = Internal_event.Make (Event_def)

let emit level message =
  let open Lwt_syntax in
  let* r = Event.emit ~section (fun () -> {level; message}) in
  match r with
  | Ok () -> return_unit
  | Error e -> Format.kasprintf Lwt.fail_with "%a" pp_print_trace e

(** Wrap an lwt computation so that it can return without waiting until the promise
    is resolved. *)
let wrap_lwt f a = Lwt.dont_wait (fun () -> f a) raise

(** Avoid calling emit, if sinks would ignore the message anyway. *)
let if_level_appropriate_or_else ~level if_so if_not fmt =
  let lwt_level =
    Lwt_log_core.Section.level @@ Internal_event.Section.to_lwt_log section
  in
  if Internal_event.Level.to_lwt_log level >= lwt_level then if_so fmt
  else if_not fmt

let log level fmt =
  if_level_appropriate_or_else
    ~level
    (Format.kasprintf (wrap_lwt @@ emit level))
    (Format.ifprintf Format.std_formatter)
    fmt

let log_lwt level fmt =
  if_level_appropriate_or_else
    ~level
    (Format.kasprintf (emit level))
    (Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter)
    fmt

(* External interface *)
let debug fmt = log Debug fmt

let log_info fmt = log Info fmt

let log_notice fmt = log Notice fmt

let warn fmt = log Warning fmt

let log_error fmt = log Error fmt

let lwt_debug fmt = log_lwt Debug fmt

let lwt_log_info fmt = log_lwt Info fmt

let lwt_log_notice fmt = log_lwt Notice fmt

let lwt_warn fmt = log_lwt Warning fmt

let lwt_log_error fmt = log_lwt Error fmt
