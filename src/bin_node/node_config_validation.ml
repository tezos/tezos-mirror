(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* The type for a node configuration warning/error. *)

type alert =
  | Alert : {
      event : level:Internal_event.Level.t -> 'a Internal_event.Simple.t;
      level : Internal_event.Level.t;
      payload : 'a;
    }
      -> alert

let is_error (Alert {level; _}) = level = Error

let is_warning (Alert {level; _}) = level = Warning

(* The type for a configuration validation report. *)

type t = alert list

let empty = []

let has_error t = List.exists is_error t

let has_warning t = List.exists is_warning t

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "config"; "validation"]

  let error_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_error"
      ~msg:
        "found the following error(s) while validating the node configuration."
      ~level:Error
      ()

  let warning_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_warning"
      ~msg:
        "found the following warning(s) while validating the node \
         configuration."
      ~level:Warning
      ()

  let emit_all t =
    Lwt_list.iter_s
      (function Alert {level; event; payload} -> emit (event ~level) payload)
      t

  let report t =
    let errors = List.filter is_error t in
    let warnings = List.filter is_warning t in
    ( match errors with
    | [] ->
        Lwt.return_unit
    | xs ->
        emit error_event () >>= fun () -> emit_all xs )
    >>= fun () ->
    match warnings with
    | [] ->
        Lwt.return_unit
    | xs ->
        emit warning_event () >>= fun () -> emit_all xs
end

let mk_alert ~event ~level ~payload = Alert {event; level; payload}

let when_ condition ~event ~level ~payload =
  if not condition then [] else [mk_alert ~event ~level ~payload]
  [@@ocaml.warning "-32"]

let unless condition ~event ~level ~payload =
  if condition then [] else [mk_alert ~event ~level ~payload]
  [@@ocaml.warning "-32"]

(* The following parts consist in node configuration validations. *)

(* Main validation passes. *)

let validation_passes = []

let validate_passes config =
  List.fold_left_es
    (fun acc f -> f config >>=? fun res -> return (res @ acc))
    empty
    validation_passes

(* Main validation functions. *)

let validate_config (config : Node_config_file.t) : t tzresult Lwt.t =
  if config.disable_config_validation then return empty
  else validate_passes config

let check config =
  validate_config config
  >>=? fun t ->
  if has_error t then Event.report t >>= fun () -> exit 1
  else if has_warning t then Event.report t >>= fun () -> return_unit
  else return_unit
