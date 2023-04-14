(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Filename.Infix

let lwt_log_sink_default_cfg =
  {
    Lwt_log_sink_unix.default_cfg with
    template = "$(date).$(milliseconds): $(message)";
  }

let make_default_internal_events ~data_dir =
  (* By default the node has two logs output:
     - on stdout using Lwt_log using the configured verbosity
     - on disk, with a 7 days rotation with an info verbosity level *)
  Internal_event_config.make_custom
    [
      Uri.make ~scheme:Internal_event.Lwt_log_sink.uri_scheme ();
      Uri.make
        ~scheme:"file-descriptor-path"
        ~path:(data_dir // "daily-logs" // "daily.log")
        ~query:
          [
            ("create-dirs", ["true"]);
            ("daily-logs", ["7"]);
            ("section-prefix", [":info"]);
            ("format", ["pp"]);
          ]
        ();
    ]

let make_internal_events_with_defaults ?internal_events ?verbosity
    ?(log_cfg = lwt_log_sink_default_cfg) () =
  let log_cfg =
    match verbosity with
    | None -> log_cfg
    | Some default_level -> {log_cfg with Lwt_log_sink_unix.default_level}
  in
  let internal_events =
    match internal_events with
    | None -> Internal_event_config.lwt_log
    | Some (internal_events, data_dir) ->
        if Internal_event_config.(is_empty internal_events) then
          make_default_internal_events ~data_dir
        else internal_events
  in
  (log_cfg, internal_events)

let init_internal_events_with_defaults ?internal_events ?verbosity
    ?(log_cfg = lwt_log_sink_default_cfg) () =
  let lwt_log_sink, configuration =
    make_internal_events_with_defaults ?internal_events ?verbosity ~log_cfg ()
  in
  Tezos_base_unix.Internal_event_unix.init ~lwt_log_sink ~configuration ()
