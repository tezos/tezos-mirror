(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Tezos_error_monad.TzLwtreslib
open Tezos_event_logging
open Error_monad

module Configuration = struct
  include Tezos_base.Internal_event_config

  let of_file path =
    let open Lwt_result_syntax in
    let* json = Lwt_utils_unix.Json.read_file path in
    protect (fun () -> return (Data_encoding.Json.destruct encoding json))
end

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4837

   This environment variable is problematic when the octez-node in not
   run with [Singleprocess]. *)
let env_var_name = "TEZOS_EVENTS_CONFIG"

let init_raw ~internal_events () =
  let _ =
    (* This is just here to force the linking (and hence
       initialization) of all these modules: *)
    [
      File_descriptor_sink.Sink_implementation_path.uri_scheme;
      File_event_sink.Sink_implementation.uri_scheme;
    ]
  in
  let open Lwt_result_syntax in
  let*! r =
    let* () =
      match Sys.(getenv_opt env_var_name) with
      | None -> return_unit
      | Some s ->
          let uris =
            TzString.split_no_empty ' ' s
            |> List.concat_map (TzString.split_no_empty '\n')
            |> List.concat_map (TzString.split_no_empty '\t')
            |> List.filter (( <> ) "")
            |> List.map Uri.of_string
          in
          let* () =
            List.iter_es
              (fun uri ->
                match Uri.scheme uri with
                | None ->
                    let* cfg = Configuration.of_file (Uri.path uri) in
                    Internal_event_config.apply cfg
                | Some _ -> Internal_event.All_sinks.activate uri)
              uris
          in
          Internal_event.Debug_event.(
            emit
              (make
                 "Loaded URIs from environment"
                 ~attach:
                   (`O
                     [("variable", `String env_var_name); ("value", `String s)])))
    in
    Configuration.apply internal_events
  in
  match r with
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "ERROR@ Initializing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_trace
        el

let close () =
  let open Lwt_syntax in
  let* r = Internal_event.All_sinks.close () in
  match r with
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "ERROR@ closing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_trace
        el

open Filename.Infix

let make_default_internal_events ~rules ~verbosity
    ~(log_output : Logs_simple_config.Output.t) ~daily_logs_path =
  (* By default the node has two logs output:
     - on the configured [log_output] using the configured [verbosity] and
       a short pretty printing
     - on disk, with a 7 days rotation with an info verbosity level and a
       standard pretty printing *)
  let origin, rules = Level_config_rules.find_log_rules rules in
  let section_prefixes =
    match rules with
    | Some rules -> (
        try Level_config_rules.parse_rules rules
        with _ ->
          Printf.ksprintf
            Stdlib.failwith
            "Incorrect log rules defined in %s"
            origin)
    | None -> []
  in
  let user_events =
    let kind =
      match log_output with
      | Stdout -> `Stdout
      | Stderr -> `Stderr
      | File fp -> `Path fp
      | Null -> `Null
      | Syslog _ -> `Syslog (Logs_simple_config.Output.to_string log_output)
    in
    Internal_event_config.make_config_uri
      ~level:verbosity
      ~section_prefixes
      ~format:"pp-short"
      kind
  in
  let sinks = [user_events] in
  let sinks =
    match daily_logs_path with
    | Some daily_logs_path ->
        let internal_logs_events =
          Internal_event_config.make_config_uri
            ~create_dirs:true
            ~daily_logs:7
            ~level:Info
            ~format:"pp-rfc5424"
            (`Path (daily_logs_path // "daily.log"))
        in
        internal_logs_events :: sinks
    | None -> sinks
  in
  Internal_event_config.make_custom sinks

let make_with_defaults ?verbosity ?enable_default_daily_logs_at
    ?(log_cfg = Logs_simple_config.default_cfg) () =
  make_default_internal_events
    ~rules:log_cfg.rules
    ~verbosity:(Option.value verbosity ~default:log_cfg.default_level)
    ~log_output:log_cfg.output
    ~daily_logs_path:enable_default_daily_logs_at

let init ?config:(internal_events = make_with_defaults ()) () =
  let open Lwt_syntax in
  let* () = init_raw ~internal_events () in
  return_unit
