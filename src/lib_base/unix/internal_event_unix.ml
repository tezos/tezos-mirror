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
    let open Lwt_tzresult_syntax in
    let* json = Lwt_utils_unix.Json.read_file path in
    protect (fun () -> return (Data_encoding.Json.destruct encoding json))
end

let env_var_name = "TEZOS_EVENTS_CONFIG"

let init ?lwt_log_sink ?(configuration = Configuration.default) () =
  let _ =
    (* This is just here to force the linking (and hence
       initialization) of all these modules: *)
    [
      File_descriptor_sink.Sink_implementation_path.uri_scheme;
      File_event_sink.Sink_implementation.uri_scheme;
    ]
  in
  Lwt_tzresult_syntax.(
    let* () =
      Lwt_result.ok @@ Lwt_log_sink_unix.initialize ?cfg:lwt_log_sink ()
    in
    let* () =
      match Sys.(getenv_opt env_var_name) with
      | None -> return_unit
      | Some s ->
          let uris =
            TzString.split ' ' s
            |> List.concat_map (TzString.split '\n')
            |> List.concat_map (TzString.split '\t')
            |> List.filter (( <> ) "")
            |> List.map Uri.of_string
          in
          let* () =
            List.iter_es
              (fun uri ->
                match Uri.scheme uri with
                | None ->
                    let* cfg = Configuration.of_file (Uri.path uri) in
                    Configuration.apply cfg
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
    Configuration.apply configuration)
  >>= function
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "ERROR@ Initializing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_trace
        el

let close () =
  Internal_event.All_sinks.close () >>= function
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "ERROR@ closing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_trace
        el
