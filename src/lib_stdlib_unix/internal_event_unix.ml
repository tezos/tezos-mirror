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

open Error_monad

module Configuration = struct
  type t = {active_sinks : Uri.t list}

  let default =
    {
      active_sinks = [Uri.make ~scheme:Internal_event.Lwt_log_sink.uri_scheme ()];
    }

  let encoding =
    let open Data_encoding in
    let object_1 key_name =
      obj1
        (dft
           key_name
           ~description:"List of URIs to activate/configure sinks."
           (list string)
           [])
    in
    union
      [
        case
          ~title:"Active-Sinks"
          ~description:"List of sinks to make sure are activated."
          (Tag 0)
          (object_1 "active_sinks")
          (fun {active_sinks} -> Some (List.map Uri.to_string active_sinks))
          (fun active_sinks ->
            {active_sinks = List.map Uri.of_string active_sinks});
        case
          ~title:"Active-Sinks-Deprecated"
          ~description:
            "List of sinks to make sure are activated, deprecated \
             backwards-compatibility encoding."
          (Tag 1)
          (object_1 "activate")
          (fun {active_sinks = _} -> None)
          (* (fun {active_sinks} -> Some (List.map Uri.to_string active_sinks)) *)
            (fun active_sinks ->
            {active_sinks = List.map Uri.of_string active_sinks});
      ]

  let of_file path =
    let open Lwt_tzresult_syntax in
    let* json = Lwt_utils_unix.Json.read_file path in
    protect (fun () -> return (Data_encoding.Json.destruct encoding json))

  let apply {active_sinks} =
    List.iter_es Internal_event.All_sinks.activate active_sinks

  let reapply config =
    let except u = Uri.scheme u = Some "lwt-log" in
    Internal_event.All_sinks.close ~except () >>=? fun () -> apply config
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
