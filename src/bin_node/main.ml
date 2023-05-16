(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let warn_if_argv0_name_not_octez () =
  let executable_name = Filename.basename Sys.argv.(0) in
  let prefix = "tezos-" in
  if TzString.has_prefix executable_name ~prefix then
    let expected_name =
      let len_prefix = String.length prefix in
      "octez-"
      ^ String.sub
          executable_name
          len_prefix
          (String.length executable_name - len_prefix)
    in
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
       The executable with name @{<kwd>%s@} has been renamed to @{<kwd>%s@}. \
       The name @{<kwd>%s@} is now@,\
       deprecated, and it will be removed in a future release. Please update@,\
       your scripts to use the new name.@]@\n\
       @."
      executable_name
      expected_name
      executable_name
  else ()

let () =
  Printexc.register_printer @@ function
  | Unix.Unix_error (code, "", _) -> Some (Unix.error_message code)
  | Unix.Unix_error (code, function_name, "") ->
      Some (Printf.sprintf "in %s: %s" function_name (Unix.error_message code))
  | Unix.Unix_error (code, function_name, arguments) ->
      Some
        (Printf.sprintf
           "in %s %S: %s"
           function_name
           arguments
           (Unix.error_message code))
  | _ -> None

let () =
  (* The default allocation policy of Octez is "best-fit" which gives
     the best compromise in terms of performances and memory
     consumption. This default policy can be changed if the user set
     an environment variable. *)
  (* Any change to this constant should be replicated into the
     external validator in [src/bin_validation/main_validator.ml]. *)
  let default_allocation_policy = 2 in
  let current = Gc.get () in
  (match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> Gc.set {current with allocation_policy = default_allocation_policy}
  | Some _ -> ()) ;
  if (Gc.get ()).allocation_policy <> default_allocation_policy then
    Format.eprintf
      "WARNING: Default allocation policy changed: %d (default %d)@."
      current.allocation_policy
      default_allocation_policy

(* This can be removed once the protocol is fixed
   (currently there is [to_int Int32.max_int] which is obviously invalid). *)
let () =
  if Sys.word_size <> 64 then (
    prerr_endline "Non-64 bit architectures are not supported." ;
    exit 1)

let () = warn_if_argv0_name_not_octez ()

let () =
  if Filename.basename Sys.argv.(0) = Updater.compiler_name then (
    try
      Octez_protocol_compiler.Compiler.main
        Octez_protocol_compiler_native.Native.driver ;
      Stdlib.exit 0
    with exn ->
      Format.eprintf "%a\n%!" Opterrors.report_error exn ;
      Stdlib.exit 1)

let () =
  if Filename.basename Sys.argv.(0) = "octez-validator" then
    Tezos_validation.Command_line.run ()

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let description =
  [
    `S "DESCRIPTION";
    `P "Entry point for initializing, configuring and running a Tezos node.";
    `P Node_identity_command.Manpage.command_description;
    `P Node_run_command.Manpage.command_description;
    `P Node_replay_command.Manpage.command_description;
    `P Config_command.Manpage.command_description;
    `P Node_upgrade_command.Manpage.command_description;
    `P Node_snapshot_command.Manpage.command_description;
    `P Node_reconstruct_command.Manpage.command_description;
    `P Node_storage_command.Manpage.command_description;
  ]

let man = description @ Node_run_command.Manpage.examples

let info =
  let version = Tezos_version_value.Bin_version.version_string in
  Cmdliner.Cmd.info ~doc:"The Octez node" ~man ~version "octez-node"

module Node_metrics_command = struct
  let dump_metrics () =
    let open Prometheus in
    let open Lwt_syntax in
    let metric_type_to_string = function
      | Counter -> "Counter"
      | Gauge -> "Gauge"
      | Summary -> "Summary"
      | Histogram -> "Histogram"
    in
    let pp_label_names fmt =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        (fun fmt v -> Format.fprintf fmt "%a" LabelName.pp v)
        fmt
    in
    Format.printf "@[<v>Name,Type,Description,Labels" ;
    let* metrics = Prometheus.CollectorRegistry.(collect default) in
    let print_metrics metrics =
      List.iter
        (fun (v, _) ->
          Format.printf
            "@,@[%a@],%s,\"%s\",%a"
            MetricName.pp
            v.MetricInfo.name
            (metric_type_to_string v.MetricInfo.metric_type)
            v.MetricInfo.help
            pp_label_names
            v.MetricInfo.label_names)
        (Prometheus.MetricFamilyMap.to_list metrics)
    in
    print_metrics metrics ;
    Format.printf "@]@." ;
    return_unit

  let dump_metrics () = Lwt_main.run (dump_metrics ())

  module Term = struct
    let process _ = `Ok (dump_metrics ())

    let docs = "METRICS OPTIONS"

    let dump_metrics =
      let doc = "Show available openmetrics in csv format." in
      Cmdliner.Arg.(value & flag & info ~docs ~doc ["dump-metrics"])

    let term = Cmdliner.Term.(ret (const process $ dump_metrics))
  end

  module Manpage = struct
    let command_description =
      "The $(b,dump-metrics) command is meant to dump openmetrics that are \
       collected by the Tezos node on console."

    let description = [`S "DESCRIPTION"; `P command_description]

    let man = description

    let info =
      Cmdliner.Cmd.info
        ~doc:"Show all the openmetrics collected by the Tezos node"
        ~man
        "dump-metrics"
  end

  let cmd = Cmdliner.Cmd.v Manpage.info Term.term
end

let commands =
  Cmdliner.Cmd.group
    ~default:term
    info
    [
      Node_run_command.cmd;
      Node_replay_command.cmd;
      Config_command.cmd;
      Node_identity_command.cmd;
      Node_upgrade_command.cmd;
      Node_snapshot_command.cmd;
      Node_reconstruct_command.cmd;
      Node_storage_command.cmd;
      Node_metrics_command.cmd;
    ]

let () =
  Random.self_init () ;
  exit (Cmdliner.Cmd.eval commands)
