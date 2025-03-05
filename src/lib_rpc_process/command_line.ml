(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module provides a common interface for the command line of the
   rpc-process-watchdog and rpc-process-hypervisor processes. *)

let parse_args name =
  let socket_dir = ref None in
  let args =
    Arg.
      [
        ( "--socket-dir",
          String
            (fun s ->
              if not (Sys.file_exists s && Sys.is_directory s) then
                raise
                  (Arg.Bad
                     (Format.sprintf "File '%s' is not a valid directory" s))
              else socket_dir := Some s),
          {|<dir>
      The rpc-process-watchdog will communicate through a socket located
      at '<dir>/octez-rpc-process-watchdog-socket-<pid>' where <pid> is the
      watchdog-process' process identifier.|}
        );
        ( "--version",
          Unit
            (fun () ->
              Format.printf
                "%s\n"
                Tezos_version_value.Bin_version.octez_version_string ;
              Stdlib.exit 0),
          " Display version information" );
      ]
  in
  let usage_msg = Format.sprintf "%s [--version] [--socket-dir <dir>]" name in
  Arg.parse
    args
    (fun s -> raise (Arg.Bad (Format.sprintf "Unexpected argument: %s" s)))
    usage_msg ;
  match !socket_dir with
  | Some s -> s
  | None ->
      raise (Arg.Bad (Format.sprintf "%s: please provide --socket-dir" name))

let run name main =
  let socket_dir = parse_args name in
  let main_promise = main ~socket_dir in
  Stdlib.exit
    (let open Lwt_syntax in
     Lwt.Exception_filter.(set handle_all_except_runtime) ;
     Tezos_base_unix.Event_loop.main_run (fun () ->
         let* r = Lwt_exit.wrap_and_exit main_promise in
         match r with
         | Ok () -> Lwt_exit.exit_and_wait 0
         | Error err ->
             Format.eprintf "%a\n%!" pp_print_trace err ;
             Lwt_exit.exit_and_wait 1))

module Watchdog = struct
  let run () = run "octez-rpc-process-watchdog" External_watchdog.main
end

module Hypervisor = struct
  let run () =
    run "octez-rpc-process-hypervisor" External_watchdog.Hypervisor.main
end
