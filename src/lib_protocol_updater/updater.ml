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

open Filename.Infix
module Events = Updater_events

(** Compiler *)

let datadir = ref None

let get_datadir () =
  let open Lwt_syntax in
  match !datadir with
  | None ->
      let* () = Events.(emit node_uninitialized) () in
      Lwt_exit.exit_and_raise 1
  | Some m -> Lwt.return m

let init dir = datadir := Some dir

let compiler_name = "tezos-protocol-compiler"

let do_compile hash p =
  let open Lwt_syntax in
  let* datadir = get_datadir () in
  let source_dir = datadir // Protocol_hash.to_short_b58check hash // "src" in
  let log_file = datadir // Protocol_hash.to_short_b58check hash // "LOG" in
  let plugin_file =
    datadir
    // Protocol_hash.to_short_b58check hash
    // Format.asprintf "protocol_%a" Protocol_hash.pp hash
  in
  let* r = Tezos_base_unix.Protocol_files.write_dir source_dir ~hash p in
  match r with
  | Error err ->
      let* () = Events.(emit compiler_exit_error) err in
      Lwt.return_false
  | Ok () -> (
      let compiler_command =
        ( Sys.executable_name,
          Array.of_list
            [compiler_name; "-register"; "-o"; plugin_file; source_dir] )
      in
      let fd = Unix.(openfile log_file [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
      let* s =
        Lwt_process.exec
          ~stdin:`Close
          ~stdout:(`FD_copy fd)
          ~stderr:(`FD_move fd)
          compiler_command
      in
      match s with
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
          let* () = Events.(emit compilation_interrupted) log_file in
          Lwt.return_false
      | Unix.WEXITED x when x <> 0 ->
          let* () = Events.(emit compilation_error) log_file in
          Lwt.return_false
      | Unix.WEXITED _ -> (
          try
            Dynlink.loadfile_private (plugin_file ^ ".cmxs") ;
            Lwt.return_true
          with
          | Dynlink.(Error (Cannot_open_dynamic_library exn)) ->
              let* () =
                Events.(emit dynlink_error_static)
                  (plugin_file, Printexc.to_string exn)
              in
              Lwt.return_false
          | Dynlink.(Error err) ->
              let* () =
                Events.(emit dynlink_error)
                  (plugin_file, Dynlink.error_message err)
              in
              Lwt.return_false))

let compile hash p =
  let open Lwt_syntax in
  if Tezos_protocol_registerer.Registerer.mem hash then Lwt.return_true
  else
    let* success = do_compile hash p in
    let loaded = Tezos_protocol_registerer.Registerer.mem hash in
    if success && not loaded then
      let* () = Events.(emit internal_error) hash in
      Lwt.return loaded
    else Lwt.return loaded
