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
  match !datadir with
  | None ->
      Events.(emit node_uninitialized) () >>= fun () ->
      Lwt_exit.exit_and_raise 1
  | Some m -> Lwt.return m

let init dir = datadir := Some dir

(* An optimization trick to avoid allocating meaningless promisses. *)
let then_false () = Lwt.return_false

let compiler_name = "tezos-protocol-compiler"

let do_compile hash p =
  get_datadir () >>= fun datadir ->
  let source_dir = datadir // Protocol_hash.to_short_b58check hash // "src" in
  let log_file = datadir // Protocol_hash.to_short_b58check hash // "LOG" in
  let plugin_file =
    datadir
    // Protocol_hash.to_short_b58check hash
    // Format.asprintf "protocol_%a" Protocol_hash.pp hash
  in
  ( Tezos_base_unix.Protocol_files.write_dir source_dir ~hash p >>=? fun () ->
    let compiler_command =
      ( Sys.executable_name,
        Array.of_list
          [compiler_name; "-register"; "-o"; plugin_file; source_dir] )
    in
    let fd = Unix.(openfile log_file [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
    Lwt_process.exec
      ~stdin:`Close
      ~stdout:(`FD_copy fd)
      ~stderr:(`FD_move fd)
      compiler_command
    >>= return )
  >>= function
  | Error err -> Events.(emit compiler_exit_error) err >>= then_false
  | Ok (Unix.WSIGNALED _ | Unix.WSTOPPED _) ->
      Events.(emit compilation_interrupted) log_file >>= then_false
  | Ok (Unix.WEXITED x) when x <> 0 ->
      Events.(emit compilation_error) log_file >>= then_false
  | Ok (Unix.WEXITED _) -> (
      try
        Dynlink.loadfile_private (plugin_file ^ ".cmxs") ;
        Lwt.return_true
      with Dynlink.Error err ->
        Events.(emit dynlink_error) (plugin_file, Dynlink.error_message err)
        >>= then_false)

let compile hash p =
  if Tezos_protocol_registerer.Registerer.mem hash then Lwt.return_true
  else
    do_compile hash p >>= fun success ->
    let loaded = Tezos_protocol_registerer.Registerer.mem hash in
    if success && not loaded then
      Events.(emit internal_error) hash >>= fun () -> Lwt.return loaded
    else Lwt.return loaded
