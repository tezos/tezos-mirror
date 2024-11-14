(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {pid : int; ic : Lwt_io.input_channel; oc : Lwt_io.output_channel}

let input_channel t = t.ic

let output_channel t = t.oc

let pid t = t.pid

(** Communication error between the main and forked process *)
type error += Process_worker_ipc_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.process_worker.ipc_error"
    ~title:"IPC error between main process and worker process"
    ~description:"IPC error between main process and worker process"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "IPC error between main process and worker process: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Process_worker_ipc_error str -> Some str | _ -> None)
    (fun str -> Process_worker_ipc_error str)

let read_message ic =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! len = Lwt_io.read_int ic in
      let msg = Bytes.create len in
      let*! () = Lwt_io.read_into_exactly ic msg 0 len in
      return (`Message msg))
    (function
      (* On incomplete message, we let the caller decide how to handle the error,
         either retrying, ignoring, etc.*)
      | End_of_file -> return `End_of_file
      (* Other errors are fatal *)
      | exn ->
          fail (Process_worker_ipc_error "read_message" :: [error_of_exn exn]))

let write_message oc msg =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let len = Bytes.length msg in
      let*! () = Lwt_io.write_int oc len in
      let*! () = Lwt_io.write_from_exactly oc msg 0 len in
      return `Write_ok)
    (function
      (* On incomplete message, we let the caller decide how to handle the error,
         either retrying, ignoring, etc.*)
      | End_of_file -> return `End_of_file
      (* Other errors are fatal *)
      | exn ->
          fail (Process_worker_ipc_error "write_message" :: [error_of_exn exn]))

let run f args =
  let open Lwt_result_syntax in
  let*! () = Lwt_io.flush_all () in
  let ic_parent, oc_child = Lwt_io.pipe ~cloexec:true () in
  let ic_child, oc_parent = Lwt_io.pipe ~cloexec:true () in
  match Lwt_unix.fork () with
  | 0 ->
      (* Child *)

      (* Close useless ends *)
      let*! () = Lwt_io.close ic_parent in
      let*! () = Lwt_io.close oc_parent in

      let run = f ic_child oc_child args in

      (* Adds signal handlers in this child process *)
      let (_ : Lwt_exit.clean_up_callback_id) =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            let () = Lwt.cancel run in
            Lwt.return_unit)
      in
      let* () = run in
      exit 0
  | pid ->
      (* Parent *)
      let*! () = Lwt_io.close ic_child in
      let*! () = Lwt_io.close oc_child in
      return {pid; ic = ic_parent; oc = oc_parent}
