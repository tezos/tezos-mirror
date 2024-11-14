(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type error +=
  | Socket_path_too_long of string
  | Socket_path_wrong_permission of string
  | Cannot_create_socket of string

let () =
  register_error_kind
    `Temporary
    ~id:"external_process.socket_path_too_long"
    ~title:"Socket path too long"
    ~description:"Socket path too long."
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "The socket path %s is too long. Please set an alternative path using \
         XDG_RUNTIME_DIR to specify where to create the file. Use, for \
         instance, XDG_RUNTIME_DIR=/tmp/xdg-runtime-dir/"
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Socket_path_too_long p -> Some p | _ -> None)
    (fun p -> Socket_path_too_long p)

let () =
  register_error_kind
    `Temporary
    ~id:"external_process.socket_path_wrong_permission"
    ~title:"Wrong permission for socket path"
    ~description:"Wrong permission for socket path."
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "The socket path %s has wrong permissions. Please set an alternative \
         path using XDG_RUNTIME_DIR to specify where to create the file. Use, \
         for instance, XDG_RUNTIME_DIR=/tmp/xdg-runtime-dir/"
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Socket_path_wrong_permission p -> Some p | _ -> None)
    (fun p -> Socket_path_wrong_permission p)

let () =
  register_error_kind
    `Temporary
    ~id:"external_process.cannot_create_socket"
    ~title:"Cannot create socket"
    ~description:"Cannot create socket for external process."
    ~pp:(fun ppf error ->
      Format.fprintf
        ppf
        "Failed to create socket for external process: %s"
        error)
    Data_encoding.(obj1 (req "error" string))
    (function Cannot_create_socket e -> Some e | _ -> None)
    (fun e -> Cannot_create_socket e)

let send pin encoding data =
  let open Lwt_syntax in
  let msg = Data_encoding.Binary.to_bytes_exn encoding data in
  let* () = Lwt_io.write_int pin (Bytes.length msg) in
  let* () = Lwt_io.write pin (Bytes.to_string msg) in
  Lwt_io.flush pin

let recv_result pout encoding =
  let open Lwt_syntax in
  let* count = Lwt_io.read_int pout in
  let buf = Bytes.create count in
  let* () = Lwt_io.read_into_exactly pout buf 0 count in
  return
    (Data_encoding.Binary.of_bytes_exn
       (Error_monad.result_encoding encoding)
       buf)

let recv pout encoding =
  let open Lwt_syntax in
  let* count = Lwt_io.read_int pout in
  let buf = Bytes.create count in
  let* () = Lwt_io.read_into_exactly pout buf 0 count in
  Lwt.return (Data_encoding.Binary.of_bytes_exn encoding buf)

(* To get optimized socket communication of processes on the same
   machine, we use Unix domain sockets: ADDR_UNIX. *)
let make_socket socket_path = Unix.ADDR_UNIX socket_path

let create_socket ~canceler =
  let open Lwt_syntax in
  let socket = Lwt_unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0o000 in
  Lwt_unix.set_close_on_exec socket ;
  Lwt_canceler.on_cancel canceler (fun () ->
      let* (_ : unit tzresult) = Lwt_utils_unix.safe_close socket in
      return_unit) ;
  Lwt_unix.setsockopt socket SO_REUSEADDR true ;
  Lwt.return socket

let create_socket_listen ~canceler ~max_requests ~socket_path =
  let open Lwt_result_syntax in
  let*! socket = create_socket ~canceler in
  let* () =
    Lwt.catch
      (fun () ->
        let*! () = Lwt_unix.bind socket (make_socket socket_path) in
        return_unit)
      (function
        | Unix.Unix_error (ENAMETOOLONG, _, _) ->
            (* Unix.ENAMETOOLONG (Filename too long (POSIX.1-2001)) can
               be thrown if the given directory has a too long path. *)
            tzfail (Socket_path_too_long socket_path)
        | Unix.Unix_error (EACCES, _, _) ->
            (* Unix.EACCES (Permission denied (POSIX.1-2001)) can be
               thrown when the given directory has wrong access rights.
               Unix.EPERM (Operation not permitted (POSIX.1-2001)) should
               not be thrown in this case. *)
            tzfail (Socket_path_wrong_permission socket_path)
        | exn -> tzfail (Cannot_create_socket (Printexc.to_string exn)))
  in
  Lwt_unix.listen socket max_requests ;
  return socket

type error += Cannot_connect_to_node_socket

let () =
  register_error_kind
    `Temporary
    ~id:"cannot_connect_to_node_socket"
    ~title:"Cannot connect to node socket"
    ~description:"External validator failed to connect to the node's socket"
    ~pp:(fun fmt () ->
      Format.fprintf
        fmt
        "External validator failed to connect to the node's socket")
    Data_encoding.unit
    (function Cannot_connect_to_node_socket -> Some () | _ -> None)
    (fun () -> Cannot_connect_to_node_socket)

module Events = struct
  open Internal_event.Simple

  let section = ["external_validation"]

  let cannot_connect_and_retry =
    declare_0
      ~section
      ~level:Warning
      ~name:"cannot_connect_and_retry"
      ~msg:"validator cannot connect to node: retrying"
      ()
end

let create_socket_connect ~canceler ~socket_path =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let*! socket = create_socket ~canceler in
  let await_socket_to_be_ready () =
    let log = function
      | [Cannot_connect_to_node_socket] ->
          Internal_event.Simple.emit Events.cannot_connect_and_retry ()
      | _ -> Lwt.return_unit
    in
    Lwt_utils_unix.retry ~log ~n:20 ~sleep:1. @@ fun () ->
    let*! b = Lwt_unix.file_exists socket_path in
    if b then return_unit else tzfail Cannot_connect_to_node_socket
  in
  let* () = await_socket_to_be_ready () in
  let*! () = Lwt_unix.connect socket (make_socket socket_path) in
  return socket
