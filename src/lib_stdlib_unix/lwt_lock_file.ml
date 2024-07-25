(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018 Dynamic Ledger Solutions <contact@tezos.com> *)
(* SPDX-FileCopyrightText: 2021-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>.             *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

let lock ~when_locked ~filename =
  let open Lwt_result_syntax in
  let* fd =
    protect @@ fun () ->
    Lwt_unix.openfile
      filename
      Unix.[O_CLOEXEC; O_TRUNC; O_CREAT; O_WRONLY]
      0o644
    |> Lwt_result.ok
  in
  let* () =
    protect ~on_error:(fun err ->
        let*! () = Lwt_unix.close fd in
        let err =
          match (when_locked, err) with
          | ( `Fail e,
              Exn (Unix.Unix_error ((EAGAIN | EACCES | EDEADLK), _, _)) :: _ )
            ->
              (* TLOCK failed *)
              TzTrace.cons e err
          | _, _ -> err
        in
        fail err)
    @@ fun () ->
    let command =
      match when_locked with `Block -> Unix.F_LOCK | `Fail _ -> Unix.F_TLOCK
    in
    let*! () = Lwt_unix.lockf fd command 0 in
    let pid_str = string_of_int (Unix.getpid ()) in
    let*! (_ : int) =
      Lwt_unix.write_string fd pid_str 0 (String.length pid_str)
    in
    return_unit
  in
  return fd

let unlock fd =
  Lwt.finalize
    (fun () -> Lwt_unix.lockf fd Unix.F_ULOCK 0)
    (fun () -> Lwt_unix.close fd)

let with_lock ~when_locked ~filename f =
  let open Lwt_result_syntax in
  let* fd = lock ~when_locked ~filename in
  Lwt.finalize f (fun () -> unlock fd)
