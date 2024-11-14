(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018 Dynamic Ledger Solutions <contact@tezos.com> *)
(* SPDX-FileCopyrightText: 2021-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>.             *)
(*                                                                           *)
(*****************************************************************************)

(** Simple abstraction over Unix lockfiles *)

open Error_monad

(** [lock ~when_locked ~filename] acquires a lock on the file [path] and returns
    the opened file descriptor (for unlocking). If there is already a lock on
    [path], this function call is blocking until the previous lock is
    released. If there is already a lock on [filename], the call will block if
    [when_locked] is [`Block], and will fail if [when_locke = `Fail e]. *)
val lock :
  when_locked:[`Block | `Fail of error] ->
  filename:string ->
  Lwt_unix.file_descr tzresult Lwt.t

(** [unlock fd] releases the lock on the opened file descriptor [fd]. If there
    is no lock or if it is already released, this function does nothing. *)
val unlock : Lwt_unix.file_descr -> unit Lwt.t

(** [with_lock ~when_locked ~filename f] tries to take a lock on file [filename]
    and calls [f] if the lock was successfully taken. If there is already a lock
    on [filename], the call to [f] is blocked until the previous lock is
    released if [when_lock] is [`Block], and will fail with [e] if [when_lock =
    `Fail e] instead. This function may fail with an I/O exception wrapped in
    the error monad if something unexpected happened. *)
val with_lock :
  when_locked:[`Block | `Fail of error] ->
  filename:string ->
  (unit -> 'a tzresult Lwt.t) ->
  'a tzresult Lwt.t
