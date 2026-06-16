(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  mutable readers : int;
  mutable writer : bool;
  mutable writers_waiting : int;
  condition : unit Lwt_condition.t;
}

let create () =
  {
    readers = 0;
    writer = false;
    writers_waiting = 0;
    condition = Lwt_condition.create ();
  }

let rec read_lock t =
  let open Lwt.Syntax in
  (* Block if writer holds lock OR writer is waiting (priority) *)
  if t.writer || t.writers_waiting > 0 then
    let* () = Lwt_condition.wait t.condition in
    read_lock t
  else (
    t.readers <- t.readers + 1 ;
    Lwt.return_unit)

let read_unlock t =
  t.readers <- t.readers - 1 ;
  if t.readers = 0 then Lwt_condition.broadcast t.condition ()

let rec write_lock t =
  let open Lwt.Syntax in
  if t.writer || t.readers > 0 then (
    t.writers_waiting <- t.writers_waiting + 1 ;
    let* () =
      Lwt.finalize
        (fun () -> Lwt_condition.wait t.condition)
        (fun () ->
          (* Decrement counter even when canceled to prevent blocking readers *)
          t.writers_waiting <- t.writers_waiting - 1 ;
          Lwt.return_unit)
    in
    write_lock t)
  else (
    t.writer <- true ;
    Lwt.return_unit)

let write_unlock t =
  t.writer <- false ;
  Lwt_condition.broadcast t.condition ()

let with_read_lock t f =
  let open Lwt.Syntax in
  let* () = read_lock t in
  Lwt.finalize f @@ fun () ->
  read_unlock t ;
  Lwt.return_unit

let with_write_lock t f =
  let open Lwt.Syntax in
  let* () = write_lock t in
  Lwt.finalize f @@ fun () ->
  write_unlock t ;
  Lwt.return_unit
