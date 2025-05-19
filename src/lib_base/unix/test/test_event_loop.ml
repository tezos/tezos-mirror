(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Base, Unix
   Invocation:   dune exec src/lib_base/unix/test/main.exe -- --file test_event_loop.ml
   Subject:      Check the serialization or deserialization of unix errors
*)

let tags = Tag.[layer1; base; unix; "event_loop"]

let lwt () = Lwt.pause ()

let eio () =
  Eio.Fiber.yield () ;
  Lwt.return_unit

(* Wrap tests in a fork in order not to pollute other tests with eio. *)
let register ~title promise =
  Test.register ~__FILE__ ~title ~tags @@ fun () ->
  match Lwt_unix.fork () with
  | 0 -> (
      match Tezos_base_unix.Event_loop.main_run ~eio:true promise with
      | () -> exit 0
      | exception _ -> exit 1)
  | pid -> (
      let* _, status = Lwt_unix.waitpid [] pid in
      match status with
      | Unix.WEXITED 0 -> Lwt.return_unit
      | _ -> Lwt.fail_with "failed")

let () = register ~title:"unix event loop: eio promise" eio

let () = register ~title:"unix event loop: lwt promise" lwt

let () =
  register ~title:"unix event loop: lwt & eio promises" @@ fun () ->
  let _ = eio () in
  let* _ = lwt () in
  Lwt.return_unit
