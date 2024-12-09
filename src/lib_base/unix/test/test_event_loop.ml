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

let register ~title promise =
  Test.register ~__FILE__ ~title ~tags @@ fun () ->
  Lwt.return @@ Tezos_base_unix.Event_loop.main_run ~eio:true promise

let () = register ~title:"unix event loop: eio promise" eio

let () = register ~title:"unix event loop: lwt promise" lwt

let () =
  register ~title:"unix event loop: lwt & eio promises" @@ fun () ->
  let _ = eio () in
  let* _ = lwt () in
  Lwt.return_unit
