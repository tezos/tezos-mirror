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
      (* The child must terminate with [Unix._exit], not [Stdlib.exit].
         Under Tezt's multi-process scheduler this child is itself a fork of a
         Tezt worker and never calls [exec], so it inherits the worker's
         [at_exit] handlers and scheduler pipe fds. [Stdlib.exit] would run
         those handlers -- notably Lwt's, which (because [lwt_io] registers a
         [flush_all] hook via [Lwt_main.at_exit]) re-enters [Lwt_main.run]. That
         re-entry iterates the inherited, forked libev engine, which is in an
         undefined post-fork state and can hang -- leaving the parent blocked in
         [waitpid] and the scheduler pipe open until the test timeout. (This is
         not about Eio: [Lwt_eio.with_event_loop] restores the previous engine
         on exit, so the Eio loop is already gone by this point.) [Unix._exit]
         skips all of it and just reports the result via the exit code. *)
      match
        Tezos_base_unix.Event_loop.main_run
          ~process_name:title
          ~eio:true
          promise
      with
      | () -> Unix._exit 0
      | exception _ -> Unix._exit 1)
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
