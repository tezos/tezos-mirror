(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This series of tests is to check the behaviour of the library in the cases
    where the "main" promise resolves "at the same time as" the start of the
    clean-up.

    Because the Lwt-exit library assumes that the two promises cannot be
    resolved simultaneously, this series of tests attempts to break that
    assumption. *)

open Lwt.Infix

let devnull = Lwt_main.run (Lwt_unix.openfile "/dev/null" [O_WRONLY] 0)

(* A signal setup with both soft and hard exits to test both behaviours *)
let signal_setup =
  Lwt_exit.make_signal_setup ~soft:[Sys.sigint; Sys.sigterm] ~hard:[Sys.sigusr1]

let child_inted p =
  Lwt_unix.dup2 devnull Lwt_unix.stderr ;
  let r = ref 0 in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        incr r ; Lwt.return ())
  in
  match Lwt_main.run @@ Lwt_exit.wrap_and_error p with
  | Error 127 ->
      assert (!r = 1) ;
      Stdlib.exit 0
  | Error _ ->
      assert false
  | Ok _ ->
      assert false

let child_ok p =
  Lwt_unix.dup2 devnull Lwt_unix.stderr ;
  let r = ref 0 in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        incr r ; Lwt.return ())
  in
  match Lwt_main.run @@ Lwt_exit.wrap_and_error p with
  | Ok 1024 ->
      assert (!r = 0) ;
      Stdlib.exit 0
  | Ok _ ->
      assert false
  | Error _ ->
      assert false

let child_already_cleaned p =
  Lwt_unix.dup2 devnull Lwt_unix.stderr ;
  let r = ref 0 in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        incr r ; Lwt.return ())
  in
  match Lwt_main.run @@ Lwt_exit.wrap_and_error p with
  | Error 2048 ->
      Stdlib.exit 0
  | Error _ ->
      assert false
  | Ok _ ->
      assert false

let main pid =
  let s : unit Lwt.t =
    Lwt_unix.sleep 0.01 >>= fun () -> Unix.kill pid Sys.sigint ; Lwt.return ()
  in
  Lwt_unix.waitpid [] pid
  >|= fun (_, status) ->
  Lwt.cancel s ;
  match status with
  | WEXITED 0 ->
      ()
  | WEXITED _ ->
      assert false
  | WSIGNALED _ ->
      assert false
  | WSTOPPED _ ->
      assert false

let test_simple () =
  match Lwt_unix.fork () with
  | 0 ->
      child_inted Lwt_exit.clean_up_starts
  | pid ->
      Lwt_main.run @@ main pid

let test_map () =
  match Lwt_unix.fork () with
  | 0 ->
      child_inted (Lwt_exit.clean_up_starts >|= function 127 -> 0 | _ -> 1)
  | pid ->
      Lwt_main.run @@ main pid

let test_bind () =
  match Lwt_unix.fork () with
  | 0 ->
      child_inted
        ( Lwt_exit.clean_up_starts
        >>= function 127 -> Lwt.return 0 | _ -> Lwt.return 1 )
  | pid ->
      Lwt_main.run @@ main pid

let test_pick () =
  match Lwt_unix.fork () with
  | 0 ->
      let dummy_state = ref 1024 in
      let dummy_ev_wait () =
        Lwt_unix.sleep 0.001 >|= fun () -> !dummy_state + 7
      in
      let dummy_ev_handling ev = dummy_state := !dummy_state + (ev / 9) in
      let rec dummy_loop () =
        Lwt.pick
          [ (Lwt_exit.clean_up_starts >|= fun _ -> 0);
            ( dummy_ev_wait ()
            >>= fun ev -> dummy_ev_handling ev ; dummy_loop () ) ]
      in
      child_inted (dummy_loop ())
  | pid ->
      Lwt_main.run @@ main pid

let test_resolve_wait_by_clean_up () =
  match Lwt_unix.fork () with
  | 0 ->
      let (p, r) = Lwt.wait () in
      let _ =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            Lwt.wakeup r 2000 ; Lwt.return_unit)
      in
      child_inted p
  | pid ->
      Lwt_main.run @@ main pid

let test_resolve_task_by_clean_up () =
  match Lwt_unix.fork () with
  | 0 ->
      let (p, r) = Lwt.task () in
      let _ =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            Lwt.wakeup r 2000 ; Lwt.return_unit)
      in
      child_inted p
  | pid ->
      Lwt_main.run @@ main pid

let test_already_resolved () =
  match Lwt_unix.fork () with
  | 0 ->
      child_ok (Lwt.return 1024)
  | pid ->
      Lwt_main.run @@ main pid

let () =
  test_simple () ;
  test_map () ;
  test_bind () ;
  test_pick () ;
  test_resolve_wait_by_clean_up () ;
  test_resolve_task_by_clean_up () ;
  test_already_resolved () ;
  Stdlib.exit 0
