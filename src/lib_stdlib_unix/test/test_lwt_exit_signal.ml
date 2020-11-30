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

open Lwt.Infix

let devnull = Lwt_main.run (Lwt_unix.openfile "/dev/null" [O_WRONLY] 0)

(* A signal setup with both soft and hard exits to test both behaviours *)
let signal_setup =
  Lwt_exit.make_signal_setup ~soft:[Sys.sigint; Sys.sigterm] ~hard:[Sys.sigusr1]

let default_double_signal_safety = Option.get @@ Ptime.Span.of_float_s 0.1

let short_max_clean_up_time = Option.get @@ Ptime.Span.of_float_s 0.04

let child_main ?double_signal_safety ?max_clean_up_time () =
  let double_signal_safety =
    Option.value double_signal_safety ~default:default_double_signal_safety
  in
  Lwt_unix.dup2 devnull Lwt_unix.stderr ;
  let r = ref 10 in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Lwt_unix.sleep 0.02
        >>= fun () ->
        r := 11 ;
        Lwt_unix.sleep 0.05
        >>= fun () ->
        r := 12 ;
        Lwt_unix.sleep 0.2 >>= fun () -> Lwt.return ())
  in
  Stdlib.exit @@ Lwt_main.run
  @@ ( Lwt_exit.wrap_and_error
         ?max_clean_up_time
         ~double_signal_safety
         ~signal_setup
         (Tezos_stdlib.Lwt_utils.never_ending ())
     >>= function
     | Ok () ->
         Lwt.return 3
     | Error code ->
         if code = 127 then Lwt.return !r
         else if code = 127 lor 128 then Lwt.return (!r lor 128)
         else Lwt.return code )

let main () =
  (* test INT *)
  match Lwt_unix.fork () with
  | 0 ->
      child_main ()
  | pid -> (
      Lwt_main.run
        (let s : unit Lwt.t =
           Lwt_unix.sleep 0.01
           >>= fun () -> Unix.kill pid Sys.sigint ; Lwt.return ()
         in
         Lwt_unix.waitpid [] pid
         >|= fun (_, status) ->
         Lwt.cancel s ;
         match status with
         | WEXITED 12 ->
             ()
         | WEXITED _ ->
             assert false
         | WSIGNALED _ ->
             assert false
         | WSTOPPED _ ->
             assert false) ;
      (* test INT short clean-up time *)
      match Lwt_unix.fork () with
      | 0 ->
          child_main ~max_clean_up_time:short_max_clean_up_time ()
      | pid -> (
          Lwt_main.run
            (let s : unit Lwt.t =
               Lwt_unix.sleep 0.01
               >>= fun () -> Unix.kill pid Sys.sigint ; Lwt.return ()
             in
             Lwt_unix.waitpid [] pid
             >|= fun (_, status) ->
             Lwt.cancel s ;
             match status with
             | WEXITED s when s = 128 lor 11 ->
                 ()
             | WEXITED _ ->
                 assert false
             | WSIGNALED _ ->
                 assert false
             | WSTOPPED _ ->
                 assert false) ;
          (* test INT-short-sleep-INT *)
          match Lwt_unix.fork () with
          | 0 ->
              child_main ()
          | pid -> (
              Lwt_main.run
                (let s : unit Lwt.t =
                   Lwt_unix.sleep 0.01
                   >>= fun () ->
                   Unix.kill pid Sys.sigint ;
                   Lwt_unix.sleep 0.02
                   >>= fun () -> Unix.kill pid Sys.sigint ; Lwt.return ()
                 in
                 Lwt_unix.waitpid [] pid
                 >|= fun (_, status) ->
                 Lwt.cancel s ;
                 match status with
                 | WEXITED 12 ->
                     ()
                 | WEXITED _ ->
                     assert false
                 | WSIGNALED _ ->
                     assert false
                 | WSTOPPED _ ->
                     assert false) ;
              (* test INT-long-sleep-INT *)
              match Lwt_unix.fork () with
              | 0 ->
                  child_main ()
              | pid -> (
                  Lwt_main.run
                    (let s : unit Lwt.t =
                       Lwt_unix.sleep 0.01
                       >>= fun () ->
                       Unix.kill pid Sys.sigint ;
                       Lwt_unix.sleep 0.11
                       >>= fun () -> Unix.kill pid Sys.sigint ; Lwt.return ()
                     in
                     Lwt_unix.waitpid [] pid
                     >|= fun (_, status) ->
                     Lwt.cancel s ;
                     match status with
                     | WEXITED s when s = 127 lor 128 ->
                         ()
                     | WEXITED _ ->
                         assert false
                     | WSIGNALED _ ->
                         assert false
                     | WSTOPPED _ ->
                         assert false) ;
                  (* test no double-signal safety *)
                  match Lwt_unix.fork () with
                  | 0 ->
                      child_main ~double_signal_safety:Ptime.Span.zero ()
                  | pid -> (
                      Lwt_main.run
                        (let s : unit Lwt.t =
                           Lwt_unix.sleep 0.01
                           >>= fun () ->
                           Unix.kill pid Sys.sigint ;
                           Lwt_unix.sleep 0.02
                           >>= fun () ->
                           Unix.kill pid Sys.sigint ; Lwt.return_unit
                         in
                         Lwt_unix.waitpid [] pid
                         >|= fun (_, status) ->
                         Lwt.cancel s ;
                         match status with
                         | WEXITED s when s = 127 lor 128 ->
                             ()
                         | WEXITED _ ->
                             assert false
                         | WSIGNALED _ ->
                             assert false
                         | WSTOPPED _ ->
                             assert false) ;
                      (* test USR1 (hard) *)
                      match Lwt_unix.fork () with
                      | 0 ->
                          child_main ()
                      | pid -> (
                          Lwt_main.run
                            (let s : unit Lwt.t =
                               Lwt_unix.sleep 0.01
                               >>= fun () ->
                               Unix.kill pid Sys.sigusr1 ; Lwt.return ()
                             in
                             Lwt_unix.waitpid [] pid
                             >|= fun (_, status) ->
                             Lwt.cancel s ;
                             match status with
                             | WEXITED s when s = 127 lor 128 ->
                                 ()
                             | WEXITED _ ->
                                 assert false
                             | WSIGNALED _ ->
                                 assert false
                             | WSTOPPED _ ->
                                 assert false) ;
                          (* test KILL *)
                          match Lwt_unix.fork () with
                          | 0 ->
                              child_main ()
                          | pid ->
                              Lwt_main.run
                                (let s : unit Lwt.t =
                                   Lwt_unix.sleep 0.01
                                   >>= fun () ->
                                   Unix.kill pid Sys.sigkill ; Lwt.return ()
                                 in
                                 Lwt_unix.waitpid [] pid
                                 >|= fun (_, status) ->
                                 Lwt.cancel s ;
                                 match status with
                                 | WEXITED _ ->
                                     assert false
                                 | WSIGNALED _ ->
                                     ()
                                 | WSTOPPED _ ->
                                     assert false) ;
                              () ) ) ) ) ) )

let () = main () ; Stdlib.exit 0
