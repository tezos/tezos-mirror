(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Error_monad

let create ?(close_on_exec = true) ?(unlink_on_exit = false) fn =
  protect (fun () ->
      let flags =
        let open Unix in
        let flags = [O_TRUNC; O_CREAT; O_WRONLY] in
        if close_on_exec then O_CLOEXEC :: flags else flags
      in
      Lwt_unix.openfile fn flags 0o644 >>= fun fd ->
      Lwt_unix.lockf fd Unix.F_TLOCK 0 >>= fun () ->
      if unlink_on_exit then Lwt_main.at_exit (fun () -> Lwt_unix.unlink fn) ;
      let pid_str = string_of_int @@ Unix.getpid () in
      Lwt_unix.write_string fd pid_str 0 (String.length pid_str) >>= fun _ ->
      return_unit)

let is_locked fn =
  if not @@ Sys.file_exists fn then return_false
  else
    protect (fun () ->
        Lwt_unix.openfile fn Unix.[O_RDONLY; O_CLOEXEC] 0o644 >>= fun fd ->
        Lwt.finalize
          (fun () ->
            Lwt.try_bind
              (fun () -> Lwt_unix.(lockf fd F_TEST 0))
              (fun () -> return_false)
              (fun _ -> return_true))
          (fun () -> Lwt_unix.close fd))
