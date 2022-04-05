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

let try_with_lock ~when_locked ~filename f =
  protect (fun () ->
      let open Lwt_result_syntax in
      let flags =
        let open Unix in
        [O_CLOEXEC; O_TRUNC; O_CREAT; O_WRONLY]
      in
      let* fd = Lwt_result.ok @@ Lwt_unix.openfile filename flags 0o644 in
      Lwt.finalize
        (fun () ->
          let* lock_status =
            Lwt.catch
              (fun () ->
                let* () = Lwt_result.ok @@ Lwt_unix.lockf fd Unix.F_TLOCK 0 in
                return `Free)
              (function
                | Unix.Unix_error ((EAGAIN | EACCES | EDEADLK), _, _) ->
                    return `Locked
                | exn -> tzfail (Exn exn))
          in
          match lock_status with
          | `Locked -> when_locked ()
          | `Free ->
              let pid_str = string_of_int (Unix.getpid ()) in
              let* (_ : int) =
                Lwt_result.ok
                @@ Lwt_unix.write_string fd pid_str 0 (String.length pid_str)
              in
              Lwt.finalize
                (fun () -> f ())
                (fun () -> Lwt_unix.lockf fd Unix.F_ULOCK 0))
        (fun () -> Lwt_unix.close fd))
