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

let r = ref 0

let first =
  Lwt_exit.register_clean_up_callback (fun _ ->
      Lwt_unix.sleep 0.2
      >>= fun () ->
      assert (!r = 0) ;
      incr r ;
      Lwt.return_unit)

let second =
  Lwt_exit.register_clean_up_callback ~after:first (fun _ ->
      Lwt_unix.sleep 0.1
      >>= fun () ->
      assert (!r = 1) ;
      incr r ;
      Lwt.return_unit)

let third =
  Lwt_exit.register_clean_up_callback ~after:second (fun _ ->
      assert (!r = 2) ;
      incr r ;
      Lwt.return_unit)

let () =
  match
    Lwt_main.run
    @@ Lwt_exit.wrap_and_error
         (let (_ : unit Lwt.t) =
            Lwt_unix.sleep 0.1 >>= fun () -> Lwt_exit.exit_and_raise 3
          in
          fst @@ Lwt.task ())
  with
  | Ok _ ->
      assert false
  | Error _ ->
      assert (!r = 3) ;
      exit 0
