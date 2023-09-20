(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

open Lwt.Syntax

exception None_successful of string

let never_ending () = fst (Lwt.wait ())

(* A worker launcher, takes a cancel callback to call upon *)
let worker name ~on_event ~run ~cancel =
  let stop, stopper = Lwt.wait () in
  let fail e =
    Lwt.finalize
      (fun () ->
        on_event
          name
          (`Failed (Printf.sprintf "Exception: %s" (Printexc.to_string e))))
      cancel
  in
  let* () = on_event name `Started in
  let p = Lwt.catch run fail in
  Lwt.on_termination p (Lwt.wakeup stopper) ;
  let* () = stop in
  Lwt.catch (fun () -> on_event name `Ended) (fun _ -> Lwt.return_unit)

let worker name ~on_event ~run ~cancel =
  Lwt.no_cancel (worker name ~on_event ~run ~cancel)

let pick_successful = function
  | [] ->
      raise
        (Invalid_argument
           "Lwt_utils.pick_successful [] would return a promise that is \
            pending forever")
  | promises ->
      let promise, resolver = Lwt.task () in
      let pending_count = ref (List.length promises) in
      let on_success value =
        try Lwt.wakeup_later resolver value
        with Stdlib.Invalid_argument _ ->
          (* If the promise is already resolved, the raised
               exception [Stdlib.Invalid_argument _] is caught
               and ignored. *)
          ()
      in
      let on_failure _exn =
        decr pending_count ;
        if !pending_count = 0 then
          Lwt.wakeup_later_exn
            resolver
            (None_successful
               "All pending tasks were rejected, canceled or did not pass \n\
                the success criteria.")
        else ()
      in
      List.iter
        (fun promise ->
          Lwt.on_success promise on_success ;
          Lwt.on_failure promise on_failure)
        promises ;
      let cancel_all promises = List.iter Lwt.cancel promises in
      Lwt.on_success promise (fun _ -> cancel_all promises) ;
      Lwt.on_cancel promise (fun () -> cancel_all promises) ;
      promise
