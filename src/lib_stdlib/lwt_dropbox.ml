(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

exception Closed

type 'a t = {
  mutable data : 'a option;
  mutable closed : bool;
  mutable put_waiter : (unit Lwt.t * unit Lwt.u) option;
}

let create () = {data = None; closed = false; put_waiter = None}

let notify_put dropbox =
  match dropbox.put_waiter with
  | None -> ()
  | Some (_waiter, wakener) ->
      dropbox.put_waiter <- None ;
      Lwt.wakeup_later wakener ()

let put dropbox elt =
  if dropbox.closed then raise Closed
  else (
    dropbox.data <- Some elt ;
    notify_put dropbox)

let peek dropbox = if dropbox.closed then raise Closed else dropbox.data

let close dropbox =
  if not dropbox.closed then (
    dropbox.closed <- true ;
    notify_put dropbox)

let wait_put_with_timeout ~timeout dropbox =
  match dropbox.put_waiter with
  | Some (waiter, _wakener) -> Lwt.pick [timeout; Lwt.protected waiter]
  | None ->
      let (waiter, wakener) = Lwt.wait () in
      dropbox.put_waiter <- Some (waiter, wakener) ;
      Lwt.pick [timeout; Lwt.protected waiter]

let wait_put_no_timeout dropbox =
  match dropbox.put_waiter with
  | Some (waiter, _wakener) -> Lwt.protected waiter
  | None ->
      let (waiter, wakener) = Lwt.wait () in
      dropbox.put_waiter <- Some (waiter, wakener) ;
      Lwt.protected waiter

let wait_put ?timeout dropbox =
  match timeout with
  | None -> wait_put_no_timeout dropbox
  | Some timeout -> wait_put_with_timeout ~timeout dropbox

let rec take dropbox =
  match dropbox.data with
  | Some elt ->
      dropbox.data <- None ;
      Lwt.return elt
  | None ->
      if dropbox.closed then Lwt.fail Closed
      else
        let* () = wait_put dropbox in
        take dropbox

let rec take_with_timeout timeout dropbox =
  match dropbox.data with
  | Some elt ->
      Lwt.cancel timeout ;
      dropbox.data <- None ;
      Lwt.return_some elt
  | None ->
      if Lwt.is_sleeping timeout then
        if dropbox.closed then Lwt.fail Closed
        else
          let* () = wait_put ~timeout dropbox in
          take_with_timeout timeout dropbox
      else Lwt.return_none
