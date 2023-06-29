(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {shutdown : unit Lwt.u; server : unit Lwt.t}

module Metrics_server = Prometheus_app.Cohttp (Cohttp_lwt_unix.Server)

let shutdown t =
  Lwt.wakeup t.shutdown () ;
  t.server

let shutdown_on_exit t =
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
      shutdown t)

let launch (addr, port) =
  let open Lwt_syntax in
  let host = Ipaddr.V6.to_string addr in
  let* () = Event.(emit starting_metrics_server) (host, port) in
  let* ctx = Conduit_lwt_unix.init ~src:host () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let mode = `TCP (`Port port) in
  let callback = Metrics_server.callback in
  let stop, shutdown = Lwt.task () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~stop
      ~ctx
      ~mode
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  let t = {shutdown; server} in
  let (_ : Lwt_exit.clean_up_callback_id) = shutdown_on_exit t in
  Lwt.return t
