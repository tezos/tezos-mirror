(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  too_few_connections : unit Lwt_condition.t;
  too_many_connections : unit Lwt_condition.t;
  new_peer : unit Lwt_condition.t;
  new_point : unit Lwt_condition.t;
  new_connection : unit Lwt_condition.t;
}

let create () =
  {
    too_few_connections = Lwt_condition.create ();
    too_many_connections = Lwt_condition.create ();
    new_peer = Lwt_condition.create ();
    new_point = Lwt_condition.create ();
    new_connection = Lwt_condition.create ();
  }

let wait_too_few_connections t = Lwt_condition.wait t.too_few_connections

let wait_too_many_connections t = Lwt_condition.wait t.too_many_connections

let wait_new_peer t = Lwt_condition.wait t.new_peer

let wait_new_point t = Lwt_condition.wait t.new_point

let wait_new_connection t = Lwt_condition.wait t.new_connection

let broadcast_new_point t = Lwt_condition.broadcast t.new_point ()

let broadcast_new_connection t = Lwt_condition.broadcast t.new_connection ()

let broadcast_new_peer t = Lwt_condition.broadcast t.new_peer ()

let broadcast_too_few_connections t =
  Lwt_condition.broadcast t.too_few_connections ()

let broadcast_too_many_connections t =
  Lwt_condition.broadcast t.too_many_connections ()
