(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [test_services] collects Alcotest testable definitions for base OCaml types
   and Tezos specific types. *)

include Alcotest
include Test_services_base

let trace : error trace testable = testable pp_print_error ( = )

let tzresults (type a) (t : a testable) : a tzresult testable = result t trace

let p2p_peer_id : P2p_peer.Id.t Alcotest.testable =
  Alcotest.testable P2p_peer.Id.pp P2p_peer.Id.equal

let json : Data_encoding.json testable = of_pp Data_encoding.Json.pp

(** Transorm a function running in the error monad into an Alcotest, taking
    care of failing the test if the function results is an error.
    Note that the given function must still take care of test assertions. *)
let tztest name speed f =
  Alcotest_lwt.test_case name speed (fun _sw () ->
      f ()
      >>= function
      | Ok () ->
          Lwt.return_unit
      | Error err ->
          Tezos_stdlib_unix.Internal_event_unix.close ()
          >>= fun () ->
          Format.printf "@\n%a@." pp_print_error err ;
          Lwt.fail Alcotest.Test_error)
