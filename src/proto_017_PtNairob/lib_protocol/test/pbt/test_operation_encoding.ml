(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec src/proto_017_PtNairob/lib_protocol/test/pbt/main.exe \
                  -- --file test_operation_encoding.ml
    Subject:      Encoding for operations
*)

open Protocol
open QCheck2
open Qcheck2_helpers

(** {2 Generators}  *)
let generate_operation =
  let open Gen in
  let+ _kind, (_hash, op) = Operation_generator.generate_operation in
  op

(** {2 Tests} *)

let test_operation =
  let open Alpha_context in
  let gen = generate_operation in
  let eq {shell = s1; protocol_data = Operation_data d1}
      {shell = s2; protocol_data = Operation_data d2} =
    let o1 : _ Operation.t = {shell = s1; protocol_data = d1} in
    let o2 : _ Operation.t = {shell = s2; protocol_data = d2} in
    match Operation.equal o1 o2 with None -> false | Some Eq -> true
  in
  test_roundtrip
    ~count:2000
    ~title:"Operation.t"
    ~gen
    ~eq
    Alpha_context.Operation.encoding

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": Operation_encoding")
    [(": roundtrip", qcheck_wrap [test_operation])]
