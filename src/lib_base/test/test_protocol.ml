(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* Testing
   -------
   Component:    Base, Protocol
   Invocation:   dune exec src/lib_base/test/main.exe \
                  -- --file test_protocol.ml
   Subject:      Check the ordering of protocol versions
*)

let all_env_versions = Protocol.[V0; V1; V2; V3]

let () =
  Test.register
    ~__FILE__
    ~title:"environment version comparison: equal"
    ~tags:[Tag.layer1; Tag.base]
  @@ fun () ->
  List.iter
    (fun v ->
      if not (Protocol.compare_version v v = 0) then
        Test.fail "a protocol version should be equal to itself")
    all_env_versions ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"environment version comparison: less-than"
    ~tags:[Tag.layer1; Tag.base]
  @@ fun () ->
  let rec check = function
    | [] -> ()
    | v :: vs ->
        if not (List.for_all (fun w -> Protocol.compare_version v w < 0) vs)
        then Test.fail "error on protocol version comparison" ;
        check vs
  in
  check all_env_versions ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"environment version comparison: greater-than"
    ~tags:[Tag.layer1; Tag.base]
  @@ fun () ->
  let rec check = function
    | [] -> ()
    | v :: vs ->
        if not (List.for_all (fun w -> Protocol.compare_version w v > 0) vs)
        then Test.fail "error on protocol version comparison" ;
        check vs
  in
  check all_env_versions ;
  unit
