(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
   Component:    Deposits Limit
   Invocation:   dune exec tezt/tests/main.exe -- --file deposits_limit.ml
   Subject:      Tests for setting and unsetting deposits limits.
*)

let test_set_deposits_limit =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"set deposits limit"
    ~tags:["deposits_limit"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let src = Constant.bootstrap1.alias in
  let* result = Client.set_deposits_limit ~src ~limit:"1000" client in
  Regression.capture result ;
  unit

let test_unset_deposits_limit =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"unset deposits limit"
    ~tags:["deposits_limit"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let src = Constant.bootstrap1.alias in
  let* result = Client.unset_deposits_limit ~src client in
  Regression.capture result ;
  unit

let register ~protocols =
  test_set_deposits_limit protocols ;
  test_unset_deposits_limit protocols
