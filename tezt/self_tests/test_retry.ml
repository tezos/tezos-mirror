(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let test_success () =
  Test.register ~__FILE__ ~title:"Success" ~tags:["retry"; "success"]
  @@ fun () ->
  Log.info "Success test." ;
  unit

let test_fail_every_other_run () =
  let should_fail = ref true in
  Test.register
    ~__FILE__
    ~title:"Fail every other run test"
    ~tags:["retry"; "fail"; "flake"]
  @@ fun () ->
  if !should_fail then (
    should_fail := false ;
    Test.fail "Failing test on first try")
  else (
    should_fail := true ;
    Log.info "Works on second" ;
    unit)

let test_fail_always () =
  Test.register
    ~__FILE__
    ~title:"Failing test"
    ~tags:["retry"; "fail"; "always"]
  @@ fun () -> Test.fail "Always failing test"

let register () =
  test_success () ;
  test_fail_every_other_run () ;
  test_fail_always ()
