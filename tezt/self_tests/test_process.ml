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

(* Testing
   -------
   Component: Tezt.Process
   Invocation:
     dune build tezt/self_tests
     dune exec tezt/self_tests/main.exe -- -f test_process.ml
*)

let register () =
  Test.register
    ~__FILE__
    ~title:"Process.terminate with timeout"
    ~tags:["process"; "kill"]
  @@ fun () ->
  let start = Unix.gettimeofday () in
  let process =
    Process.spawn
      ~name:"process"
      "_build/default/tezt/self_tests/bin_catch_sigterm/main.exe"
      []
  in
  let* () = Lwt_unix.sleep 1. in
  Log.info "Will now terminate process." ;
  Process.terminate ~timeout:1. process ;
  let* _ = Process.wait process in
  let time = Unix.gettimeofday () -. start in
  Log.info "Process.wait returned after %g seconds from start." time ;
  if time > 2.5 then
    Test.fail "expected Process.wait to return after about 2 seconds" ;
  unit
