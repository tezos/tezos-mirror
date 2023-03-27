(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Components: Michelson
   Invocation: dune exec tezt/tests/main.exe -- --file big_map_arity.ml
   Subject: Tests the error message in case the EMPTY_BIG_MAP instruction has bad arity.
*)

let expected_msg =
  rex "primitive EMPTY_BIG_MAP expects 2 arguments but is given 1."

let test_big_map_arity =
  Protocol.register_test
    ~__FILE__
    ~title:"Test EMPTY_BIG_MAP arity error"
    ~tags:["client"; "michelson"; "typechecking"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script =
    Michelson_script.(find ["ill_typed"; "big_map_arity"] protocol |> path)
  in
  let process = Client.spawn_typecheck_script ~scripts:[script] client in
  Process.check_error ~exit_code:1 ~msg:expected_msg process

let register ~protocols = test_big_map_arity protocols
