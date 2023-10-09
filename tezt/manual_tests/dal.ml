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
   Component:    DAL
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file dal.ml --test-arg output-file=<file>
   Subject:      Test getting informaton about the DAL distribution.
*)

module Dal = Dal_common

let dal_distribution =
  Test.register
    ~__FILE__
    ~title:"Get the DAL distribution"
    ~tags:["dal"; "distribution"]
  @@ fun () ->
  let open Dal.Cryptobox in
  let number_of_shards = Cli.get_int "number_of_shards" in
  let slot_size = Cli.get_int "slot_size" in
  let redundancy_factor = Cli.get_int "redundancy_factor" in
  let page_size = Cli.get_int "page_size" in
  let parameters =
    {number_of_shards; redundancy_factor; page_size; slot_size}
  in
  Internal_for_tests.parameters_initialisation parameters
  |> Internal_for_tests.load_parameters ;
  match make parameters with
  | Ok _ ->
      Log.report "Set of parameters is valid" ;
      unit
  | Error (`Fail s) ->
      Test.fail "The set of parameters is invalid. Reason:@.%s@." s

let register () = ()
