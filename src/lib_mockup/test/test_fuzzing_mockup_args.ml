(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Mockup args library
    Invocation:   dune exec src/lib_mockup/test/main.exe
    Subject:      Fuzzing tests of the Mockup args library
*)

open Tezos_mockup_registration.Mockup_args
open Qcheck2_helpers

let chain_id_gen = QCheck2.Gen.(map Chain_id.of_string string)

(** {!val:Chain_id.choose} always prioritizes the config file over the default value *)
let test_config_file_has_priority_over_default_value from_config_file_val =
  let expected = from_config_file_val in
  let actual = Chain_id.choose ~from_config_file:(Some from_config_file_val) in
  let pp = Tezos_crypto.Hashed.Chain_id.pp in
  qcheck_eq' ~pp ~expected ~actual ()

let test_prioritize_config_file =
  QCheck2.Test.make
    ~name:
      "Chain_id.choose always prioritizes the config file over the default \
       value"
    chain_id_gen
    test_config_file_has_priority_over_default_value

let () =
  Alcotest.run
    ~__FILE__
    "Fuzzing_mockup_args"
    [("Chain_id.choose", qcheck_wrap [test_prioritize_config_file])]
