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
    Subject:      Unit tests of the Mockup args library
*)

open Tezos_mockup_registration.Mockup_args

let testable_chain_id =
  Alcotest.testable
    Tezos_crypto.Hashed.Chain_id.pp
    Tezos_crypto.Hashed.Chain_id.( = )

(** {!val:Chain_id.choose} uses the dummy value if no config file
    is specified *)
let test_no_config_file_dummy () =
  let expected = Chain_id.dummy in
  let actual = Chain_id.choose ~from_config_file:None in
  Alcotest.check testable_chain_id "default value is dummy" expected actual

let tests =
  [
    ( "Chain_id.choose uses the dummy value if no config file is specified",
      `Quick,
      test_no_config_file_dummy );
  ]

let () = Alcotest.run ~__FILE__ "tezos-mockup" [("mockup_args", tests)]
