(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (rewards)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                 -- --file test_adaptive_inflation.ml
    Subject:    Test reward values under adaptive inflation
*)

open Protocol
open Alpha_context

let test_reward_coefficient () =
  let csts = Default_parameters.constants_test in
  let default =
    Delegate.Rewards.Internal_for_tests.(
      reward_from_constants csts ~reward_kind:Baking_reward_fixed_portion)
  in
  let default_times_4 =
    Delegate.Rewards.Internal_for_tests.(
      reward_from_constants
        ~coeff:(Q.of_int 4)
        csts
        ~reward_kind:Baking_reward_fixed_portion)
  in
  assert (Tez.(equal (mul_exn default 4) default_times_4)) ;
  return_unit

let tests =
  Tztest.
    [
      tztest
        "adaptive inflation - application of coefficient to rewards"
        `Quick
        test_reward_coefficient;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("adaptive inflation", tests)]
  |> Lwt_main.run
