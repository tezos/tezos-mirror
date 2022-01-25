(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
module Unit_test : sig
  (** 
   * Example: [spec "Alpha_context.ml" Test_alpha_context.test_cases]
   * Unit tests needs tag in log (like "[UNIT] some test description here...")
   * This function handles such meta data *)
  val spec :
    string ->
    unit Alcotest_lwt.test_case list ->
    string * unit Alcotest_lwt.test_case list

  (** Tests with description string without [Unit] are skipped *)
  val skip :
    string ->
    unit Alcotest_lwt.test_case list ->
    string * unit Alcotest_lwt.test_case list
end = struct
  let spec unit_name test_cases = ("[Unit] " ^ unit_name, test_cases)

  let skip unit_name test_cases = ("[SKIPPED] " ^ unit_name, test_cases)
end

let () =
  Alcotest_lwt.run
    "protocol_alpha unit tests"
    [
      Unit_test.spec "Alpha_context.ml" Test_alpha_context.tests;
      Unit_test.spec "Raw_level_repr.ml" Test_raw_level_repr.tests;
      Unit_test.skip "Raw_level_repr.ml" Test_raw_level_repr.skipped_tests;
      Unit_test.spec "Tez_repr.ml" Test_tez_repr.tests;
      Unit_test.spec "Contract_repr.ml" Test_contract_repr.tests;
      Unit_test.spec "Destination_repr.ml" Test_destination_repr.tests;
      Unit_test.spec "Operation_repr.ml" Test_operation_repr.tests;
      Unit_test.spec
        "Global_constants_storage.ml"
        Test_global_constants_storage.tests;
      Unit_test.spec "fitness" Test_fitness.tests;
      Unit_test.spec "fixed point computation" Test_fixed_point.tests;
      Unit_test.spec "level module" Test_level_module.tests;
      Unit_test.spec "qty" Test_qty.tests;
      Unit_test.spec "round" Test_round_repr.tests;
      Unit_test.spec "time" Test_time_repr.tests;
      Unit_test.spec "receipt encodings" Test_receipt.tests;
      Unit_test.spec "saturation arithmetic" Test_saturation.tests;
      Unit_test.spec "gas monad" Test_gas_monad.tests;
    ]
  |> Lwt_main.run
