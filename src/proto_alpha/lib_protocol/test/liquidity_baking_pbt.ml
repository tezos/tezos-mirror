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
    Component:    pbt for liquidity baking
    Invocation:   [QCHECK_SEED=<seed>] dune exec src/proto_alpha/lib_protocol/test/liquidity_baking_pbt.exe
    Subject:      Test liquidity baking contracts using randomly generated inputs.
*)

open Liquidity_baking_machine

(** We use the “machines” provided by the {! Liquidity_baking_machine}
    module.  Because using the [ConcreteMachine] (hence, the {!
    ValidationMachine} too) is slow, we implement the following
    test-suit architecture:

    - One {v QCheck v}-based test is used to validate consistency of
      the {! SymbolicMachine} wrt. the [ConcreteMachine], thanks to
      the {! ValidationMachine}.
    - The rest of the tests use the {! SymbolicMachine} in order to be
      more effective. *)

let extract_qcheck_tzresult : unit tzresult Lwt.t -> bool =
 fun p ->
  match Lwt_main.run p with
  | Ok () -> true
  | Error err -> QCheck.Test.fail_reportf "@\n%a@." pp_print_error err

let rec run_and_check check scenarios env state =
  match scenarios with
  | step :: rst ->
      let state' = SymbolicMachine.step step env state in
      assert (check state state') ;
      run_and_check check rst env state'
  | [] -> state

let one_balance_decreases c env state state' =
  let xtz = SymbolicMachine.get_xtz_balance c state in
  let tzbtc = SymbolicMachine.get_tzbtc_balance c env state in
  let lqt = SymbolicMachine.get_liquidity_balance c env state in
  let xtz' = SymbolicMachine.get_xtz_balance c state' in
  let tzbtc' = SymbolicMachine.get_tzbtc_balance c env state' in
  let lqt' = SymbolicMachine.get_liquidity_balance c env state' in
  xtz' < xtz || tzbtc' < tzbtc || lqt' < lqt
  || (xtz' = xtz && tzbtc' = tzbtc && lqt' = lqt)

let tests =
  [
    QCheck.Test.make
      ~count:5
      ~name:"arbitrary scenarios"
      (Liquidity_baking_generator.arb_scenario 1_000_000 1_000_000 10)
      (fun (specs, scenario) ->
        extract_qcheck_tzresult
          ( ValidationMachine.build specs >>=? fun (state, env) ->
            ValidationMachine.run scenario env state >>=? fun _ -> return_unit
          ));
    QCheck.Test.make
      ~count:100
      ~name:"arbitrary adversary scenarios"
      (Liquidity_baking_generator.arb_adversary_scenario
         1_000_000
         1_000_000
         100)
      (fun (specs, attacker, scenario) ->
        let (state, env) = SymbolicMachine.build ~subsidy:0L specs in
        let _ =
          run_and_check (one_balance_decreases attacker env) scenario env state
        in
        true);
  ]

let _ =
  let open Lib_test.Qcheck_helpers in
  Alcotest.run
    "Liquidity baking PBT"
    [("Arbitrary scenarios", qcheck_wrap tests)]
