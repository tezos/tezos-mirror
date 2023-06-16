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
    Component:    Adaptive Inflation, launch vote
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_adaptive_inflation_launch.ml
    Subject:      Test the launch vote feature of Adaptive Inflation.
*)

let assert_level ~loc (blk : Block.t) expected =
  let current_level = blk.header.shell.level in
  Assert.equal_int32 ~loc current_level expected

(* Test that the EMA of the adaptive inflation vote reaches the
   threshold after the expected duration. *)
let test_ema_reaches_threshold threshold expected_vote_duration () =
  let open Lwt_result_syntax in
  let assert_ema_above_threshold ~loc
      (metadata : Protocol.Main.block_header_metadata) =
    let ema =
      Protocol.Alpha_context.Toggle_votes.Adaptive_inflation_launch_EMA.to_int32
        metadata.adaptive_inflation_toggle_ema
    in
    Assert.lt_int32 ~loc threshold ema
  in
  let* block, _contract =
    let default_constants = Default_parameters.constants_test in
    let adaptive_inflation =
      {
        default_constants.adaptive_inflation with
        launch_ema_threshold = threshold;
      }
    in
    let consensus_threshold = 0 in
    Context.init_with_constants1
      {default_constants with consensus_threshold; adaptive_inflation}
  in
  let* block =
    Block.bake_while_with_metadata
      ~adaptive_inflation_vote:Toggle_vote_on
      (fun _block metadata ->
        let ema =
          Protocol.Alpha_context.Toggle_votes.Adaptive_inflation_launch_EMA
          .to_int32
            metadata.adaptive_inflation_toggle_ema
        in
        Compare.Int32.(ema < threshold))
      block
  in
  let* () =
    assert_level ~loc:__LOC__ block (Int32.pred expected_vote_duration)
  in
  let* block, metadata =
    Block.bake_n_with_metadata ~adaptive_inflation_vote:Toggle_vote_on 1 block
  in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  let* () = assert_level ~loc:__LOC__ block expected_vote_duration in
  return_unit

let tests =
  [
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level"
      `Slow
      (test_ema_reaches_threshold
         Default_parameters.constants_test.adaptive_inflation
           .launch_ema_threshold
         187259l
         (* This vote duration is consistent with the result of the
            unit test for this EMA in
            ../unit/test_adaptive_inflation_ema.ml*));
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("adaptive inflation launch", tests)]
  |> Lwt_main.run
