(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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
    Component:    Lib_scoru_wasm durable
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Durable snapshot$"
    Subject:      Tests for the tezos-scoru-wasm durable snapshotting
*)

open Tztest
open QCheck2
open Encodings_util
open Durable_snapshot_util
open Probability_utils

let tztest_qcheck2 ?(number_of_runs = 1) ~name (generator, verifier) =
  let verifier inp = Lwt.map (fun x -> Ok x) (verifier inp) in
  (* We consider it's weird if something failed during test generation,
     hence max_gen = 1
  *)
  let max_gen = 1 in
  (* Once again, if anything failed during generation, it's bad,
     hence max_fail = 1
  *)
  let max_fail = 1 in
  (* No retries needed while shrinking *)
  let retries = 1 in
  (* All tests have to be generated properly, fail otherwis*)
  let if_assumptions_fail = (`Fatal, 1.0) in
  let name, speed, run =
    QCheck_alcotest.to_alcotest
      ( QCheck2.Test.make
          ~if_assumptions_fail
          ~max_gen
          ~max_fail
          ~retries
          ~count:number_of_runs
          ~name
          (* Let's disable shrinking for now.
             Reason for that that if tests fails,
             shriking is perpetual.
             I will take care of this later:
             * either writing own shrinking for testcase type,
             * or by reducing number of constants,
                 which are not affected by shrinking
          *)
          (Gen.no_shrink generator)
      @@ fun x ->
        match Lwt_main.run (verifier x) with
        | Ok _ -> true
        | Error _err -> false )
  in
  Alcotest_lwt.test_case name speed (fun _sw () -> Lwt.return @@ run ())

module Runners = struct
  (* rounds - number of operations to be generated in a stress-test *)
  let gen_stress ~(rounds : int) ~(initial_tree_size : int)
      ~operations_distribution =
    ( Durable_program_generator.gen_testcase
        ~initial_size:initial_tree_size
        ~operations_number:rounds
        operations_distribution,
      Durable_program_runner.Verifiable_program_runner.run_testcase )

  let run_scenario (scenario : Verifiable_current_durable.t -> 'a Lwt.t) :
      'a Lwt.t =
    let open Lwt_syntax in
    let* tree = empty_tree () in
    let* durable =
      Tree_encoding_runner.decode Verifiable_current_durable.encoding tree
    in
    let* result = scenario durable in
    let* final_state_eq =
      compare_durable_storages (fst durable) (snd durable)
    in
    match final_state_eq with
    | Error (snapshot_str, current_str) ->
        Assert.fail
          ~loc:__LOC__
          ~msg:"Final durable states diverged"
          Fmt.string
          snapshot_str
          current_str
    | _ -> Lwt.return result
end

let assert_exception exected_ex run =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let+ _ = run () in
      assert false)
    (fun caught_exn ->
      match caught_exn with
      | e when e = exected_ex -> Lwt.return_unit
      | x -> raise x)

(* Actual tests *)
let test_several_operations () =
  Runners.run_scenario @@ fun durable ->
  let open Lwt_syntax in
  let key1 =
    Verifiable_current_durable.key_of_string_exn "/durable/value/to/write1"
  in
  let key2 =
    Verifiable_current_durable.key_of_string_exn "/durable/value/to2/write2"
  in
  let* durable =
    Verifiable_current_durable.write_value_exn durable key1 0L "hello"
  in
  let* durable =
    Verifiable_current_durable.write_value_exn durable key2 0L "world"
  in
  let* res_hello =
    Verifiable_current_durable.read_value_exn durable key1 0L 5L
  in
  Assert.String.equal ~loc:__LOC__ "hello" res_hello ;
  let* res_still_hello =
    Verifiable_current_durable.read_value_exn durable key1 0L 10L
  in
  Assert.String.equal ~loc:__LOC__ "hello" res_still_hello ;
  let key_prefix =
    Verifiable_current_durable.key_of_string_exn "/durable/value"
  in
  let* durable = Verifiable_current_durable.delete durable key_prefix in
  let* () =
    assert_exception Tezos_scoru_wasm_durable_snapshot.Durable.Value_not_found
    @@ fun () -> Verifiable_current_durable.read_value_exn durable key1 0L 5L
  in
  return_ok_unit

let stress_test_desceding ~init_size ~rounds =
  tztest_qcheck2
    ~name:
      (Format.asprintf
         "Descending distributed all operations. Initial size: %d, %d \
          operations"
         init_size
         rounds)
  @@ Runners.gen_stress
       ~initial_tree_size:init_size
       ~rounds
       ~operations_distribution:
         (Distributions.descending_distribution_l
            Durable_operation_generator.all_operations)

let stress_test_uniform ~init_size ~rounds =
  tztest_qcheck2
    ~name:
      (Format.asprintf
         "Uniformly distributed all operations. Initial size: %d, %d operations"
         init_size
         rounds)
  @@ Runners.gen_stress
       ~initial_tree_size:init_size
       ~rounds
       ~operations_distribution:
         (Distributions.uniform_distribution_l
            Durable_operation_generator.all_operations)

let stress_strcture_ops ~init_size ~rounds =
  tztest_qcheck2
    ~name:
      (Format.asprintf
         "Uniformly distributed structural operations. Initial size: %d, %d \
          operations"
         init_size
         rounds)
  @@ Runners.gen_stress
       ~initial_tree_size:init_size
       ~rounds
       ~operations_distribution:
         (Distributions.uniform_distribution_l
         @@ List.concat
              [
                Durable_operation_generator.structure_modification_operations;
                Durable_operation_generator.structure_inspection_operations;
              ])

let stress_each_op () =
  let initial_tree_size = 1000 in
  let rounds = 2000 in
  let test_case ~name distr =
    tztest_qcheck2 ~name
    @@ Runners.gen_stress
         ~rounds
         ~initial_tree_size
         ~operations_distribution:distr
  in
  List.mapi
    (fun i op_tag ->
      let name =
        Format.asprintf
          "Stress-test operation %a. Initial size: %d, %d operations"
          Durable_operation.pp_operation_tag
          op_tag
          initial_tree_size
          rounds
      in
      test_case ~name
      @@ Distributions.one_of_n i Durable_operation_generator.all_operations)
    Durable_operation.all_operation_tags

let tests : unit Alcotest_lwt.test_case trace =
  List.append
    [
      tztest "Do several operations on durable" `Quick test_several_operations;
      stress_test_desceding ~init_size:2000 ~rounds:3000;
      stress_test_uniform ~init_size:2000 ~rounds:20000;
      stress_strcture_ops ~init_size:2000 ~rounds:3000;
    ]
    (stress_each_op ())
