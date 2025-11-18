(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (committee selection)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/unit/main.exe \
                  -- --file test_fitness.ml
    Subject:    test the fitness module
*)

open Protocol

let level_zero = Raw_level_repr.of_int32_exn 0l

let round_of_int32_exn i =
  match Round_repr.of_int32 i with
  | Ok i -> i
  | Error _ -> Stdlib.failwith "Invalid round representation"

let make_tuple (level, r_opt, r0, r1) =
  let r_opt = Option.map round_of_int32_exn r_opt in
  let r0 = round_of_int32_exn r0 in
  let r1 = round_of_int32_exn r1 in
  (level, r_opt, r0, r1)

let test_cases =
  List.map
    make_tuple
    [
      (3l, Some 1l, 1l, 12l);
      (10l, Some 1l, 1l, 12l);
      (10l, Some 4l, 2l, 6l);
      (10l, Some 4l, 1l, 12l);
      (9l, Some 2l, 0l, 3l);
      (7l, None, 0l, 3l);
      (7l, None, 1l, 3l);
      (0l, None, 0l, 0l);
      (12l, Some 2l, 8l, 7l);
      (10l, Some 0l, 1l, 1l);
      (8l, None, 1l, 0l);
      (12l, Some 1l, 8l, 7l);
      (8l, None, 6l, 0l);
    ]

let rec product l1 l2 =
  match l1 with
  | [] -> []
  | h :: tl -> List.map (fun x -> (h, x)) l2 @ product tl l2

let test_product_cases = product test_cases test_cases

let tuple_to_fitness (level, locked_round, predecessor_round, round) =
  Fitness_repr.create
    ~level:(Raw_level_repr.of_int32_exn level)
    ~locked_round
    ~predecessor_round
    ~round

let tuple_to_fitness_exn tuple =
  tuple_to_fitness tuple |> function
  | Ok f -> f
  | Error err ->
      Format.kasprintf
        Stdlib.failwith
        "cannot create fitness from tuple: %a"
        pp_print_trace
        (Environment.wrap_tztrace err)

let test_from_to_raw_fitness tuple =
  let fitness = tuple_to_fitness_exn tuple in
  Fitness_repr.from_raw (Fitness_repr.to_raw fitness) |> function
  | Ok new_fitness -> assert (fitness = new_fitness)
  | Error _x -> assert false

let test_from_to_raw_fitness_all () =
  let open Lwt_result_syntax in
  List.iter test_from_to_raw_fitness test_cases ;
  return_unit

let test_locked_round () =
  let open Lwt_result_wrap_syntax in
  let test_bad_cases =
    List.map
      make_tuple
      [
        (8l, Some 7l, 1l, 1l);
        (9l, Some 8l, 0l, 3l);
        (10l, Some 7l, 2l, 6l);
        (11l, Some 5l, 5l, 1l);
        (8l, Some 2l, 1l, 1l);
        (9l, Some 3l, 0l, 3l);
        (11l, Some 5l, 5l, 1l);
        (13l, Some 2l, 1l, 1l);
        (10l, Some 4l, 1l, 1l);
        (8l, Some 7l, 1l, 1l);
        (10l, Some 8l, 2l, 6l);
        (11l, Some 9l, 5l, 1l);
        (12l, Some 10l, 8l, 7l);
        (13l, Some 14l, 1l, 1l);
      ]
  in
  List.iter_es
    (fun tuple ->
      let@ fitness = tuple_to_fitness tuple in
      match fitness with
      | Error
          [
            Environment.Ecoproto_error
              (Fitness_repr.Locked_round_not_less_than_round _);
          ] ->
          return_unit
      | Error err -> failwith "unexpected failure: %a" pp_print_trace err
      | Ok f -> failwith "unexpected success: %a" Fitness_repr.pp f)
    test_bad_cases

let test_compare (tuple1, tuple2) =
  let fitness1 = tuple_to_fitness_exn tuple1 in
  let fitness2 = tuple_to_fitness_exn tuple2 in
  let raw_fitness1 = Fitness_repr.to_raw fitness1 in
  let raw_fitness2 = Fitness_repr.to_raw fitness2 in
  let cmp_fitness = Fitness_repr.Internal_for_tests.compare fitness1 fitness2 in
  let cmp_raw_fitness = Fitness.compare raw_fitness1 raw_fitness2 in
  Assert.equal_int ~loc:__LOC__ cmp_fitness cmp_raw_fitness

let test_compare_all () = List.iter_es test_compare test_product_cases

let tests =
  [
    Tztest.tztest
      "from/to raw fitness is identity"
      `Quick
      test_from_to_raw_fitness_all;
    Tztest.tztest "locked round is smaller than round" `Quick test_locked_round;
    Tztest.tztest
      "compare fitness = compare raw_fitness"
      `Quick
      test_compare_all;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("fitness", tests)] |> Lwt_main.run
