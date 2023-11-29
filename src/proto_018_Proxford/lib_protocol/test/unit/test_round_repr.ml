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
    Component:    protocol
    Invocation:   dune exec src/proto_018_Proxford/lib_protocol/test/unit/main.exe \
                  -- --file test_round_repr.ml
    Subject:      test the Round_repr module
*)

open Protocol
open Alpha_context

type round_test = {
  (* input: round; output: round duration *)
  round_duration : (int * int) list;
  (* input: level offset; output: round, round offset *)
  round_and_offset : (int * (int * int)) list;
  (* input: pred_ts, pred_round, round; output: ts *)
  timestamp_of_round : ((int * int * int) * int) list;
  (* input: pred_ts, pred_round, ts; output: round *)
  round_of_timestamp : ((int * int * int) * int) list;
}

(* an association list of the input, output values *)
let case_3_4 =
  {
    round_duration = [(0, 3); (1, 4); (2, 5); (3, 6)];
    round_and_offset =
      [
        (0, (0, 0));
        (1, (0, 1));
        (2, (0, 2));
        (3, (1, 0));
        (4, (1, 1));
        (5, (1, 2));
        (6, (1, 3));
        (7, (2, 0));
        (8, (2, 1));
      ];
    timestamp_of_round = [((100, 0, 6), 136); ((100, 1, 6), 137)];
    round_of_timestamp =
      [
        ((100, 0, 121), 4);
        ((100, 0, 122), 4);
        ((100, 0, 123), 4);
        ((100, 0, 124), 4);
        ((100, 0, 125), 4);
        ((100, 0, 126), 4);
        ((100, 1, 121), 3);
        ((100, 1, 122), 4);
        ((100, 1, 123), 4);
        ((100, 1, 124), 4);
        ((100, 1, 125), 4);
        ((100, 1, 126), 4);
      ];
  }

let case_3_6 =
  {
    round_duration =
      [
        (0, 3);
        (1, 6);
        (2, 9);
        (3, 12);
        (4, 15);
        (5, 18);
        (6, 21);
        (7, 24);
        (8, 27);
      ];
    round_and_offset =
      [
        (0, (0, 0));
        (1, (0, 1));
        (2, (0, 2));
        (3, (1, 0));
        (4, (1, 1));
        (5, (1, 2));
        (6, (1, 3));
        (7, (1, 4));
        (8, (1, 5));
        (9, (2, 0));
        (10, (2, 1));
        (11, (2, 2));
        (97, (7, 13));
      ];
    timestamp_of_round =
      [
        ((100, 0, 0), 103);
        ((100, 1, 0), 106);
        ((100, 0, 6), 166);
        ((100, 1, 6), 169);
      ];
    round_of_timestamp =
      [
        ((100, 0, 103), 0);
        ((100, 0, 104), 0);
        ((100, 0, 105), 0);
        ((100, 0, 106), 1);
        ((100, 0, 111), 1);
        ((100, 0, 112), 2);
        ((100, 0, 120), 2);
        ((100, 0, 121), 3);
        ((100, 0, 132), 3);
        ((100, 0, 133), 4);
        ((100, 1, 106), 0);
        ((100, 1, 107), 0);
        ((100, 1, 108), 0);
        ((100, 1, 109), 1);
        ((100, 1, 114), 1);
        ((100, 1, 115), 2);
      ];
  }

let test_cases =
  [
    (* (first_round_duration, delay_increment_per_round), test_case_expectations *)
    ((3, 1), case_3_4, "case_3_4");
    ((3, 3), case_3_6, "case_3_6");
  ]

let round_of_int i = Round_repr.of_int i

let mk_round_durations first_round_duration delay_increment_per_round =
  let first_round_duration =
    Period_repr.of_seconds_exn @@ Int64.of_int first_round_duration
  in
  let delay_increment_per_round =
    Period_repr.of_seconds_exn @@ Int64.of_int delay_increment_per_round
  in
  (* We assume test specifications do respect round_durations
     invariants and cannot fail *)
  Stdlib.Option.get
  @@ Round_repr.Durations.create_opt
       ~first_round_duration
       ~delay_increment_per_round

let process_test_case (round_durations, ios, _) =
  let open Lwt_result_wrap_syntax in
  let open Round_repr in
  let* () =
    List.iter_es
      (fun (i, o) ->
        let*?@ round = round_of_int i in
        let dur = Durations.round_duration round_durations round in
        Assert.equal_int64
          ~loc:__LOC__
          (Int64.of_int o)
          (Period_repr.to_seconds dur))
      ios.round_duration
  in
  let open Internals_for_test in
  (* test [round_and_offset] *)
  let* () =
    List.iter_es
      (fun (level_offset, (round, ro)) ->
        let level_offset =
          Period_repr.of_seconds_exn (Int64.of_int level_offset)
        in
        let*?@ round_and_offset =
          round_and_offset round_durations ~level_offset
        in
        let* () =
          Assert.equal_int32
            ~loc:__LOC__
            (Int32.of_int round)
            (Round_repr.to_int32 round_and_offset.round)
        in
        Assert.equal_int64
          ~loc:__LOC__
          (Int64.of_int ro)
          (Period_repr.to_seconds round_and_offset.offset))
      ios.round_and_offset
  in
  (* test [timestamp_of_round] *)
  let* () =
    List.iter_es
      (fun ((pred_ts, pred_round, round), o) ->
        let predecessor_timestamp =
          Time_repr.of_seconds (Int64.of_int pred_ts)
        in
        let*@ ts =
          let*? predecessor_round = Round_repr.of_int pred_round in
          let*? round = Round_repr.of_int round in
          Lwt.return
            (timestamp_of_round
               round_durations
               ~predecessor_timestamp
               ~predecessor_round
               ~round)
        in
        Assert.equal_int64
          ~loc:__LOC__
          (Int64.of_int o)
          (Time_repr.to_seconds ts))
      ios.timestamp_of_round
  in
  (* test [round_of_timestamp] *)
  List.iter_es
    (fun ((pred_ts, pred_round, ts), o) ->
      let predecessor_timestamp = Time_repr.of_seconds (Int64.of_int pred_ts) in
      let*@ round =
        let*? predecessor_round = Round_repr.of_int pred_round in
        Lwt.return
          (round_of_timestamp
             round_durations
             ~predecessor_timestamp
             ~predecessor_round
             ~timestamp:(Time_repr.of_seconds (Int64.of_int ts)))
      in
      Assert.equal_int32
        ~loc:__LOC__
        (Int32.of_int o)
        (Round_repr.to_int32 round))
    ios.round_of_timestamp

let test_round () =
  let final_test_cases =
    List.map
      (fun ((first_round_duration, delay_increment_per_round), ios, name) ->
        ( mk_round_durations first_round_duration delay_increment_per_round,
          ios,
          name ))
      test_cases
  in
  (* TODO this could be run in the error monad instead of lwt *)
  List.iter_es process_test_case final_test_cases

let ts_add ts period =
  match Timestamp.(ts +? period) with
  | Ok ts' -> ts'
  | Error _ -> Environment.Pervasives.failwith "timestamp add"

let test_round_of_timestamp () =
  let open Lwt_result_wrap_syntax in
  let duration0 = Period.of_seconds_exn 1L in
  let*?@ round_durations =
    Round.Durations.create
      ~first_round_duration:duration0
      ~delay_increment_per_round:Period.one_second
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let level_start = ts_add predecessor_timestamp duration0 in
  let rec loop ~expected_round ~elapsed_time =
    if elapsed_time < 1000 then
      let timestamp =
        ts_add level_start (Period.of_seconds_exn (Int64.of_int elapsed_time))
      in
      match
        Round.round_of_timestamp
          round_durations
          ~predecessor_timestamp
          ~timestamp
          ~predecessor_round:Round.zero
      with
      | Ok round ->
          let* () =
            Assert.equal_int32
              ~loc:__LOC__
              (Round.to_int32 round)
              (Int32.of_int expected_round)
          in
          let elapsed_time = elapsed_time + (expected_round + 1)
          and expected_round = 1 + expected_round in
          loop ~expected_round ~elapsed_time
      | Error _ -> failwith "error "
    else return_unit
  in
  loop ~elapsed_time:0 ~expected_round:0

let round_of_timestamp_perf (duration0_int64, dipr) =
  let open Lwt_result_wrap_syntax in
  let duration0 = Period.of_seconds_exn duration0_int64 in
  let delay_increment_per_round = Period.of_seconds_exn dipr in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~first_round_duration:duration0
         ~delay_increment_per_round
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let level_start = ts_add predecessor_timestamp duration0 in
  let max_ts = Int64.(sub (of_int32 Int32.max_int) duration0_int64) in
  let rec loop i =
    let open Result_syntax in
    if i >= 0L then (
      let repeats = 100 in
      let rec loop_inner sum j =
        if j > 0 then
          let timestamp =
            ts_add level_start (Period.of_seconds_exn (Int64.sub max_ts i))
          in
          let t0 = Unix.gettimeofday () in
          let* (_round : Round.t) =
            Round.round_of_timestamp
              round_durations
              ~predecessor_timestamp
              ~timestamp
              ~predecessor_round:Round.zero
          in
          let t1 = Unix.gettimeofday () in
          let time = t1 -. t0 in
          loop_inner (sum +. time) (j - 1)
        else return sum
      in
      let* sum = loop_inner 0.0 repeats in
      let time = sum /. float_of_int repeats in
      assert (time < 0.01) ;
      loop (Int64.pred i))
    else return_unit
  in
  let*?@ () = loop 1000L in
  return_unit

let default_round_durations_list =
  [(1L, 1L); (1L, 2L); (1L, 3L); (2L, 3L); (2L, 4L)]

let test_round_of_timestamp_perf () =
  List.iter_es round_of_timestamp_perf default_round_durations_list

let timestamp_of_round_perf (duration0_int64, dipr) =
  let open Lwt_result_wrap_syntax in
  let duration0 = Period.of_seconds_exn duration0_int64 in
  let delay_increment_per_round = Period.of_seconds_exn dipr in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~first_round_duration:duration0
         ~delay_increment_per_round
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let rec loop i =
    let open Result_syntax in
    if i >= 0l then (
      let* round = Round.of_int32 Int32.(sub max_int i) in
      let t0 = Unix.gettimeofday () in
      let* (_ts : Timestamp.time) =
        Round.timestamp_of_round
          round_durations
          ~predecessor_timestamp
          ~predecessor_round:Round.zero
          ~round
      in
      let t1 = Unix.gettimeofday () in
      let time = t1 -. t0 in
      assert (time < 0.01) ;
      loop (Int32.pred i))
    else return_unit
  in
  let*?@ () = loop 1000l in
  return_unit

let test_timestamp_of_round_perf () =
  List.iter_es timestamp_of_round_perf default_round_durations_list

let test_error_is_triggered_for_too_high_timestamp () =
  let open Result_wrap_syntax in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~first_round_duration:Period.one_second
         ~delay_increment_per_round:Period.one_second
  in

  let predecessor_timestamp = Time.Protocol.epoch in
  let@ res =
    Round.round_of_timestamp
      round_durations
      ~predecessor_timestamp
      ~predecessor_round:Round.zero
      ~timestamp:(Time_repr.of_seconds Int64.max_int)
  in
  match res with
  | Error _ ->
      Assert.proto_error_with_info ~loc:__LOC__ res "level offset too high"
  | Ok _ -> Assert.error ~loc:__LOC__ res (fun _ -> false)

let rec ( --> ) i j =
  (* [i; i+1; ...; j] *)
  if Compare.Int.(i > j) then [] else i :: (succ i --> j)

let ts_of_round_inverse (duration0_int64, dipr) round_int =
  let open Lwt_result_wrap_syntax in
  let first_round_duration = Period.of_seconds_exn duration0_int64 in
  let delay_increment_per_round = Period.of_seconds_exn dipr in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~first_round_duration
         ~delay_increment_per_round
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let predecessor_round = Round.zero in
  let*?@ round = Round.of_int round_int in
  let*?@ timestamp =
    Round.timestamp_of_round
      round_durations
      ~predecessor_timestamp
      ~predecessor_round
      ~round
  in
  let*?@ round' =
    Round.round_of_timestamp
      round_durations
      ~predecessor_timestamp
      ~predecessor_round
      ~timestamp
  in
  let*?@ round' = Round.to_int round' in
  Assert.equal_int ~loc:__LOC__ round_int round'

(* We restrict to round 134,217,727 as rounds above can lead to
   integer overflow in [Round_repr.round_and_offset] and are already prevented
   by returning an error. *)
let test_ts_of_round_inverse () =
  let open Lwt_result_syntax in
  let* () =
    List.iter_es
      (fun durations ->
        List.iter_es
          (ts_of_round_inverse durations)
          ((0 --> 20) @ (60000 --> 60010)))
      default_round_durations_list
  in
  List.iter_es
    (ts_of_round_inverse (1L, 1L))
    (List.map (fun i -> Int32.to_int 134_217_727l - i) (1 --> 20))

let round_of_ts_inverse ~first_round_duration ~delay_increment_per_round ts =
  let open Lwt_result_wrap_syntax in
  Format.printf "ts = %Ld@." ts ;
  let first_round_duration = Period.of_seconds_exn first_round_duration in
  let delay_increment_per_round =
    Period.of_seconds_exn delay_increment_per_round
  in
  let*?@ round_durations =
    Round.Durations.create ~first_round_duration ~delay_increment_per_round
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let predecessor_round = Round.zero in
  let*?@ level_start =
    Timestamp.( +? ) predecessor_timestamp first_round_duration
  in
  let start_of_round timestamp =
    let*?@ round =
      Round.round_of_timestamp
        round_durations
        ~predecessor_timestamp
        ~predecessor_round
        ~timestamp
    in
    let*?@ t =
      Round.timestamp_of_round
        round_durations
        ~predecessor_timestamp
        ~predecessor_round
        ~round
    in
    return (round, t)
  in
  let*?@ timestamp = Period.of_seconds_exn ts |> Timestamp.( +? ) level_start in
  let* round, ts_start_of_round = start_of_round timestamp in
  let* () =
    Assert.leq_int64
      ~loc:__LOC__
      (Timestamp.to_seconds ts_start_of_round)
      (Timestamp.to_seconds timestamp)
  in
  let pred ts = Period.one_second |> Timestamp.( - ) ts in
  let rec iter ts =
    let* round', ts_start_of_round' = start_of_round ts in
    let* () =
      Assert.equal_int64
        ~loc:__LOC__
        (Timestamp.to_seconds ts_start_of_round)
        (Timestamp.to_seconds ts_start_of_round')
    in
    let* () =
      Assert.equal_int32
        ~loc:__LOC__
        (Round.to_int32 round)
        (Round.to_int32 round')
    in
    if Timestamp.(ts > ts_start_of_round') then iter (pred ts) else return_unit
  in
  if Timestamp.(timestamp > ts_start_of_round) then iter (pred timestamp)
  else return_unit

let test_round_of_ts_inverse () =
  let open Lwt_result_syntax in
  let* () =
    List.iter_es
      (fun (first_round_duration, delay_increment_per_round) ->
        List.iter_es
          (fun ts ->
            round_of_ts_inverse
              ~first_round_duration
              ~delay_increment_per_round
              (Int64.of_int ts))
          ((0 --> 20) @ (60000 --> 60010)))
      default_round_durations_list
  in
  List.iter_es
    (fun ts ->
      Format.printf "%Ld@." ts ;
      round_of_ts_inverse
        ~first_round_duration:1L
        ~delay_increment_per_round:2L
        ts)
    (List.map
       (fun i -> Int64.of_int (Int32.to_int Int32.max_int - i))
       (0 --> 20))

let test_level_offset_of_round () =
  let open Lwt_result_wrap_syntax in
  let rd1 =
    let first_round_duration = 3 in
    let delay_increment_per_round = 1 in
    mk_round_durations first_round_duration delay_increment_per_round
  in
  List.iter_es
    (fun (round_durations, tests) ->
      List.iter_es
        (fun (round, expected_offset) ->
          let*?@ round = Round_repr.of_int round in
          let*?@ computed_offset =
            Round_repr.level_offset_of_round round_durations ~round
          in
          Assert.equal_int64
            ~loc:__LOC__
            (Period_repr.to_seconds computed_offset)
            (Int64.of_int expected_offset))
        tests)
    [
      (rd1, [(0, 0); (1, 3); (2, 7)]);
      (mk_round_durations 3 3, [(0, 0); (1, 3); (2, 9); (3, 18)]);
    ]

(* This is the previous implementation, serving as an oracle *)
let round_and_offset_oracle (round_durations : Round_repr.Durations.t)
    ~level_offset =
  let open Result_syntax in
  let level_offset_in_seconds = Period_repr.to_seconds level_offset in
  (* We have the invariant [round <= level_offset] so there is no need to search
     beyond [level_offset]. We set [right_bound] to [level_offset + 1] to avoid
     triggering the error level_offset too high when the round equals
     [level_offset]. *)
  let right_bound =
    if Compare.Int64.(level_offset_in_seconds < Int64.of_int32 Int32.max_int)
    then Int32.of_int (Int64.to_int level_offset_in_seconds + 1)
    else Int32.max_int
  in
  let rec bin_search min_r max_r =
    if Compare.Int32.(min_r >= right_bound) then invalid_arg "foo"
    else
      let* round =
        Round_repr.of_int32 @@ Int32.(add min_r (div (sub max_r min_r) 2l))
      in
      let next_round = Round_repr.succ round in
      let* next_level_offset =
        Round_repr.level_offset_of_round round_durations ~round:next_round
      in
      if Period_repr.(level_offset >= next_level_offset) then
        bin_search (Round_repr.to_int32 next_round) max_r
      else
        let* current_level_offset =
          Round_repr.level_offset_of_round round_durations ~round
        in
        if Period_repr.(level_offset < current_level_offset) then
          bin_search min_r (Round_repr.to_int32 round)
        else
          return
            Round_repr.Internals_for_test.
              {
                round;
                offset =
                  Period_repr.of_seconds_exn
                    (Int64.sub
                       (Period_repr.to_seconds level_offset)
                       (Period_repr.to_seconds current_level_offset));
              }
  in
  bin_search 0l right_bound

(* Test whether the new version is equivalent to the old one *)
let test_round_and_offset_correction =
  let open Lwt_result_wrap_syntax in
  Tztest.tztest_qcheck2
    ~name:"round_and_offset is correct"
    QCheck2.(
      Gen.pair
        Qcheck2_helpers.(Gen.pair uint16 uint16)
        (Qcheck2_helpers.int64_range_gen 0L 100000L))
    (fun ((first_round_duration, delay_increment_per_round), level_offset) ->
      QCheck2.assume (first_round_duration > 0) ;
      QCheck2.assume (delay_increment_per_round > 0) ;
      let first_round_duration =
        Period_repr.of_seconds_exn (Int64.of_int first_round_duration)
      and delay_increment_per_round =
        Period_repr.of_seconds_exn (Int64.of_int delay_increment_per_round)
      and level_offset = Period_repr.of_seconds_exn level_offset in
      let round_duration =
        Stdlib.Option.get
          (Round_repr.Durations.create_opt
             ~first_round_duration
             ~delay_increment_per_round)
      in
      let@ expected = round_and_offset_oracle round_duration ~level_offset in
      let computed =
        Round_repr.Internals_for_test.round_and_offset
          round_duration
          ~level_offset
      in
      match (computed, expected) with
      | Error _, Error _ -> return_unit
      | Ok {round; offset}, Ok {round = round'; offset = offset'} ->
          let* () =
            Assert.equal_int32
              ~loc:__LOC__
              (Round_repr.to_int32 round)
              (Round_repr.to_int32 round')
          in
          Assert.equal_int64
            ~loc:__LOC__
            (Period_repr.to_seconds offset)
            (Period_repr.to_seconds offset')
      | Ok _, Error _ -> failwith "expected error is ok"
      | Error _, Ok _ -> failwith "expected ok is error")

let tests =
  Tztest.
    [
      tztest "level_offset_of_round" `Quick test_level_offset_of_round;
      tztest "Round_duration" `Quick test_round;
      tztest "round_of_timestamp" `Quick test_round_of_timestamp;
      tztest "round_of_timestamp_perf" `Quick test_round_of_timestamp_perf;
      tztest "timestamp_of_round_perf" `Quick test_timestamp_of_round_perf;
      tztest
        "level offset too high error is triggered"
        `Quick
        test_error_is_triggered_for_too_high_timestamp;
      tztest "round_of_ts (ts_of_round r) = r" `Quick test_ts_of_round_inverse;
      tztest
        "ts_of_round (round_of_ts ts) <= ts"
        `Quick
        test_round_of_ts_inverse;
      test_round_and_offset_correction;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("round", tests)] |> Lwt_main.run
