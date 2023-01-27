(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Plugin.Mempool
    Invocation:   dune exec src/proto_017_PtNairob/lib_plugin/test/main.exe \
                  -- --file test_consensus_filter.ml
    Subject:      Unit tests the Mempool consensus filter
*)

open Qcheck2_helpers
open Plugin.Mempool
open Alpha_context

(** {2. Conversion helpers} *)

let timestamp_of_int32 ts = Timestamp.of_seconds (Int64.of_int32 ts)

(** Data Generators *)
module Generator = struct
  open QCheck2.Gen

  let decorate ?(prefix = "") ?(suffix = "") printer d =
    prefix ^ printer d ^ suffix

  let config =
    let* drift_int_opt = opt small_nat in
    let clock_drift =
      Option.map
        (fun drift_int -> Period.of_seconds_exn (Int64.of_int drift_int))
        drift_int_opt
    in
    return {default_config with clock_drift}

  let print_config =
    decorate ~prefix:"clock_drift " (fun config ->
        Option.fold
          ~none:"round_0 duration"
          ~some:(fun drift -> Int64.to_string @@ Period.to_seconds drift)
          config.clock_drift)

  let of_result = Result.value_f ~default:(fun _ -> assert false)

  let small_nat_32 =
    let+ small_nat in
    Int32.of_int small_nat

  let small_signed_32 =
    let+ small_signed_int in
    Int32.of_int small_signed_int

  let dup gen =
    let+ x = gen in
    (x, x)

  let round =
    let+ x = map (fun i32 -> Round.of_int32 i32) small_nat_32 in
    of_result x

  let print_round = Format.asprintf "%a" Round.pp

  let same_rounds = dup round

  let level =
    let+ x = map Raw_level.of_int32 small_nat_32 in
    of_result x

  let print_level = Format.asprintf "%a" Raw_level.pp

  let same_levels = dup level

  let timestamp =
    let+ i32 = int32 in
    timestamp_of_int32 i32

  let print_timestamp = Timestamp.to_notation

  let near_timestamps =
    let+ i, diff = pair int32 small_signed_32 in
    timestamp_of_int32 i |> fun ts1 ->
    timestamp_of_int32 Int32.(add i diff) |> fun ts2 -> (ts1, ts2)

  let successive_timestamp =
    let+ ts, (diff : int) = pair timestamp small_nat in
    let x =
      Period.of_seconds (Int64.of_int diff) >>? fun diff ->
      Timestamp.(ts +? diff) >>? fun ts2 -> Ok (ts, ts2)
    in
    of_result x

  let param_acceptable ?(rounds = pair round round) ?(levels = pair level level)
      ?(timestamps = near_timestamps) () =
    pair config (pair (pair rounds levels) timestamps)

  let print_param_acceptable =
    let open QCheck2.Print in
    let print_levels = pair print_level print_level in
    let print_timestamps = pair print_timestamp print_timestamp in
    let print_rounds = pair print_round print_round in
    pair print_config (pair (pair print_rounds print_levels) print_timestamps)
end

let assert_no_error d = match d with Error _ -> assert false | Ok d -> d

(** Constants :
    This could be generated but it would largely increase the search space. *)
let round_durations : Round.round_durations =
  assert_no_error
  @@ Round.Durations.create
       ~first_round_duration:Period.(of_seconds_exn 4L)
       ~delay_increment_per_round:Period.(of_seconds_exn 10L)

let round_zero_duration = Round.round_duration round_durations Round.zero

(** Don't allow test to fail  *)
let no_error = function
  | Ok b -> b
  | Error errs ->
      Format.printf
        "test fail due to error : %a@."
        Error_monad.pp_print_trace
        (Environment.wrap_tztrace errs) ;
      false

(** Helper to compute  *)
let durations round_durations start stop =
  List.map_e
    (fun round ->
      Round.of_int round >|? fun round ->
      Round.round_duration round_durations round |> Period.to_seconds)
    Tezos_stdlib.Utils.Infix.(start -- stop)

(** Expected timestamp for the begining of a round at same level that
    the proposal.

    It has been developped before the  Round.timestamp_of_round_same_level and has a
    different implementation.

*)
let timestamp_of_round round_durations ~proposal_timestamp ~proposal_round
    ~round =
  (let iproposal_round = Int32.to_int @@ Round.to_int32 proposal_round in
   let iround = Int32.to_int @@ Round.to_int32 round in
   if Round.(proposal_round = round) then ok (Period.zero, proposal_timestamp)
   else if Round.(proposal_round < round) then
     durations round_durations iproposal_round (iround - 1) >>? fun durations ->
     Period.of_seconds @@ List.fold_left Int64.add Int64.zero durations
     >>? fun rounds_duration ->
     Timestamp.(proposal_timestamp +? rounds_duration) >|? fun ts ->
     (rounds_duration, ts)
   else
     durations round_durations iround (iproposal_round - 1) >>? fun durations ->
     List.fold_left Int64.add Int64.zero durations |> fun rounds_duration ->
     Timestamp.of_seconds
     @@ Int64.sub (Timestamp.to_seconds proposal_timestamp) rounds_duration
     |> fun ts ->
     Period.of_seconds rounds_duration >|? fun rounds_duration ->
     (rounds_duration, ts))
  >>? fun (_rnd_dur, exp_ts) -> ok exp_ts

let drift_of =
  let r0_dur = Round.round_duration round_durations Round.zero in
  fun clock_drift -> Option.value ~default:r0_dur clock_drift

(** [max_ts] computes the upper bound on future timestamps given the
    accepted round drift.
*)
let max_ts clock_drift prop_ts now =
  Timestamp.(max prop_ts now +? drift_of clock_drift)

let predecessor_start proposal_timestamp proposal_round grandparent_round =
  assert_no_error
  @@ ( Round.level_offset_of_round
         round_durations
         ~round:Round.(succ grandparent_round)
     >>? fun proposal_level_offset ->
       Round.level_offset_of_round round_durations ~round:proposal_round
       >>? fun proposal_round_offset ->
       Period.(add proposal_level_offset proposal_round_offset)
       >>? fun proposal_offset ->
       Ok Timestamp.(proposal_timestamp - proposal_offset) )

(** {2. Tests} *)

(** Test past operations that are accepted whatever the current timestamp is:
   strictly before the predecessor level or at the current level and with a
   strictly lower round than the head. *)

let test_acceptable_past_level =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"acceptable past op "
    (Generator.param_acceptable ())
    (fun
      ( config,
        ( ((proposal_round, op_round), (proposal_level, op_level)),
          (proposal_timestamp, now_timestamp) ) )
    ->
      Raw_level.(
        proposal_level > succ op_level
        || (proposal_level = op_level && Round.(proposal_round > op_round)))
      ==> no_error
          @@ acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp)

(** Test acceptable operations at current level, current round, i.e. on the
   currently considered proposal *)
let test_acceptable_current_level_current_round =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"same round, same level "
    Generator.(param_acceptable ~rounds:same_rounds ~levels:same_levels ())
    (fun ( config,
           (((op_round, _), (_, op_level)), (proposal_timestamp, now_timestamp))
         ) ->
      let proposal_level = op_level in
      let proposal_round = op_round in
      no_error
      @@ acceptable_op
           ~config
           ~round_durations
           ~round_zero_duration
           ~proposal_level
           ~proposal_round
           ~proposal_timestamp
           ~proposal_predecessor_level_start:
             (predecessor_start proposal_timestamp proposal_round Round.zero)
           ~op_level
           ~op_round
           ~now_timestamp)

(** Test operations at same level, different round, with an acceptable expected
    timestamp for the operation.   *)
let test_acceptable_current_level =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"same level, different round, acceptable op"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (_, op_level)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let proposal_level = op_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:op_round
        >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp -> ok Timestamp.(expected_time <= max_timestamp)
        )
      ==> no_error
          @@ acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp)

(** Test operations at same level, different round, with a too high expected
    timestamp for the operation,  and not at current round (which is always accepted). *)
let test_not_acceptable_current_level =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"same level, different round, too far"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (_, op_level)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let proposal_level = op_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:op_round
        >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp ->
          ok
            Timestamp.(
              expected_time > max_timestamp
              && Round.(proposal_round <> op_round)) )
      ==> no_error
            (acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp
            >|? not))

(** Test operations at next level, different round, with an acceptable timestamp for
    the operation. *)
let test_acceptable_next_level =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"next level, acceptable op"
    Generator.(param_acceptable ~levels:same_levels ())
    (fun ( config,
           ( ((proposal_round, op_round), (proposal_level, _)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let op_level = Raw_level.succ proposal_level in
      no_error
        ( timestamp_of_round
            round_durations
            ~proposal_timestamp
            ~proposal_round
            ~round:Round.zero
        >>? fun current_level_start ->
          Round.timestamp_of_round
            round_durations
            ~predecessor_timestamp:current_level_start
            ~predecessor_round:Round.zero
            ~round:op_round
          >>? fun expected_time ->
          max_ts config.clock_drift proposal_timestamp now_timestamp
          >>? fun max_timestamp -> ok Timestamp.(expected_time <= max_timestamp)
        )
      ==> no_error
          @@ acceptable_op
               ~config
               ~round_durations
               ~round_zero_duration
               ~proposal_level
               ~proposal_round
               ~proposal_timestamp
               ~proposal_predecessor_level_start:
                 (predecessor_start
                    proposal_timestamp
                    proposal_round
                    Round.zero)
               ~op_level
               ~op_round
               ~now_timestamp)

(** Test operations at next level, different round, with a too high timestamp
   for the operation. *)
let test_not_acceptable_next_level =
  let open QCheck2 in
  Test.make
    ~print:Generator.print_param_acceptable
    ~name:"next level, too far"
    Generator.(
      param_acceptable ~levels:same_levels ~timestamps:successive_timestamp ())
    (fun ( config,
           ( ((proposal_round, op_round), (proposal_level, _)),
             (proposal_timestamp, now_timestamp) ) ) ->
      let op_level = Raw_level.succ proposal_level in
      QCheck2.assume
      @@ no_error
           ( timestamp_of_round
               round_durations
               ~proposal_timestamp
               ~proposal_round
               ~round:Round.zero
           >>? fun current_level_start ->
             Round.timestamp_of_round
               round_durations
               ~predecessor_timestamp:current_level_start
               ~predecessor_round:Round.zero
               ~round:op_round
             >>? fun expected_time ->
             Timestamp.(
               proposal_timestamp
               +? Round.round_duration round_durations proposal_round)
             >>? fun next_level_ts ->
             max_ts config.clock_drift next_level_ts now_timestamp
             >>? fun max_timestamp ->
             ok Timestamp.(expected_time > max_timestamp) ) ;
      no_error
      @@ (acceptable_op
            ~config
            ~round_durations
            ~round_zero_duration
            ~proposal_level
            ~proposal_round
            ~proposal_timestamp
            ~proposal_predecessor_level_start:
              (predecessor_start proposal_timestamp proposal_round Round.zero)
            ~op_level
            ~op_round
            ~now_timestamp
         >|? not))

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [
      ( "pre_filter",
        qcheck_wrap
          [
            test_acceptable_past_level;
            test_acceptable_current_level_current_round;
            test_acceptable_current_level;
            test_not_acceptable_current_level;
            test_acceptable_next_level;
            test_not_acceptable_next_level;
          ] );
    ]
