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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^round$"
    Subject:      test the Round_repr module
*)

open Protocol
open Alpha_context

let round_duration round_durations ~round =
  match List.nth_opt round_durations (Int32.to_int round) with
  | Some duration -> duration
  | None -> (
      let len = List.length round_durations in
      match List.rev round_durations with
      | last :: last_but_one :: _ ->
          let last = Period_repr.to_seconds last in
          let last_but_one = Period_repr.to_seconds last_but_one in
          let diff = Int64.sub last last_but_one in
          assert (Compare.Int64.(diff >= 0L)) ;
          let offset =
            Int64.sub (Int64.of_int32 round) (Int64.of_int (len - 1))
          in
          let duration = Int64.add last (Int64.mul diff offset) in
          Period_repr.of_seconds_exn duration
      | _ -> assert false)

let test_round_repr () =
  let round0 = Period_repr.one_second in
  let round1 = Period_repr.of_seconds_exn 4L in
  let round_durations =
    Stdlib.Option.get @@ Round_repr.Durations.create_opt ~round0 ~round1 ()
  in
  let rec check_rounds = function
    | [] -> return_unit
    | round :: rounds -> (
        let duration = round_duration ~round [round0; round1] in
        match Round_repr.of_int32 round with
        | Error _ -> failwith "Incorrect round value %ld" round
        | Ok round ->
            if
              Period_repr.equal
                (Round_repr.Durations.round_duration round_durations round)
                duration
            then check_rounds rounds
            else
              failwith
                "Incorrect computed duration for round %a@."
                Round_repr.pp
                round)
  in
  check_rounds [0l; 1l; 2l; 3l; 4l; 5l]

let ( >>>=? ) v f = v >|= Environment.wrap_tzresult >>=? f

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
let case_3_3 =
  {
    round_duration = [(0, 3); (1, 3); (2, 3); (3, 3)];
    round_and_offset =
      [
        (0, (0, 0));
        (1, (0, 1));
        (2, (0, 2));
        (3, (1, 0));
        (4, (1, 1));
        (5, (1, 2));
        (6, (2, 0));
        (7, (2, 1));
        (8, (2, 2));
      ];
    timestamp_of_round = [((100, 0, 6), 121); ((100, 1, 6), 121)];
    round_of_timestamp =
      [
        ((100, 0, 121), 6);
        ((100, 0, 122), 6);
        ((100, 0, 123), 6);
        ((100, 0, 124), 7);
        ((100, 0, 125), 7);
        ((100, 0, 126), 7);
        ((100, 1, 121), 6);
        ((100, 1, 122), 6);
        ((100, 1, 123), 6);
        ((100, 1, 124), 7);
        ((100, 1, 125), 7);
        ((100, 1, 126), 7);
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
    ([3; 3], case_3_3);
    ([3; 3; 3], case_3_3);
    ([3; 6], case_3_6);
    ([3; 6; 9], case_3_6);
  ]

let round_of_int i = Round_repr.of_int i |> Environment.wrap_tzresult

let process_test_case (round_durations, ios) =
  let open Round_repr in
  (match
     List.map
       (fun x -> Period_repr.of_seconds_exn (Int64.of_int x))
       round_durations
   with
  | round0 :: round1 :: other_rounds ->
      Durations.create ~round0 ~round1 ~other_rounds ()
      |> Environment.wrap_tzresult
  | _ -> assert false)
  >>?= fun round_durations ->
  (* test [round_duration] *)
  List.iter_es
    (fun (i, o) ->
      round_of_int i >>?= fun round ->
      let dur = Durations.round_duration round_durations round in
      Assert.equal_int64
        ~loc:__LOC__
        (Int64.of_int o)
        (Period_repr.to_seconds dur))
    ios.round_duration
  >>=? fun () ->
  (* test [round_and_offset] *)
  List.iter_es
    (fun (i, (r, ro)) ->
      let level_offset = Period_repr.of_seconds_exn (Int64.of_int i) in
      Environment.wrap_tzresult (round_and_offset round_durations ~level_offset)
      >>?= fun round_and_offset ->
      Assert.equal_int32
        ~loc:__LOC__
        (Int32.of_int r)
        (Round_repr.to_int32 round_and_offset.round)
      >>=? fun () ->
      Assert.equal_int64
        ~loc:__LOC__
        (Int64.of_int ro)
        (Period_repr.to_seconds round_and_offset.offset))
    ios.round_and_offset
  >>=? fun () ->
  (* test [timestamp_of_round] *)
  List.iter_es
    (fun ((pred_ts, pred_round, round), o) ->
      let predecessor_timestamp = Time_repr.of_seconds (Int64.of_int pred_ts) in
      Lwt.return
        ( Round_repr.of_int pred_round >>? fun predecessor_round ->
          Round_repr.of_int round >>? fun round ->
          timestamp_of_round
            round_durations
            ~predecessor_timestamp
            ~predecessor_round
            ~round )
      >>>=? fun ts ->
      Assert.equal_int64 ~loc:__LOC__ (Int64.of_int o) (Time_repr.to_seconds ts))
    ios.timestamp_of_round
  >>=? fun () ->
  (* test [round_of_timestamp] *)
  List.iter_es
    (fun ((pred_ts, pred_round, ts), o) ->
      let predecessor_timestamp = Time_repr.of_seconds (Int64.of_int pred_ts) in
      Lwt.return
        ( Round_repr.of_int pred_round >>? fun predecessor_round ->
          round_of_timestamp
            round_durations
            ~predecessor_timestamp
            ~predecessor_round
            ~timestamp:(Time_repr.of_seconds (Int64.of_int ts)) )
      >>>=? fun round ->
      Assert.equal_int32
        ~loc:__LOC__
        (Int32.of_int o)
        (Round_repr.to_int32 round))
    ios.round_of_timestamp

let test_round () =
  (* TODO this could be run in the error monad instead of lwt *)
  List.iter_es process_test_case test_cases

let ts_add ts period =
  match Timestamp.(ts +? period) with
  | Ok ts' -> ts'
  | Error _ -> Environment.Pervasives.failwith "timestamp add"

let test_round_of_timestamp () =
  let duration0 = Period.of_seconds_exn 1L in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt ~round0:duration0 ~round1:duration0 ()
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let diff = ref 0 in
  let level_start = ts_add predecessor_timestamp duration0 in
  while !diff < 1000 do
    let timestamp =
      ts_add level_start (Period.of_seconds_exn (Int64.of_int !diff))
    in
    match
      Round.round_of_timestamp
        round_durations
        ~predecessor_timestamp
        ~timestamp
        ~predecessor_round:Round.zero
    with
    | Ok round ->
        assert (Round.to_int32 round = Int32.of_int !diff) ;
        diff := !diff + 1
    | _ -> assert false
  done ;
  return_unit

let round_of_timestamp_perf durations =
  let duration0_int64 = Stdlib.List.hd durations in
  let duration0 = Period.of_seconds_exn duration0_int64 in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~round0:duration0
         ~round1:(Period.of_seconds_exn (Stdlib.List.nth durations 1))
         ()
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let level_start = ts_add predecessor_timestamp duration0 in
  let max_ts = Int64.(sub (of_int32 Int32.max_int) duration0_int64) in
  let rec loop i =
    if i >= 0L then (
      let timestamp =
        ts_add level_start (Period.of_seconds_exn (Int64.sub max_ts i))
      in
      let t0 = Unix.gettimeofday () in
      Round.round_of_timestamp
        round_durations
        ~predecessor_timestamp
        ~timestamp
        ~predecessor_round:Round.zero
      >>? fun _round ->
      let t1 = Unix.gettimeofday () in
      let time = t1 -. t0 in
      assert (time < 0.01) ;
      loop (Int64.pred i))
    else ok ()
  in
  Environment.wrap_tzresult (loop 1000L) >>?= fun () -> return_unit

let test_round_of_timestamp_perf () =
  List.iter_es
    round_of_timestamp_perf
    [[1L; 1L]; [1L; 2L]; [1L; 3L]; [2L; 3L]; [2L; 4L]]

let timestamp_of_round_perf durations =
  let duration0 = Period.of_seconds_exn (Stdlib.List.hd durations) in
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~round0:duration0
         ~round1:(Period.of_seconds_exn (Stdlib.List.nth durations 1))
         ()
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let rec loop i =
    if i >= 0l then (
      Round.of_int32 Int32.(sub max_int i) >>? fun round ->
      let t0 = Unix.gettimeofday () in
      Round.timestamp_of_round
        round_durations
        ~predecessor_timestamp
        ~predecessor_round:Round.zero
        ~round
      >>? fun _ts ->
      let t1 = Unix.gettimeofday () in
      let time = t1 -. t0 in
      assert (time < 0.01) ;
      loop (Int32.pred i))
    else ok ()
  in
  Environment.wrap_tzresult (loop 1000l) >>?= fun () -> return_unit

let test_timestamp_of_round_perf () =
  List.iter_es
    timestamp_of_round_perf
    [[1L; 1L]; [1L; 2L]; [1L; 3L]; [2L; 3L]; [2L; 4L]]

let test_error_is_triggered_for_too_high_timestamp () =
  let round_durations =
    Stdlib.Option.get
    @@ Round.Durations.create_opt
         ~round0:(Period.of_seconds_exn 1L)
         ~round1:(Period.of_seconds_exn 1L)
         ()
  in
  let predecessor_timestamp = Time.Protocol.epoch in
  let res =
    Round.round_of_timestamp
      round_durations
      ~predecessor_timestamp
      ~predecessor_round:Round.zero
      ~timestamp:(Time_repr.of_seconds Int64.max_int)
  in
  let res = Environment.wrap_tzresult res in
  match res with
  | Error _ ->
      Assert.proto_error ~loc:__LOC__ res (function err ->
          let error_info =
            Error_monad.find_info_of_error (Environment.wrap_tzerror err)
          in
          error_info.title = "level offset too high")
  | Ok _ -> Assert.error ~loc:__LOC__ res (fun _ -> false)

let tests =
  Tztest.
    [
      tztest "round_duration [0..5]" `Quick test_round_repr;
      tztest "test Round_duration" `Quick test_round;
      tztest "round_of_timestamp" `Quick test_round_of_timestamp;
      tztest "round_of_timestamp_perf" `Quick test_round_of_timestamp_perf;
      tztest "timestamp_of_round_perf" `Quick test_timestamp_of_round_perf;
      tztest
        "test level offset too high error is triggered"
        `Quick
        test_error_is_triggered_for_too_high_timestamp;
    ]
