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

type round = int32

type t = round

module Map = Map.Make (Int32)

include (Compare.Int32 : Compare.S with type t := t)

let zero = 0l

let succ = Int32.succ

let pp fmt i = Format.fprintf fmt "%ld" i

type error += Negative_round of int32

type error += Round_overflow of int

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"negative_round"
    ~title:"Negative round"
    ~description:"Round cannot be built out of negative int32."
    ~pp:(fun ppf i ->
      Format.fprintf
        ppf
        "Negative round cannot be built out of negative int32 (%ld)"
        i)
    (obj1 (req "Negative_round" int32))
    (function Negative_round i -> Some i | _ -> None)
    (fun i -> Negative_round i) ;
  register_error_kind
    `Permanent
    ~id:"round_overflow"
    ~title:"Round overflow"
    ~description:
      "Round cannot be built out of integer greater than maximum int32 value."
    ~pp:(fun ppf i ->
      Format.fprintf
        ppf
        "Round cannot be built out of integer greater than maximum int32 value \
         (%Ld)"
        i)
    (obj1 (req "Negative_round" int64))
    (function Round_overflow i -> Some (Int64.of_int i) | _ -> None)
    (fun i -> Round_overflow (Int64.to_int i))

let of_int32 i = if i >= 0l then Ok i else error (Negative_round i) [@@inline]

let pred r =
  let p = Int32.pred r in
  of_int32 p

let of_int i =
  let i32 = Int32.of_int i in
  if Compare.Int.(Int32.to_int i32 = i) then of_int32 i32
  else error (Round_overflow i)

let to_int i32 =
  let i = Int32.to_int i32 in
  if Int32.(equal (of_int i) i32) then ok i else error (Round_overflow i)

let to_int32 t = t [@@inline]

let to_slot round ~committee_size =
  to_int round >|? fun r ->
  let slot = r mod committee_size in
  Slot_repr.of_int_do_not_use_except_for_parameters slot

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun i -> i)
    (fun i ->
      match of_int32 i with
      | Ok _ as res -> res
      | Error _ -> Error "Round_repr.encoding: negative round")
    Data_encoding.int32

module Durations = struct
  type error +=
    | Non_increasing_rounds of {
        round : Period_repr.t;
        next_round : Period_repr.t;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:"durations.non_increasing_rounds"
      ~title:"Non increasing round"
      ~description:"The provided rounds are not increasing."
      ~pp:(fun ppf (round, next_round) ->
        Format.fprintf
          ppf
          "The provided rounds are not increasing (round: %a, next round: %a)"
          Period_repr.pp
          round
          Period_repr.pp
          next_round)
      Data_encoding.(
        obj2
          (req "round" Period_repr.encoding)
          (req "next_round" Period_repr.encoding))
      (function
        | Non_increasing_rounds {round; next_round} -> Some (round, next_round)
        | _ -> None)
      (fun (round, next_round) -> Non_increasing_rounds {round; next_round})

  type t = Period_repr.t list

  let pp fmt l =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        Period_repr.pp)
      fmt
      l

  let rec check_ordered = function
    | [_] | [] -> Result.return_unit
    | r0 :: (r1 :: _ as rs) ->
        error_when
          Period_repr.(r0 > r1)
          (Non_increasing_rounds {round = r0; next_round = r1})
        >>? fun () -> check_ordered rs

  let create ?(other_rounds = []) ~round0 ~round1 () =
    error_when
      Period_repr.(round1 < round0)
      (Non_increasing_rounds {round = round0; next_round = round1})
    >>? fun () ->
    match other_rounds with
    | [] -> ok [round0; round1]
    | r :: _ ->
        error_when
          Period_repr.(round1 > r)
          (Non_increasing_rounds {round = round1; next_round = r})
        >>? fun () ->
        check_ordered other_rounds >>? fun () ->
        ok (round0 :: round1 :: other_rounds)

  let create_opt ?(other_rounds = []) ~round0 ~round1 () =
    match create ~other_rounds ~round0 ~round1 () with
    | Ok v -> Some v
    | Error _ -> None

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun l -> l)
      (function
        | round0 :: round1 :: other_rounds -> (
            match create_opt ~round0 ~round1 ~other_rounds () with
            | None -> Error "The provided round durations are not increasing."
            | Some rounds -> Ok rounds)
        | [] | [_] ->
            Error "Round durations are expected to have at least two elements")
      (Data_encoding.list Period_repr.encoding)

  let round_duration round_durations round =
    assert (Compare.Int32.(round >= 0l)) ;
    match round_durations with
    | duration0 :: duration1 :: durations ->
        if Compare.Int32.(round = 0l) then duration0
        else if Compare.Int32.(round = 1l) then duration1
        else
          let rec loop i ultimate penultimate = function
            | d :: ds ->
                if Compare.Int32.(i = 0l) then d
                else loop (Int32.pred i) d ultimate ds
            | [] ->
                (* The last element of the list is the ultimate *)
                let last = Period_repr.to_seconds ultimate in
                let last_but_one = Period_repr.to_seconds penultimate in
                let diff = Int64.sub last last_but_one in
                assert (Compare.Int64.(diff >= 0L)) ;
                let offset = Int32.succ i in
                let duration = Int64.(add last (mul diff (of_int32 offset))) in
                Period_repr.of_seconds_exn duration
          in
          loop (Int32.sub round 2l) duration1 duration0 durations
    | _ ->
        (* Durations are at least length 2, so this should not happen *)
        assert false

  let first = function h :: _ -> h | _ -> assert false
end

type error += Round_too_high of int32

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"round_too_high"
    ~title:"round too high"
    ~description:"The block's round is too high."
    ~pp:(fun ppf round ->
      Format.fprintf ppf "The block's round is too high: %ld" round)
    (obj1 (req "level_offset_too_high" int32))
    (function Round_too_high round -> Some round | _ -> None)
    (fun round -> Round_too_high round)

(** [level_offset_of_round round] returns the time period between the
    start of round 0 and the start of round [round]. That is, the sum
    of the duration of rounds [0] to [round-1]. Note that [round] is
    necessarily a positive number. *)
let level_offset_of_round (round_durations : Durations.t) ~round =
  (* Auxiliary function to return a pair of the last element of the
     list and the sum the [round - 1]-th first elements of
     [round_durations]. *)
  let rec last_and_sum_loop round_durations ~round ~sum_acc =
    match round_durations with
    | [] -> assert false
    | [last] when Compare.Int32.(round <> Int32.zero) ->
        (last, Int64.add sum_acc (Period_repr.to_seconds last))
    | d :: round_durations' ->
        if Compare.Int32.(round = Int32.zero) then (d, sum_acc)
        else
          last_and_sum_loop
            round_durations'
            ~round:(Int32.pred round)
            ~sum_acc:(Int64.add sum_acc (Period_repr.to_seconds d))
  in
  let parameters_len = Int32.of_int (List.length round_durations) in
  if round < parameters_len then
    (* Let τ be the sequence of round durations (exactly as computed
       by function [duration_of_round]). We just sum the constants
       in [round_durations]:
       Σ_{k = 0}^{round - 1} (τ_k)
    *)
    ok (snd (last_and_sum_loop round_durations ~round ~sum_acc:Int64.zero))
  else
    (* Instead of recursively adding durations given by calling
       function [round_duration], basic algebra gives the same result
       in constant-time (as the infinite-sequence of round durations
       is affine after the initial values). Let n be the length of
       [round_durations] and let τ be the sequence of round durations
       (exactly as computed by function [duration_of_round]). We have:

       Σ_{k = 0}^{round - 1} (τ_k)
         = Σ_{k = 0}^{n - 2} (τ_k)                                (1)
           + 1/2 * (round - n + 1) * (τ_{n-1} + τ_{round - 1})     (2)

       Note that τ_{n-1} designates the last value of list
       [round_durations].
    *)

    (* 1. Sum the constants in [round_durations] until the last but
       one. *)
    let (round_durations_last, sum_round_durations) =
      last_and_sum_loop
        round_durations
        ~round:(Int32.pred parameters_len)
        ~sum_acc:Int64.zero
    in
    (* 2. Compute the rest of the terms arithmetically (instead of
       recursively). *)
    let sum_after_round_durations =
      let round_durations_last = Period_repr.to_seconds round_durations_last
      and after_round_durations_last =
        Period_repr.to_seconds
          (Durations.round_duration round_durations (Int32.pred round))
      in
      Int64.div
        (Int64.mul
           (Int64.add round_durations_last after_round_durations_last)
           (Int64.succ
              (Int64.sub (Int64.of_int32 round) (Int64.of_int32 parameters_len))))
        (Int64.of_int 2)
    in
    (* We might get an overflow when round reaches Int32.max_int and
       round_durations are bigger than 1. *)
    if Compare.Int64.(sum_after_round_durations < 0L) then
      error (Round_too_high round)
    else ok (Int64.add sum_round_durations sum_after_round_durations)

type error += Level_offset_too_high of Period_repr.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"level_offset_too_high"
    ~title:"level offset too high"
    ~description:"The block's level offset is too high."
    ~pp:(fun ppf offset ->
      Format.fprintf
        ppf
        "The block's level offset is too high: %a"
        Period_repr.pp
        offset)
    (obj1 (req "level_offset_too_high" Period_repr.encoding))
    (function Level_offset_too_high offset -> Some offset | _ -> None)
    (fun offset -> Level_offset_too_high offset)

type round_and_offset = {round : int32; offset : Period_repr.t}

(** Complexity: in the worst case, O(log max_int)*O(|round_durations|^2).
    Normally, [level_offset] is small enough for [check_first] to
    return the searched round, thus the binary search will not be performed.
    [check_first] can be made to run in O(round_duration) but it is kept
    this way for simplicity given that currently |round_durations| = 2. *)
let round_and_offset round_durations ~level_offset =
  let level_offset_in_seconds = Period_repr.to_seconds level_offset in
  let rec check_first ~max_round round =
    if Compare.Int.(Int32.to_int round >= max_round) then ok None
    else
      level_offset_of_round round_durations ~round:(Int32.succ round)
      >>? fun next_level_offset ->
      if Compare.Int64.(level_offset_in_seconds < next_level_offset) then
        level_offset_of_round round_durations ~round
        >>? fun current_level_offset ->
        ok
          (Some
             {
               round;
               offset =
                 Period_repr.of_seconds_exn
                   (Int64.sub
                      (Period_repr.to_seconds level_offset)
                      current_level_offset);
             })
      else check_first ~max_round (Int32.succ round)
  in
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
    let round = Int32.(add min_r (div (sub max_r min_r) 2l)) in
    if Compare.Int32.(min_r = right_bound) then
      error (Level_offset_too_high level_offset)
    else if Compare.Int32.(min_r = Int32.pred max_r) then
      bin_search min_r (Int32.succ max_r)
    else
      level_offset_of_round round_durations ~round:(Int32.succ round)
      >>? fun next_level_offset ->
      if
        Compare.Int64.(Period_repr.to_seconds level_offset >= next_level_offset)
      then bin_search (Int32.succ round) max_r
      else
        level_offset_of_round round_durations ~round
        >>? fun current_level_offset ->
        if
          Compare.Int64.(
            Period_repr.to_seconds level_offset < current_level_offset)
        then bin_search min_r (Int32.pred round)
        else
          ok
            {
              round;
              offset =
                Period_repr.of_seconds_exn
                  (Int64.sub
                     (Period_repr.to_seconds level_offset)
                     current_level_offset);
            }
  in
  let n = List.length round_durations in
  check_first ~max_round:n 0l >>? fun res ->
  match res with
  | Some result -> ok result
  | None -> bin_search (Int32.of_int n) right_bound

(** Complexity: O(|round_durations|). *)
let timestamp_of_round round_durations ~predecessor_timestamp ~predecessor_round
    ~round =
  let pred_round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  (* First, the function computes when the current level l is supposed
     to start. This is given by adding to the timestamp of the round
     of predecessor level l-1 [predecessor_timestamp], the duration of
     its last round [predecessor_round]. *)
  Time_repr.(predecessor_timestamp +? pred_round_duration)
  >>? fun start_of_current_level ->
  level_offset_of_round round_durations ~round >>? fun level_offset ->
  let level_offset = Period_repr.of_seconds_exn level_offset in
  (* Finally, we sum of round durations of current level l until
     reaching current [round]. *)
  Time_repr.(start_of_current_level +? level_offset)

(** Unlike [timestamp_of_round], this function gets the starting time
    of a given round, given the timestamp and the round of a proposal
    at the same level.

    We compute the starting time of [considered_round] from a given
    [round_durations] description, some [current_round], and its
    starting time [current_timestamp].

    Complexity: O(|round_durations|). *)
let timestamp_of_another_round_same_level round_durations ~current_timestamp
    ~current_round ~considered_round =
  level_offset_of_round round_durations ~round:considered_round
  >>? fun target_offset ->
  level_offset_of_round round_durations ~round:current_round
  >>? fun current_offset ->
  ok
  @@ Time_repr.of_seconds
       Int64.(
         add
           (sub (Time_repr.to_seconds current_timestamp) current_offset)
           target_offset)

type error +=
  | Round_of_past_timestamp of {
      provided_timestamp : Time.t;
      predecessor_timestamp : Time.t;
      predecessor_round : t;
    }

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"round_of_past_timestamp"
    ~title:"Round_of_timestamp for past timestamp"
    ~description:"Provided timestamp is before the expected level start."
    ~pp:(fun ppf (provided_ts, predecessor_ts, round) ->
      Format.fprintf
        ppf
        "Provided timestamp (%a) is before the expected level start (computed \
         based on predecessor_ts %a at round %a)."
        Time.pp_hum
        provided_ts
        Time.pp_hum
        predecessor_ts
        pp
        round)
    (obj3
       (req "provided_timestamp" Time.encoding)
       (req "predecessor_timestamp" Time.encoding)
       (req "predecessor_round" encoding))
    (function
      | Round_of_past_timestamp
          {provided_timestamp; predecessor_timestamp; predecessor_round} ->
          Some (provided_timestamp, predecessor_timestamp, predecessor_round)
      | _ -> None)
    (fun (provided_timestamp, predecessor_timestamp, predecessor_round) ->
      Round_of_past_timestamp
        {provided_timestamp; predecessor_timestamp; predecessor_round})

let round_of_timestamp round_durations ~predecessor_timestamp ~predecessor_round
    ~timestamp =
  let round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  Time_repr.(predecessor_timestamp +? round_duration)
  >>? fun start_of_current_level ->
  Period_repr.of_seconds (Time_repr.diff timestamp start_of_current_level)
  |> Error_monad.record_trace
       (Round_of_past_timestamp
          {
            predecessor_timestamp;
            provided_timestamp = timestamp;
            predecessor_round;
          })
  >>? fun diff ->
  round_and_offset round_durations ~level_offset:diff
  >>? fun round_and_offset -> ok round_and_offset.round

let level_offset_of_round round_durations ~round =
  level_offset_of_round round_durations ~round >>? fun offset ->
  ok (Period_repr.of_seconds_exn offset)
