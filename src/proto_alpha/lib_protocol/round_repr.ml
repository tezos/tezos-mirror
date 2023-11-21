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

let succ n =
  if Compare.Int32.equal n Int32.max_int then
    invalid_arg "round_repr.succ: cannot apply succ to maximum round value"
  else Int32.succ n

let pp fmt i = Format.fprintf fmt "%ld" i

type error += Negative_round of int

type error += Round_overflow of int

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"negative_round"
    ~title:"Negative round"
    ~description:"Round cannot be built out of negative integers."
    ~pp:(fun ppf i ->
      Format.fprintf
        ppf
        "Negative round cannot be built out of negative integers (%Ld)"
        i)
    (obj1 (req "Negative_round" int64))
    (function Negative_round i -> Some (Int64.of_int i) | _ -> None)
    (fun i -> Negative_round (Int64.to_int i)) ;
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
    (obj1 (req "Round_overflow" int64))
    (function Round_overflow i -> Some (Int64.of_int i) | _ -> None)
    (fun i -> Round_overflow (Int64.to_int i))

let of_int32 i =
  let open Result_syntax in
  if i >= 0l then return i else tzfail (Negative_round (Int32.to_int i))
  [@@inline]

let pred r =
  let p = Int32.pred r in
  of_int32 p

let of_int i =
  let open Result_syntax in
  if Compare.Int.(i < 0) then tzfail (Negative_round i)
  else
    (* i is positive *)
    let i32 = Int32.of_int i in
    if Compare.Int.(Int32.to_int i32 = i) then Ok i32
    else tzfail (Round_overflow i)

let to_int i32 =
  let open Result_syntax in
  let i = Int32.to_int i32 in
  if Int32.(equal (of_int i) i32) then return i else tzfail (Round_overflow i)

let to_int32 t = t [@@inline]

let to_slot round ~committee_size =
  let open Result_syntax in
  let* r = to_int round in
  let slot = r mod committee_size in
  Slot_repr.of_int slot

let encoding =
  Data_encoding.conv_with_guard
    (fun i -> i)
    (fun i ->
      match of_int32 i with
      | Ok _ as res -> res
      | Error _ -> Error "Round_repr.encoding: negative round")
    Data_encoding.int32

module Durations = struct
  type t = {
    first_round_duration : Period_repr.t;
    delay_increment_per_round : Period_repr.t;
  }

  type error +=
    | Non_increasing_rounds of {increment : Period_repr.t}
    | Round_durations_must_be_at_least_one_second of {round : Period_repr.t}

  let () =
    register_error_kind
      `Permanent
      ~id:"durations.non_increasing_rounds"
      ~title:"Non increasing round"
      ~description:"The provided rounds are not increasing."
      ~pp:(fun ppf increment ->
        Format.fprintf
          ppf
          "The provided rounds are not increasing (increment: %a)"
          Period_repr.pp
          increment)
      Data_encoding.(obj1 (req "increment" Period_repr.encoding))
      (function
        | Non_increasing_rounds {increment} -> Some increment | _ -> None)
      (fun increment -> Non_increasing_rounds {increment})

  let pp fmt t =
    Format.fprintf
      fmt
      "%a,@ +%a"
      Period_repr.pp
      t.first_round_duration
      Period_repr.pp
      t.delay_increment_per_round

  let create ~first_round_duration ~delay_increment_per_round =
    let open Result_syntax in
    let* () =
      error_when
        Compare.Int64.(Period_repr.to_seconds first_round_duration < 1L)
        (Round_durations_must_be_at_least_one_second
           {round = first_round_duration})
    in
    let* () =
      error_when
        Compare.Int64.(Period_repr.to_seconds delay_increment_per_round < 1L)
        (Non_increasing_rounds {increment = delay_increment_per_round})
    in
    return {first_round_duration; delay_increment_per_round}

  let create_opt ~first_round_duration ~delay_increment_per_round =
    match create ~first_round_duration ~delay_increment_per_round with
    | Ok v -> Some v
    | Error _ -> None

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun {first_round_duration; delay_increment_per_round} ->
        (first_round_duration, delay_increment_per_round))
      (fun (first_round_duration, delay_increment_per_round) ->
        match create_opt ~first_round_duration ~delay_increment_per_round with
        | None ->
            Error
              "Either round durations are non-increasing or minimal block \
               delay < 1"
        | Some rounds -> Ok rounds)
      (obj2
         (req "first_round_duration" Period_repr.encoding)
         (req "delay_increment_per_round" Period_repr.encoding))

  let round_duration {first_round_duration; delay_increment_per_round} round =
    if Compare.Int32.(round < 0l) then
      invalid_arg "round must be a non-negative integer"
    else
      let first_round_duration_s = Period_repr.to_seconds first_round_duration
      and delay_increment_per_round_s =
        Period_repr.to_seconds delay_increment_per_round
      in
      Period_repr.of_seconds_exn
        Int64.(
          add
            first_round_duration_s
            (mul (of_int32 round) delay_increment_per_round_s))
end

type error += Round_too_high of int32

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"round_too_high"
    ~title:"round too high"
    ~description:"block round too high."
    ~pp:(fun ppf round ->
      Format.fprintf ppf "Block round is too high: %ld" round)
    (obj1 (req "level_offset_too_high" int32))
    (function Round_too_high round -> Some round | _ -> None)
    (fun round -> Round_too_high round)

(* The duration of round n follows the arithmetic sequence:

        round_duration(0)   = first_round_duration
        round_duration(r+1) = round_duration(r) + delay_increment_per_round

      Hence, this sequence can be explicited into:

        round_duration(r) = first_round_duration + r * delay_increment_per_round

      The level offset of round r is the sum of the durations of the rounds up
      until round r - 1. In other words, when r > 0

        raw_level_offset_of_round(0)   = 0
        raw_level_offset_of_round(r+1) =
          raw_level_offset_of_round(r) + round_duration(r)

   Hence

        raw_level_offset_of_round(r) = Σ_{k=0}^{r-1} (round_duration(k))

      After unfolding the series, the same function can be finally explicited into

        raw_level_offset_of_round(0) = 0
        raw_level_offset_of_round(r) = r * first_round_duration
                                   + 1/2 * r * (r - 1) * delay_increment_per_round
*)
let raw_level_offset_of_round round_durations ~round =
  let open Result_syntax in
  if Compare.Int32.(round = zero) then return Int64.zero
  else
    let sum_durations =
      let Durations.{first_round_duration; delay_increment_per_round} =
        round_durations
      in
      let roundz = Int64.of_int32 round in
      let m = Z.of_int64 Int64.(div (mul roundz (pred roundz)) 2L) in
      Z.(
        add
          (mul
             m
             (Z.of_int64 @@ Period_repr.to_seconds delay_increment_per_round))
          (mul
             (Z.of_int32 round)
             (Z.of_int64 @@ Period_repr.to_seconds first_round_duration)))
    in
    if Z.fits_int64 sum_durations then return (Z.to_int64 sum_durations)
    else tzfail (Round_too_high round)

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

(** Complexity: O(log level_offset). *)
let round_and_offset round_durations ~level_offset =
  let open Result_syntax in
  let level_offset_in_seconds = Period_repr.to_seconds level_offset in
  (* We set the bound as 2^53 to prevent overflows when computing the
     variable [discr] for reasonable values of [first_round_duration] and
     [delay_increment_per_round]. This bound is derived by a rough approximation
     from the inequation [discr] < Int64.max_int. *)
  let overflow_bound = Int64.shift_right Int64.max_int 10 in
  if Compare.Int64.(overflow_bound < level_offset_in_seconds) then
    tzfail (Level_offset_too_high level_offset)
  else
    let Durations.{first_round_duration; delay_increment_per_round} =
      round_durations
    in
    let first_round_duration = Period_repr.to_seconds first_round_duration in
    let delay_increment_per_round =
      Period_repr.to_seconds delay_increment_per_round
    in
    (* If [level_offset] is lower than the first round duration, then
       the solution straightforward. *)
    if Compare.Int64.(level_offset_in_seconds < first_round_duration) then
      return {round = 0l; offset = level_offset}
    else
      let round =
        if Compare.Int64.(delay_increment_per_round = Int64.zero) then
          (* Case when delay_increment_per_round is zero and a simple
             linear solution exists. *)
          Int64.div level_offset_in_seconds first_round_duration
        else
          (* Case when the increment is non-negative and we look for the
             quadratic solution. *)
          let pow_2 n = Int64.mul n n in
          let double n = Int64.shift_left n 1 in
          let times_8 n = Int64.shift_left n 3 in
          let half n = Int64.shift_right n 1 in
          (* The integer square root is implemented using the Newton-Raphson
             method. For any integer N, the convergence within the
             neighborhood of √N is ensured within log2 (N) steps. *)
          let sqrt (n : int64) =
            let x0 = ref (half n) in
            if Compare.Int64.(!x0 > 1L) then (
              let x1 = ref (half (Int64.add !x0 (Int64.div n !x0))) in
              while Compare.Int64.(!x1 < !x0) do
                x0 := !x1 ;
                x1 := half (Int64.add !x0 (Int64.div n !x0))
              done ;
              !x0)
            else n
          in
          (* The idea is to solve the following equation in [round] and
             use its integer value:

             Σ_{k=0}^{round-1} round_duration(k) = level_offset

             After unfolding the sum and expanding terms, we obtain a
             quadratic equation:

             delay_increment_per_round × round²
               + (2 first_round_duration - delay_increment_per_round) × round
               - 2 level_offset
                 = 0

             From there, we compute the discriminant and the solution of
             the equation.

             Refer to https://gitlab.com/tezos/tezos/-/merge_requests/4009
             for more explanations.
          *)
          let discr =
            Int64.add
              (pow_2
                 (Int64.sub
                    (double first_round_duration)
                    delay_increment_per_round))
              (times_8
                 (Int64.mul delay_increment_per_round level_offset_in_seconds))
          in
          Int64.div
            (Int64.add
               (Int64.sub
                  delay_increment_per_round
                  (double first_round_duration))
               (sqrt discr))
            (double delay_increment_per_round)
      in
      let* current_level_offset =
        raw_level_offset_of_round round_durations ~round:(Int64.to_int32 round)
      in
      return
        {
          round = Int64.to_int32 round;
          offset =
            Period_repr.of_seconds_exn
              (Int64.sub
                 (Period_repr.to_seconds level_offset)
                 current_level_offset);
        }

(** Complexity: O(|round_durations|). *)
let timestamp_of_round round_durations ~predecessor_timestamp ~predecessor_round
    ~round =
  let open Result_syntax in
  let pred_round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  (* First, the function computes when the current level l is supposed
     to start. This is given by adding to the timestamp of the round
     of predecessor level l-1 [predecessor_timestamp], the duration of
     its last round [predecessor_round]. *)
  let* start_of_current_level =
    Time_repr.(predecessor_timestamp +? pred_round_duration)
  in
  (* Finally, we sum the durations of the rounds at the current level l until
     reaching current [round]. *)
  let* level_offset = raw_level_offset_of_round round_durations ~round in
  let level_offset = Period_repr.of_seconds_exn level_offset in
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
  let open Result_syntax in
  let* target_offset =
    raw_level_offset_of_round round_durations ~round:considered_round
  in
  let* current_offset =
    raw_level_offset_of_round round_durations ~round:current_round
  in
  return
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
  let open Result_syntax in
  let round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  let* start_of_current_level =
    Time_repr.(predecessor_timestamp +? round_duration)
  in
  let* diff =
    Period_repr.of_seconds (Time_repr.diff timestamp start_of_current_level)
    |> Error_monad.record_trace
         (Round_of_past_timestamp
            {
              predecessor_timestamp;
              provided_timestamp = timestamp;
              predecessor_round;
            })
  in
  let* round_and_offset = round_and_offset round_durations ~level_offset:diff in
  return round_and_offset.round

let level_offset_of_round round_durations ~round =
  let open Result_syntax in
  let* offset = raw_level_offset_of_round round_durations ~round in
  return (Period_repr.of_seconds_exn offset)

module Internals_for_test = struct
  type round_and_offset_raw = {round : round; offset : Period_repr.t}

  let round_and_offset round_durations ~level_offset =
    let open Result_syntax in
    let+ v = round_and_offset round_durations ~level_offset in
    {round = v.round; offset = v.offset}
end
