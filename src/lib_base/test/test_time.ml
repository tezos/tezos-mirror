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

(* Testing
   -------
   Component:    Base
   Invocation:   dune exec src/lib_base/test/main.exe
   Subject:      Check that the Protocol and System times behave correctly
                 regarding addition and encoding (binary and JSON)
*)

open Time
open Qcheck2_helpers

module Protocol = struct
  include Protocol
  open QCheck2

  let max_rfc3339_seconds = to_seconds max_rfc3339

  let min_rfc3339_seconds = to_seconds min_rfc3339

  let gen = Gen.(map of_seconds int64)

  let rfc3339_compatible_t_gen =
    let open Gen in
    let within_rfc3339 =
      map of_seconds (int64_range_gen min_rfc3339_seconds max_rfc3339_seconds)
    in
    frequency
      [
        (97, within_rfc3339);
        (1, pure max_rfc3339);
        (1, pure min_rfc3339);
        (1, pure epoch);
      ]

  let pp fmt t = Format.fprintf fmt "%Lx" (to_seconds t)

  let print = Format.asprintf "%a" pp

  let add_diff_roundtrip =
    Test.make
      ~name:"Protocol.[add|diff] roundtrip"
      Gen.(pair gen int64)
      (fun (some_time, delta) ->
        let other_time = add some_time delta in
        let actual = diff other_time some_time in
        qcheck_eq'
          ~pp:(fun fmt i64 -> Format.fprintf fmt "%Lx" i64)
          ~eq:Int64.equal
          ~expected:delta
          ~actual
          ())

  let diff_add_roundtrip =
    Test.make
      ~name:"Protocol.[diff|add] roundtrip"
      (Gen.pair gen gen)
      (fun (some_time, other_time) ->
        let delta = diff other_time some_time in
        let actual = add some_time delta in
        qcheck_eq' ~pp ~eq:equal ~expected:other_time ~actual ())

  let encoding_binary_roundtrip =
    Test.make ~name:"Protocol.encoding roundtrips in binary" gen (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let actual = Data_encoding.Binary.of_bytes_exn encoding b in
        qcheck_eq' ~pp ~eq:equal ~expected:t ~actual ())

  let encoding_json_roundtrip =
    Test.make ~name:"Protocol.encoding roundtrips in JSON" gen (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let actual = Data_encoding.Json.destruct encoding j in
        qcheck_eq' ~pp ~eq:equal ~expected:t ~actual ())

  let encoding_to_notation_roundtrip =
    Test.make
      ~name:"Protocol.[to|of]_notation roundtrip in RFC3339 range"
      ~print
      rfc3339_compatible_t_gen
      (fun t ->
        to_notation t |> of_notation |> function
        | None -> Test.fail_report "Failed to roundtrip notation"
        | Some actual -> qcheck_eq' ~pp ~eq:equal ~expected:t ~actual ())

  let tests =
    [
      add_diff_roundtrip;
      diff_add_roundtrip;
      encoding_binary_roundtrip;
      encoding_json_roundtrip;
      encoding_to_notation_roundtrip;
    ]
end

module System = struct
  open System
  open QCheck2

  let gen_date : (int * int * int) Gen.t =
    let open Gen in
    let thirty_one = [1; 3; 5; 7; 8; 10; 12] |> List.map pure |> oneof in
    let thirty = [4; 6; 9; 11] |> List.map pure |> oneof in

    let gen_month_day =
      oneof
        [
          pair thirty_one (1 -- 31);
          pair thirty (1 -- 30);
          pair (pure 2) (1 -- 28);
        ]
    in

    map
      (fun (year, (month, day)) -> (year, month, day))
      (pair (0 -- 9999) gen_month_day)

  (** Generator of {!t} from usual time fragments year-month-day hour-minute-second, parsed through {!Ptime.of_date_time}. *)
  let t_ymdhms_gen : t Gen.t =
    Gen.(
      pair gen_date (triple (0 -- 23) (0 -- 59) (0 -- 60))
      |> map (fun (date, time) ->
             Ptime.of_date_time (date, (time, 0)) |> Option.get))

  let min_day, min_ps = Ptime.min |> Ptime.to_span |> Ptime.Span.to_d_ps

  let max_day, max_ps = Ptime.max |> Ptime.to_span |> Ptime.Span.to_d_ps

  (** Gen.T of {!t} from days + picoseconds, parsed through {!Ptime.Span.of_d_ps}. *)
  let t_dps_gen : t Gen.t =
    let open Gen in
    pair (min_day -- max_day) (int64_range_gen min_ps max_ps)
    |> map (fun (d, ps) ->
           Ptime.Span.of_d_ps (d, ps)
           |> Option.get |> Ptime.of_span |> Option.get)

  let gen = Gen.oneof [t_ymdhms_gen; t_dps_gen]

  let print = Format.asprintf "%a" pp_hum

  (** Check that the span is smaller than 1 second (useful for Protocol time roundtrips as Protocol time precision is the second). *)
  let is_small delta =
    Stdlib.( < )
      (Ptime.Span.compare delta (Ptime.Span.v (0, 1_000_000_000_000L)))
      0

  let to_protocol_of_protocol_roundtrip =
    Test.make
      ~name:"System.[to|of]_protocol roundtrip modulo option"
      ~print
      gen
      (fun t ->
        match to_protocol t |> of_protocol_opt with
        | None -> Test.fail_report "Failed roundtrip"
        | Some actual ->
            let delta = Ptime.Span.abs @@ Ptime.diff t actual in
            is_small delta)

  (** Since Protocol time domain is (vastly) bigger than System time domain,
      converting a Protocol time to a System time:
      - either succeeds, in which case we can roundtrip back to the original
        Protocol time
      - or the Protocol time must be out of the System time range (i.e. out
        of the RFC3339 time range)
  *)
  let of_protocol_to_protocol_roundtrip_or_outside_rfc3339_with_gen gen =
    Test.make
      ~name:"System.[of|to]_protocol roundtrip or outside RFC3339 range"
        (* Use both generators, otherwise statistically, we will almost
            never hit the RFC3339 time range. *)
      ~print:Protocol.print
      gen
      (fun protocol_time ->
        match of_protocol_opt protocol_time with
        | None ->
            Protocol.(
              protocol_time < min_rfc3339 || max_rfc3339 < protocol_time)
        | Some system_time ->
            let actual = to_protocol system_time in
            qcheck_eq'
              ~pp:Time.Protocol.pp
              ~eq:Time.Protocol.equal
              ~expected:protocol_time
              ~actual
              ())

  let of_protocol_to_protocol_roundtrip_or_outside_rfc3339 =
    of_protocol_to_protocol_roundtrip_or_outside_rfc3339_with_gen
      Gen.(oneof [Protocol.gen; Protocol.rfc3339_compatible_t_gen])

  let of_protocol_to_protocol_roundtrip_or_outside_rfc3339_int_sizes_trap =
    let interesting_values =
      Int64.
        [
          max_int;
          div max_int 2L;
          min_int;
          0xf0_00_00_00_00L;
          0xf0_00_00_00_02L;
          0xf0_00_00_00_10L;
          0xff_ff_f0_00_00_00_10L;
          logor 2932890L 0xff_00_00_00_00_00L;
          logor 18L 0xff_00_00_00_00_00L;
        ]
    in
    of_protocol_to_protocol_roundtrip_or_outside_rfc3339_with_gen
      Gen.(oneofl (List.map Protocol.of_seconds interesting_values))

  let rfc_encoding_binary_roundtrip =
    Test.make
      ~name:"System.rfc_encoding roundtrips in binary modulo precision"
      ~print
      gen
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn rfc_encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn rfc_encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let rfc_encoding_json_roundtrip =
    Test.make
      ~name:"System.rfc_encoding roundtrips in JSON modulo precision"
      ~print
      gen
      (fun t ->
        let j = Data_encoding.Json.construct rfc_encoding t in
        let tt = Data_encoding.Json.destruct rfc_encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let encoding_binary_roundtrip =
    Test.make
      ~name:"System.encoding roundtrips in binary modulo precision"
      ~print
      gen
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let encoding_json_roundtrip =
    Test.make
      ~name:"System.encoding roundtrips in JSON modulo precision"
      ~print
      gen
      (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let tt = Data_encoding.Json.destruct encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let tests =
    [
      to_protocol_of_protocol_roundtrip;
      of_protocol_to_protocol_roundtrip_or_outside_rfc3339;
      of_protocol_to_protocol_roundtrip_or_outside_rfc3339_int_sizes_trap;
      rfc_encoding_binary_roundtrip;
      rfc_encoding_json_roundtrip;
      encoding_binary_roundtrip;
      encoding_json_roundtrip;
    ]
end

let () =
  Alcotest.run
    "Time"
    [
      ("Protocol", qcheck_wrap Protocol.tests);
      ("System", qcheck_wrap System.tests);
    ]
