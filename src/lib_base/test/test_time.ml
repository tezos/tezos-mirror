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
   Invocation:   dune exec src/lib_base/test/test_time.exe
   Subject:      Check that the Protocol and System times behave correctly
                 regarding addition and encoding (binary and JSON)
*)

open Time
open Lib_test.Qcheck_helpers

module Protocol = struct
  include Protocol

  let max_rfc3339_seconds = to_seconds max_rfc3339

  let min_rfc3339_seconds = to_seconds min_rfc3339

  let t_arb = QCheck.map ~rev:to_seconds of_seconds QCheck.int64

  let rfc3339_compatible_t_arb =
    let within_rfc3339 =
      QCheck.map
        ~rev:to_seconds
        of_seconds
        (int64_range min_rfc3339_seconds max_rfc3339_seconds)
    in
    QCheck.frequency
      [
        (97, within_rfc3339);
        (1, QCheck.always max_rfc3339);
        (1, QCheck.always min_rfc3339);
        (1, QCheck.always epoch);
      ]

  let pp fmt t = Format.fprintf fmt "%Lx" (to_seconds t)

  let add_diff_roundtrip =
    QCheck.Test.make
      ~name:"Protocol.[add|diff] roundtrip"
      (QCheck.pair t_arb QCheck.int64)
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
    QCheck.Test.make
      ~name:"Protocol.[diff|add] roundtrip"
      (QCheck.pair t_arb t_arb)
      (fun (some_time, other_time) ->
        let delta = diff other_time some_time in
        let actual = add some_time delta in
        qcheck_eq' ~pp ~eq:equal ~expected:other_time ~actual ())

  let encoding_binary_roundtrip =
    QCheck.Test.make
      ~name:"Protocol.encoding roundtrips in binary"
      t_arb
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let actual = Data_encoding.Binary.of_bytes_exn encoding b in
        qcheck_eq' ~pp ~eq:equal ~expected:t ~actual ())

  let encoding_json_roundtrip =
    QCheck.Test.make
      ~name:"Protocol.encoding roundtrips in JSON"
      t_arb
      (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let actual = Data_encoding.Json.destruct encoding j in
        qcheck_eq' ~pp ~eq:equal ~expected:t ~actual ())

  let encoding_to_notation_roundtrip =
    QCheck.Test.make
      ~name:"Protocol.[to|of]_notation roundtrip in RFC3339 range"
      rfc3339_compatible_t_arb
      (fun t ->
        to_notation t |> of_notation |> function
        | None -> QCheck.Test.fail_report "Failed to roundtrip notation"
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

  (** Arbitrary of {!t} from usual time fragments year-month-day hour-minute-second, parsed through {!Ptime.of_date_time}. *)
  let t_ymdhms_arb : t QCheck.arbitrary =
    let open QCheck in
    let rev t =
      Option.get t |> Ptime.to_date_time |> fun (date, (time, _)) -> (date, time)
    in
    of_option_arb
      (pair
         (triple (0 -- 9999) (1 -- 12) (1 -- 31))
         (triple (0 -- 23) (0 -- 59) (0 -- 60))
      |> map ~rev (fun (date, time) -> Ptime.of_date_time (date, (time, 0))))
    |> set_print (Format.asprintf "%a" pp_hum)

  let (min_day, min_ps) = Ptime.min |> Ptime.to_span |> Ptime.Span.to_d_ps

  let (max_day, max_ps) = Ptime.max |> Ptime.to_span |> Ptime.Span.to_d_ps

  (** Arbitrary of {!t} from days + picoseconds, parsed through {!Ptime.Span.of_d_ps}. *)
  let t_dps_arb : t QCheck.arbitrary =
    let open QCheck in
    let rev t = Ptime.to_span t |> Ptime.Span.to_d_ps in
    pair (min_day -- max_day) (int64_range min_ps max_ps)
    |> map ~rev (fun (d, ps) ->
           Ptime.Span.of_d_ps (d, ps)
           |> Option.get |> Ptime.of_span |> Option.get)
    (* But please keep using a nice pretty printer... We can probably write in the future a generic function that mixes features of [map ~rev] and [map_keep_input ~print] to only pass the monotonic transformation and the pretty printer, instead of manually writing [rev]. *)
    |> set_print (Format.asprintf "%a" pp_hum)

  let t_arb = QCheck.choose [t_ymdhms_arb; t_dps_arb]

  (** Check that the span is smaller than 1 second (useful for Protocol time roundtrips as Protocol time precision is the second). *)
  let is_small delta =
    Stdlib.( < )
      (Ptime.Span.compare delta (Ptime.Span.v (0, 1_000_000_000_000L)))
      0

  let to_protocol_of_protocol_roundtrip =
    QCheck.Test.make
      ~name:"System.[to|of]_protocol roundtrip modulo option"
      t_arb
      (fun t ->
        match to_protocol t |> of_protocol_opt with
        | None -> QCheck.Test.fail_report "Failed roundtrip"
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
  let of_protocol_to_protocol_roundtrip_or_outside_rfc3339 =
    QCheck.Test.make
      ~name:"System.[of|to]_protocol roundtrip or outside RFC3339 range"
      (* Use both generators, otherwise statistically, we will almost
          never hit the RFC3339 time range. *)
      (QCheck.choose [Protocol.t_arb; Protocol.rfc3339_compatible_t_arb])
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

  let rfc_encoding_binary_roundtrip =
    QCheck.Test.make
      ~name:"System.rfc_encoding roundtrips in binary modulo precision"
      t_arb
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn rfc_encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn rfc_encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let rfc_encoding_json_roundtrip =
    QCheck.Test.make
      ~name:"System.rfc_encoding roundtrips in JSON modulo precision"
      t_arb
      (fun t ->
        let j = Data_encoding.Json.construct rfc_encoding t in
        let tt = Data_encoding.Json.destruct rfc_encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let encoding_binary_roundtrip =
    QCheck.Test.make
      ~name:"System.encoding roundtrips in binary modulo precision"
      t_arb
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let encoding_json_roundtrip =
    QCheck.Test.make
      ~name:"System.encoding roundtrips in JSON modulo precision"
      t_arb
      (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let tt = Data_encoding.Json.destruct encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        is_small delta)

  let tests =
    [
      to_protocol_of_protocol_roundtrip;
      of_protocol_to_protocol_roundtrip_or_outside_rfc3339;
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
