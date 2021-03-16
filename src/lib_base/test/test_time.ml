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

open Time

module Protocol = struct
  open Protocol

  let t = Crowbar.map [Crowbar.int64] of_seconds

  let pp fmt t = Format.fprintf fmt "%Lx" (to_seconds t)

  let () =
    Crowbar.add_test
    (* Property:
       forall [t]: [Protocol.t], forall [delta]: [int64]:
         [(t + delta) - t = delta] *)
      ~name:"Base.Time.Protocol.add-diff"
      [t; Crowbar.int64]
      (fun some_time delta ->
        let other_time = add some_time delta in
        let same_delta = diff other_time some_time in
        Crowbar.check_eq
          ~pp:(fun fmt i64 -> Format.fprintf fmt "%Lx" i64)
          ~eq:Int64.equal
          delta
          same_delta)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [Protocol.t], forall [tb]: [Protocol.t]:
         [(tb - ta) + ta = tb] *)
      ~name:"Base.Time.Protocol.diff-add"
      [t; t]
      (fun some_time other_time ->
        let delta = diff other_time some_time in
        let same_other_time = add some_time delta in
        Crowbar.check_eq ~pp ~eq:equal other_time same_other_time)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [Protocol.encoding] roundtrips in binary *)
      ~name:"Base.Time.Protocol.encoding-binary"
      [t]
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn encoding b in
        Crowbar.check_eq ~pp ~eq:equal t tt)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [Protocol.encoding] roundtrips in json *)
      ~name:"Base.Time.Protocol.encoding-json"
      [t]
      (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let tt = Data_encoding.Json.destruct encoding j in
        Crowbar.check_eq ~pp ~eq:equal t tt)

  let () =
    Crowbar.add_test
      ~name:"Base.Time.Protocol.to_notation roundtrip"
      [Crowbar.range 1000]
      (fun i ->
        let close_to_epoch = add epoch (Int64.neg @@ Int64.of_int i) in
        let s = to_notation close_to_epoch in
        match of_notation s with
        | None ->
            Crowbar.fail "Failed to roundtrip notation"
        | Some after_roundtrip ->
            Crowbar.check_eq ~pp ~eq:equal close_to_epoch after_roundtrip)
end

module System = struct
  open System

  let t_ymdhms =
    let open Crowbar in
    map
      [ range 10000;
        range ~min:01 12;
        range ~min:01 31;
        range 24;
        range 60;
        range 60 ]
      (fun year month day hour minute second ->
        match
          Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))
        with
        | None ->
            (* when the day of the month overflows for the month *)
            bad_test ()
        | Some p ->
            p)

  let min_day = Ptime.min |> Ptime.to_span |> Ptime.Span.to_d_ps |> fst

  let max_day = Ptime.max |> Ptime.to_span |> Ptime.Span.to_d_ps |> fst

  let day_range = max_day - min_day

  let t_dps =
    let open Crowbar in
    (* to avoid generating lots of out-of-range ps, we assume we're on a 64-bit
        machine and we clip the range to acceptable ps inputs *)
    map
      [range (day_range + 2); range 86_400_000_000_000_000]
      (fun d ps ->
        let d = d + min_day - 1 in
        let ps = Int64.of_int ps in
        match Ptime.Span.of_d_ps (d, ps) with
        | None ->
            assert false
        | Some span -> (
          match Ptime.of_span span with
          | None ->
              bad_test () (* range issue *)
          | Some p ->
              p ))

  let t =
    let open Crowbar in
    choose [t_ymdhms; t_dps]

  let () =
    Crowbar.add_test
    (* Property:
       forall [t]: [System.t],
         [of_protocol_opt (to_protocol t)] is [Some _] *)
      ~name:"Base.Time.System.to-protocol"
      [t]
      (fun t ->
        let protocol_time = to_protocol t in
        match of_protocol_opt protocol_time with
        | None ->
            Crowbar.check false
        | Some _ ->
            Crowbar.check true)

  let () =
    Crowbar.add_test
    (* Property:
       forall [t]: [Protocol.t],
         [to_protocol]/[of_protocol_opt] roundtrip modulo option. *)
      ~name:"Base.Time.System.to-protocol-of-protocol"
      [Protocol.t]
      (fun protocol_time ->
        match of_protocol_opt protocol_time with
        | None ->
            Crowbar.check true
        | Some system_time ->
            let same_protocol_time = to_protocol system_time in
            Crowbar.check_eq
              ~pp:Time.Protocol.pp_hum
              ~eq:Time.Protocol.equal
              protocol_time
              same_protocol_time)

  let is_small delta =
    Stdlib.( < )
      (Ptime.Span.compare delta (Ptime.Span.v (0, 1_000_000_000_000L)))
      0

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [System.rfc_encoding] roundtrips in binary modulo precision *)
      ~name:"Base.Time.Protocol.rfc-encoding-binary"
      [t]
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn rfc_encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn rfc_encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        Crowbar.check @@ is_small delta)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [System.rfc_encoding] roundtrips in json modulo precision *)
      ~name:"Base.Time.Protocol.rfc-encoding-json"
      [t]
      (fun t ->
        let j = Data_encoding.Json.construct rfc_encoding t in
        let tt = Data_encoding.Json.destruct rfc_encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        Crowbar.check @@ is_small delta)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [System.encoding] roundtrips in binary modulo precision *)
      ~name:"Base.Time.Protocol.encoding-binary"
      [t]
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn encoding b in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        Crowbar.check @@ is_small delta)

  let () =
    Crowbar.add_test
    (* Property:
       forall [ta]: [System.encoding] roundtrips in json modulo precision *)
      ~name:"Base.Time.Protocol.encoding-json"
      [t]
      (fun t ->
        let j = Data_encoding.Json.construct encoding t in
        let tt = Data_encoding.Json.destruct encoding j in
        let delta = Ptime.Span.abs @@ Ptime.diff t tt in
        Crowbar.check @@ is_small delta)
end
