(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let max_daysL =
  (* [= 2932896L] which is less than [Stdlib.max_int] even on 32-bit
     architecture. This ensures [Int64.to_int] is accurate no matter what. *)
  let max_days, _ = Ptime.(Span.to_d_ps (to_span max)) in
  Int64.of_int max_days

let min_daysL =
  (* Same as [max_daysL] but min. *)
  let min_days, _ = Ptime.(Span.to_d_ps (to_span min)) in
  Int64.of_int min_days

module Protocol = struct
  type t = int64

  include (Compare.Int64 : Compare.S with type t := int64)

  let epoch = 0L

  let diff = Int64.sub

  let add = Int64.add

  let of_ptime t =
    let days, ps = Ptime.Span.to_d_ps (Ptime.to_span t) in
    let s_days = Int64.mul (Int64.of_int days) 86_400L in
    Int64.add s_days (Int64.div ps 1_000_000_000_000L)

  let to_ptime t =
    let daysL = Int64.div t 86_400L in
    let ps = Int64.mul (Int64.rem t 86_400L) 1_000_000_000_000L in
    let daysL, ps =
      if ps < 0L then
        (* [Ptime.Span.of_d_ps] only accepts picoseconds in the range 0L-86_399_999_999_999_999L. Subtract a day and add a day's worth of picoseconds if need be. *)
        (Int64.pred daysL, Int64.(add ps (mul 86_400L 1_000_000_000_000L)))
      else (daysL, ps)
    in
    if Compare.Int64.(daysL > max_daysL || daysL < min_daysL) then
      invalid_arg "Time.Protocol.to_ptime" (* already out of range *)
    else
      let days = Int64.to_int daysL in
      match Option.bind (Ptime.Span.of_d_ps (days, ps)) Ptime.of_span with
      | None -> invalid_arg "Time.Protocol.to_ptime"
      | Some ptime -> ptime

  let of_notation s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Some (of_ptime t)
    | Error _ -> None

  let of_notation_exn s =
    match Ptime.(rfc3339_error_to_msg (of_rfc3339 s)) with
    | Error (`Msg msg) -> invalid_arg ("Time.Protocol.of_notation: " ^ msg)
    | Ok (t, _, _) -> of_ptime t

  let to_notation t = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 (to_ptime t)

  let of_seconds x = x

  let to_seconds x = x

  let rfc_encoding =
    let open Data_encoding in
    def
      "timestamp.rfc"
      ~title:"RFC 3339 formatted timestamp"
      ~description:"A date in RFC 3339 notation."
    @@ conv
         to_notation
         (fun s ->
           match of_notation s with
           | Some s -> s
           | None ->
               Data_encoding.Json.cannot_destruct "Time.Protocol.of_notation")
         string

  let max_rfc3339 = of_ptime Ptime.max

  let min_rfc3339 = of_ptime Ptime.min

  let as_string_encoding =
    let open Data_encoding in
    conv
      (fun i ->
        if min_rfc3339 <= i && i <= max_rfc3339 then to_notation i
        else Int64.to_string i)
      ( Json.wrap_error
      (* NOTE: this encoding is only used as a building block for a json
          encoding so we can raise the json exception directly. *)
      @@
      fun s ->
        match of_notation s with
        | Some i -> i
        | None -> (
            match Int64.of_string_opt s with
            | Some i -> i
            | None -> raise (Invalid_argument "Time.Protocol.decoding")) )
      string

  let encoding =
    let open Data_encoding in
    def
      "timestamp.protocol"
      ~description:
        "A timestamp as seen by the protocol: second-level precision, epoch \
         based."
    @@ splitted ~binary:int64 ~json:as_string_encoding

  let rpc_arg =
    Tezos_rpc.Arg.make
      ~name:"date"
      ~descr:"A date in seconds from epoch"
      ~destruct:(function
        | "none" | "epoch" -> Ok epoch
        | s -> (
            match Int64.of_string_opt s with
            | Some t -> Ok t
            | None ->
                Error (Format.asprintf "failed to parse time (epoch): %S" s)))
      ~construct:Int64.to_string
      ()

  let pp ppf t = Format.fprintf ppf "%Ld" t

  let pp_hum ppf t = Ptime.pp_rfc3339 () ppf (to_ptime t)
end

module System = struct
  let frac_s = 3 (* sub-second fractional precision for pretty-printing *)

  type t = Ptime.t

  let now () = Ptime_clock.now ()

  include Compare.Make (Ptime)

  let epoch = Ptime.epoch

  module Span = struct
    type t = Ptime.Span.t

    let multiply_exn f s =
      let open Ptime.Span in
      WithExceptions.Option.to_exn
        ~none:(Failure "Time.System.Span.multiply_exn")
        (of_float_s (f *. Ptime.Span.to_float_s s))

    let of_seconds_exn f =
      match Ptime.Span.of_float_s f with
      | None -> invalid_arg "Time.System.Span.of_seconds_exn"
      | Some s -> s

    let pp_hum = Ptime.Span.pp

    let encoding =
      let open Data_encoding in
      def
        "timespan.system"
        ~description:"A span of time, as seen by the local computer."
      @@ conv
           Ptime.Span.to_float_s
           (fun f ->
             match Ptime.Span.of_float_s f with
             | None -> invalid_arg "Time.System.Span.encoding"
             | Some s -> s)
           float

    let rpc_arg =
      Tezos_rpc.Arg.make
        ~name:"timespan"
        ~descr:"A span of time in seconds"
        ~destruct:(fun s ->
          match float_of_string s with
          | exception Failure _ ->
              Error (Format.asprintf "failed to parse timespan: %S" s)
          | f -> (
              match Ptime.Span.of_float_s f with
              | Some t -> Ok t
              | None -> Error (Format.asprintf "failed to parse timespan: %S" s)
              ))
        ~construct:(fun s -> string_of_float (Ptime.Span.to_float_s s))
        ()
  end

  let of_seconds_opt seconds =
    let x = Int64.abs seconds in
    let daysL = Int64.div x 86_400L in
    if Compare.Int64.(daysL > max_daysL || daysL < min_daysL) then None
      (* already out of range *)
    else
      let days = Int64.to_int daysL in
      let ps = Int64.mul (Int64.rem x 86_400L) 1_000_000_000_000L in
      match Ptime.Span.of_d_ps (days, ps) with
      | None -> None
      | Some span ->
          let span =
            if Compare.Int64.(seconds < 0L) then Ptime.Span.neg span else span
          in
          Ptime.of_span span

  let of_seconds_exn x =
    match of_seconds_opt x with
    | Some t -> t
    | None -> invalid_arg "Time.of_seconds"

  let to_seconds x =
    let days, ps = Ptime.(Span.to_d_ps (to_span x)) in
    let s_days = Int64.mul (Int64.of_int days) 86_400L in
    Int64.add s_days (Int64.div ps 1_000_000_000_000L)

  let of_protocol_exn = of_seconds_exn

  let of_protocol_opt = of_seconds_opt

  let to_protocol = to_seconds

  let of_notation_opt s =
    match Ptime.of_rfc3339 s with Ok (t, _, _) -> Some t | Error _ -> None

  let of_notation_exn s =
    match Ptime.(rfc3339_error_to_msg (of_rfc3339 s)) with
    | Ok (t, _, _) -> t
    | Error (`Msg msg) -> invalid_arg ("Time.of_notation: " ^ msg)

  let to_notation t = Ptime.to_rfc3339 ~frac_s t

  let rfc_encoding =
    let open Data_encoding in
    def
      "timestamp.rfc"
      ~title:"RFC 3339 formatted timestamp"
      ~description:"A date in RFC 3339 notation."
    @@ conv
         to_notation
         (fun s ->
           match of_notation_opt s with
           | Some s -> s
           | None -> Data_encoding.Json.cannot_destruct "Time.of_notation")
         string

  let encoding =
    let open Data_encoding in
    let binary = conv to_seconds of_seconds_exn int64 in
    let json =
      union
        [
          case
            Json_only
            ~title:"RFC encoding"
            rfc_encoding
            (fun i -> Some i)
            (fun i -> i);
          case
            Json_only
            ~title:"Second since epoch"
            int64
            (fun _ -> None)
            (fun i -> of_seconds_exn i);
        ]
    in
    def
      "timestamp.system"
      ~description:
        "A timestamp as seen by the underlying, local computer: \
         subsecond-level precision, epoch or rfc3339 based."
    @@ splitted ~binary ~json

  let rpc_arg =
    Tezos_rpc.Arg.make
      ~name:"date"
      ~descr:"A date in seconds from epoch"
      ~destruct:(function
        | "none" | "epoch" -> Ok Ptime.epoch
        | s -> (
            match of_notation_opt s with
            | Some t -> Ok t
            | None -> (
                match of_seconds_exn (Int64.of_string s) with
                | t -> Ok t
                | (exception Failure _) | (exception Invalid_argument _) ->
                    Error (Format.asprintf "failed to parse time (epoch): %S" s)
                )))
      ~construct:to_notation
      ()

  let pp_hum ppf t = Ptime.pp_rfc3339 ~frac_s () ppf t

  type 'a stamped = {data : 'a; stamp : Ptime.t}

  let stamped_encoding arg_encoding =
    let open Data_encoding in
    conv
      (fun {stamp; data} -> (stamp, data))
      (fun (stamp, data) -> {stamp; data})
      (tup2 encoding arg_encoding)

  let pp_stamped pp fmt {data; stamp} =
    Format.fprintf fmt "%a(%a)" pp data pp_hum stamp

  let stamp ~time data = {data; stamp = time}

  let recent a1 a2 =
    match (a1, a2) with
    | None, None -> None
    | None, (Some _ as a) | (Some _ as a), None -> a
    | Some (_, t1), Some (_, t2) -> if t1 < t2 then a2 else a1

  let hash t = Int64.to_int (to_seconds t)

  module Set = Set.Make (Ptime)
  module Map = Map.Make (Ptime)

  module Table = Hashtbl.Make (struct
    include Ptime

    let hash = hash
  end)
end

module Monotonic = struct
  module Span = struct
    type t = Mtime.Span.t

    let to_ms x =
      Int64.to_int
        Mtime.Span.(Int64.unsigned_div (to_uint64_ns x) (to_uint64_ns ms))

    let to_float_us x = Mtime.Span.(to_float_ns x /. to_float_ns us)

    let to_float_s x = Mtime.Span.(to_float_ns x /. to_float_ns s)
  end
end

let () =
  Data_encoding.Registration.register ~pp:Protocol.pp_hum Protocol.encoding ;
  Data_encoding.Registration.register ~pp:System.pp_hum System.encoding ;
  Data_encoding.Registration.register System.Span.encoding
