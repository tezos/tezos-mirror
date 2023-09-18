(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
include Time

type time = Time.t

type error += Timestamp_add (* `Permanent *)

type error += Timestamp_sub (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"timestamp_add"
    ~title:"Timestamp add"
    ~description:"Overflow when adding timestamps."
    ~pp:(fun ppf () -> Format.fprintf ppf "Overflow when adding timestamps.")
    Data_encoding.empty
    (function Timestamp_add -> Some () | _ -> None)
    (fun () -> Timestamp_add) ;
  register_error_kind
    `Permanent
    ~id:"timestamp_sub"
    ~title:"Timestamp sub"
    ~description:"Subtracting timestamps resulted in negative period."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Subtracting timestamps resulted in negative period.")
    Data_encoding.empty
    (function Timestamp_sub -> Some () | _ -> None)
    (fun () -> Timestamp_sub)

let of_seconds_string s = Option.map Time.of_seconds (Int64.of_string_opt s)

let to_seconds_string s = Int64.to_string (to_seconds s)

let pp = pp_hum

let ( +? ) x y =
  let open Result_syntax in
  let span = Period_repr.to_seconds y in
  let t64 = Time.add x span in
  (* As long as span and time representations are int64, we cannont overflow if
     x is negative. *)
  if x < Time.of_seconds 0L then return t64
  else if t64 < Time.of_seconds 0L then tzfail Timestamp_add
  else return t64

let ( -? ) x y =
  record_trace Timestamp_sub (Period_repr.of_seconds (Time.diff x y))

let ( - ) x y =
  Time.of_seconds Int64.(sub (Time.to_seconds x) (Period_repr.to_seconds y))
