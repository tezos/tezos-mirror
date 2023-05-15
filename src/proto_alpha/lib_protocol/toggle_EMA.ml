(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Exponential moving average of toggle votes. Represented as an int32 between
    0 and 2,000,000,000. It is an exponential moving average of the "off" votes
    over a window of the most recent 2000 blocks that did not vote "pass". *)

(* The exponential moving average is represented as an Int32 between 0l and 2_000_000_000l *)
type t = Int32.t (* Invariant 0 <= ema <= 2_000_000_000l *)

(* This error is not registered because we don't expect it to be
   raised. *)
type error += Toggle_ema_out_of_bound of Int32.t

let check_bounds x = Compare.Int32.(0l <= x && x <= 2_000_000_000l)

let of_int32 (x : Int32.t) : t tzresult Lwt.t =
  if check_bounds x then return x else tzfail @@ Toggle_ema_out_of_bound x

let zero : t = Int32.zero

(* The conv_with_guard combinator of Data_encoding expects a (_, string) result. *)
let of_int32_for_encoding x =
  if check_bounds x then Ok x else Error "out of bounds"

let to_int32 (ema : t) : Int32.t = ema

(* We perform the computations in Z to avoid overflows. *)

let z_1999 : Z.t = Z.of_int 1999

let z_2000 : Z.t = Z.of_int 2000

let attenuate z = Z.(div (mul z_1999 z) z_2000)

let z_1_000_000_000 = Z.of_int 1_000_000_000

(* Outside of this module, the EMA is always between 0 and 2,000,000,000.
   This [recenter] wrappers, puts it in between -1,000,000,000 and 1,000,000,000.
   The goal of this recentering around zero is to make [update_ema_off] and
   [update_ema_on] behave symmetrically with respect to rounding. *)
let recenter f ema = Z.(add z_1_000_000_000 (f (sub ema z_1_000_000_000)))

let z_500_000 = Z.of_int 500_000

let update_ema_off (ema : t) : t =
  let ema = Z.of_int32 ema in
  recenter (fun ema -> Z.add (attenuate ema) z_500_000) ema |> Z.to_int32

let update_ema_on (ema : t) : t =
  let ema = Z.of_int32 ema in
  recenter (fun ema -> Z.sub (attenuate ema) z_500_000) ema |> Z.to_int32

let ( < ) : t -> Int32.t -> bool = Compare.Int32.( < )

let encoding : t Data_encoding.t =
  Data_encoding.(conv_with_guard to_int32 of_int32_for_encoding int32)
