(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Escape_EMA : sig
  (* The exponential moving average is represented as an Int32 between 0l and 2_000_000_000l *)

  type t

  val zero : t

  val of_int32 : Int32.t -> t tzresult Lwt.t

  val to_int32 : t -> Int32.t

  val update_ema_off : t -> t

  val update_ema_on : t -> t

  val ( < ) : t -> Int32.t -> bool

  val encoding : t Data_encoding.t
end = struct
  type t = Int32.t (* Invariant 0 <= ema <= 2_000_000l *)

  (* This error is not registered because we don't expect it to be
     raised. *)
  type error += Liquidity_baking_escape_ema_out_of_bound of Int32.t

  let check_bounds x = Compare.Int32.(0l <= x && x <= 2_000_000_000l)

  let of_int32 x =
    if check_bounds x then return x
    else fail @@ Liquidity_baking_escape_ema_out_of_bound x

  let zero = Int32.zero

  let of_int32_for_encoding x =
    if check_bounds x then Ok x else Error "out of bounds"

  let to_int32 ema = ema

  (* We perform the computations in Z to avoid overflows. *)

  let z_1999 = Z.of_int 1999

  let z_2000 = Z.of_int 2000

  let attenuate z = Z.(div (mul z_1999 z) z_2000)

  let z_1_000_000_000 = Z.of_int 1_000_000_000

  let recenter f ema = Z.(add z_1_000_000_000 (f (sub ema z_1_000_000_000)))

  let z_500_000 = Z.of_int 500_000

  let update_ema_off ema =
    let ema = Z.of_int32 ema in
    recenter (fun ema -> Z.add (attenuate ema) z_500_000) ema |> Z.to_int32

  let update_ema_on ema =
    let ema = Z.of_int32 ema in
    recenter (fun ema -> Z.sub (attenuate ema) z_500_000) ema |> Z.to_int32

  let ( < ) = Compare.Int32.( < )

  let encoding =
    Data_encoding.(conv_with_guard to_int32 of_int32_for_encoding int32)
end

let get_cpmm_address = Storage.Liquidity_baking.Cpmm_address.get

let get_escape_ema ctxt =
  Storage.Liquidity_baking.Escape_ema.get ctxt >>=? fun ema ->
  Escape_EMA.of_int32 ema

let on_cpmm_exists ctxt f =
  get_cpmm_address ctxt >>=? fun cpmm_contract ->
  Contract_storage.exists ctxt cpmm_contract >>=? function
  | false ->
      (* do nothing if the cpmm is not found *)
      return (ctxt, [])
  | true -> f ctxt cpmm_contract

let check_below_sunset ctxt =
  let sunset_level = Constants_storage.liquidity_baking_sunset_level ctxt in
  let level = Raw_level_repr.to_int32 (Level_storage.current ctxt).level in
  Compare.Int32.(level < sunset_level)

(* Invariant: 0 <= ema <= 2_000_000 *)
let compute_new_ema ~escape_vote ema =
  match escape_vote with
  | Block_header_repr.LB_pass -> ema
  | LB_off -> Escape_EMA.update_ema_off ema
  | LB_on -> Escape_EMA.update_ema_on ema

(* ema starts at zero and is always between 0 and 2_000_000. It is an
   exponentional moving average representing the fraction of recent blocks
   having the liquidity_baking_escape_vote flag set to "off". The computation
   of this EMA ignores all blocks with the flag set to "pass".

   More precisely,
   if escape_vote[n] is pass, then ema[n+1] = ema[n].
   if escape_vote[n] is off, then ema[n+1] = (1999 * ema[n] // 2000) + 1000
   if escape_vote[n] is on, then ema[n+1] = (1999 * ema[n] // 2000)
   where escape_vote is protocol_data.contents.liquidity_baking_escape_vote *)
let update_escape_ema ctxt ~escape_vote =
  get_escape_ema ctxt >>=? fun old_ema ->
  let new_ema = compute_new_ema ~escape_vote old_ema in
  Storage.Liquidity_baking.Escape_ema.update ctxt (Escape_EMA.to_int32 new_ema)
  >|=? fun ctxt -> (ctxt, new_ema)

let check_ema_below_threshold ctxt ema =
  Escape_EMA.(
    ema < Constants_storage.liquidity_baking_escape_ema_threshold ctxt)

let on_subsidy_allowed ctxt ~escape_vote f =
  update_escape_ema ctxt ~escape_vote >>=? fun (ctxt, escape_ema) ->
  if check_ema_below_threshold ctxt escape_ema && check_below_sunset ctxt then
    on_cpmm_exists ctxt f >|=? fun (ctxt, operation_results) ->
    (ctxt, operation_results, escape_ema)
  else return (ctxt, [], escape_ema)
