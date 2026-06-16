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

(** Block headers contain some fields whose values represent the block
   producer's opinion on some topics. The votes are averaged to get an
   estimation of the will of the stake-weighted majority of bakers on
   these topics. The protocol can then perform automatic actions
   depending on the values of these averages; typically activating or
   deactivating some features.

   This module is about the computation of these averages.

   We use exponential moving averages (EMA for short) because they can
   easily and efficiently be implemented because a single value needs to
   be stored in the context for each average. Each EMA is updated once per
   block and stored in the context. It is represented using a 32-bit
   signed integer but it can only take non-negative values in a range
   of the form 0...ema_max where the constant ema_max, the maximum
   value of the EMA, is a parameter of this module. To update an EMA,
   we multiply the EMA computed in the previous block by a constant
   factor slightly less than 1 called the attenuation factor, and then
   we either add or remove (depending on the vote that was casted in
   the block header) another constant called the baker's contribution
   to the EMA. The baker contribution is also a parameter of this
   module. When multiplying by the attenuation factor, we round toward
   the middle of the 0...ema_max range. The update formula is thus:

   new_ema = ((old_ema - ema_max/2) * attenuation_factor) +- baker_contribution + ema_max/2

*)

module type EMA_PARAMETERS = sig
  val baker_contribution : Z.t

  val ema_max : Int32.t

  (* We don't need to parameterize by the attenuation factor because
     it can be computed from the two other parameters with the
     following formula:

     attenuation_factor = (ema_max - 2 baker_contribution) / ema_max
  *)
end

module type T = sig
  type t

  val of_int32 : Int32.t -> t tzresult Lwt.t

  val zero : t

  val to_int32 : t -> Int32.t

  val encoding : t Data_encoding.t

  val ( < ) : t -> Int32.t -> bool

  val update_ema_up : t -> t

  val update_ema_down : t -> t
end

module Make (EMA_parameters : EMA_PARAMETERS) : T = struct
  type t = Int32.t
  (* Invariant 0l <= ema <= EMA_Parameters.ema_max *)

  (* This error is not registered because we don't expect it to be
     raised. *)
  type error += Toggle_ema_out_of_bound of Int32.t

  let check_bounds x = Compare.Int32.(0l <= x && x <= EMA_parameters.ema_max)

  let of_int32 (x : Int32.t) : t tzresult Lwt.t =
    if check_bounds x then return x else tzfail @@ Toggle_ema_out_of_bound x

  let zero : t = Int32.zero

  (* The conv_with_guard combinator of Data_encoding expects a (_, string) result. *)
  let of_int32_for_encoding x =
    if check_bounds x then Ok x else Error "out of bounds"

  let to_int32 (ema : t) : Int32.t = ema

  (* We perform the computations in Z to avoid overflows. *)

  let ema_max_z = Z.of_int32 EMA_parameters.ema_max

  let attenuation_numerator =
    Z.(sub ema_max_z (mul (of_int 2) EMA_parameters.baker_contribution))

  let attenuation_denominator = ema_max_z

  let attenuate z =
    Z.(div (mul attenuation_numerator z) attenuation_denominator)

  let half_ema_max_z = Z.(div ema_max_z (of_int 2))

  (* Outside of this module, the EMA is always between 0l and ema_max.
     This [recenter] wrappers, puts it in between -ema_max/2 and
     ema_max/2.  The goal of this recentering around zero is to make
     [update_ema_off] and [update_ema_on] behave symmetrically with
     respect to rounding. *)
  let recenter f ema = Z.(add half_ema_max_z (f (sub ema half_ema_max_z)))

  let update_ema_up (ema : t) : t =
    let ema = Z.of_int32 ema in
    recenter
      (fun ema -> Z.add (attenuate ema) EMA_parameters.baker_contribution)
      ema
    |> Z.to_int32

  let update_ema_down (ema : t) : t =
    let ema = Z.of_int32 ema in
    recenter
      (fun ema -> Z.sub (attenuate ema) EMA_parameters.baker_contribution)
      ema
    |> Z.to_int32

  let ( < ) : t -> Int32.t -> bool = Compare.Int32.( < )

  let encoding : t Data_encoding.t =
    Data_encoding.(conv_with_guard to_int32 of_int32_for_encoding int32)
end
