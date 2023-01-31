(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type S = sig
  type t

  (**  [consume consumption fuel] consumes the [consumption] amount from the
       original [fuel].  It returns [None] when the [consumption] is greater
       than the original [fuel] or [Some remaining_fuel].  *)
  val consume : t -> t -> t option

  (** The amount of fuel required to run one PVM tick.
    {[
      one_tick_consumption = of_ticks 1L
    ]}
   *)
  val one_tick_consumption : t

  (** [of_ticks ticks] gives the amount of fuel required to execute the amount
      of [ticks]. *)
  val of_ticks : int64 -> t

  val is_empty : t -> bool

  (** The maximum number of ticks that can be executed with the given amount of
      fuel.
      {[
        max_ticks ∘ of_ticks = Fun.id
        of_ticks ∘ max_ticks = Fun.id
      ]}
  *)
  val max_ticks : t -> int64
end

(** Free fuel where consumption has no effect. *)
module Free : S = struct
  type t = Free

  let one_tick_consumption = Free

  let of_ticks _ = Free

  let consume _ tank = Some tank

  let is_empty _ = false

  let max_ticks _ = Int64.max_int
end

(** Accounted fuel where each tick consumes one unit of fuel. *)
module Accounted : S = struct
  type t = int64

  let of_ticks i =
    assert (Int64.compare i 0L >= 0) ;
    i

  let one_tick_consumption = 1L

  let consume consumption fuel =
    if Int64.compare fuel consumption >= 0 then
      Some (Int64.sub fuel consumption)
    else None

  let is_empty fuel = Int64.compare fuel 0L <= 0

  let max_ticks fuel_left = Int64.max 0L fuel_left
end
