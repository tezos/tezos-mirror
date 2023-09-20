(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Our favorite "ring" *)
module R = Float

module type S = sig
  type t

  type basis

  val is_empty : t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val zero : t

  val add : t -> t -> t

  val smul : R.t -> t -> t

  val neg : t -> t

  val fold : (basis -> R.t -> 'b -> 'b) -> t -> 'b -> 'b

  val iter : (basis -> R.t -> unit) -> t -> unit

  val find_map : (basis -> R.t -> 'res option) -> t -> 'res option

  val set : t -> basis -> R.t -> t

  val get_exn : t -> basis -> R.t

  val get_opt : t -> basis -> R.t option

  (** Returns [zero] if [basis] is not found in [t]. *)
  val get : t -> basis -> R.t

  val swap : t -> basis -> basis -> t

  val of_list : (basis * R.t) list -> t

  val to_list : t -> (basis * R.t) list

  val pp :
    pp_basis:(Format.formatter -> basis -> unit) ->
    pp_element:(Format.formatter -> R.t -> unit) ->
    Format.formatter ->
    t ->
    unit

  module Op : sig
    val ( .%[] ) : t -> basis -> R.t

    val ( .%[]<- ) : t -> basis -> R.t -> t

    val ( + ) : t -> t -> t

    val ( * ) : R.t -> t -> t
  end
end

module Make (M : Map.S) : S with type t = R.t M.t and type basis = M.key

module String : S with type t = R.t String.Map.t and type basis = string
