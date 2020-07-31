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

type fp_tag (* Tag for fixed point computations *)

type integral_tag (* Tag for integral computations *)

module type Safe = sig
  type 'a t

  type fp = fp_tag t

  type integral = integral_tag t

  val integral : Z.t -> integral

  val integral_of_int : int -> integral

  val integral_to_z : integral -> Z.t

  val zero : 'a t

  val add : 'a t -> 'a t -> 'a t

  val sub : 'a t -> 'a t -> 'a t

  val ceil : fp -> integral

  val floor : fp -> integral

  val fp : 'a t -> fp

  val ( = ) : 'a t -> 'b t -> bool

  val ( <> ) : 'a t -> 'b t -> bool

  val ( < ) : 'a t -> 'b t -> bool

  val ( <= ) : 'a t -> 'b t -> bool

  val ( >= ) : 'a t -> 'b t -> bool

  val ( > ) : 'a t -> 'b t -> bool

  val compare : 'a t -> 'b t -> int

  val equal : 'a t -> 'b t -> bool

  val max : 'a t -> 'a t -> 'a t

  val min : 'a t -> 'a t -> 'a t

  val pp : Format.formatter -> 'a t -> unit

  val pp_integral : Format.formatter -> integral -> unit

  val n_fp_encoding : fp Data_encoding.t

  val n_integral_encoding : integral Data_encoding.t

  val z_fp_encoding : fp Data_encoding.t

  val z_integral_encoding : integral Data_encoding.t
end

module type Full = sig
  include Safe

  val unsafe_fp : Z.t -> fp
end

module type Decimals = sig
  val decimals : int
end

module Make (Arg : Decimals) : Full = struct
  let () = assert (Compare.Int.(Arg.decimals >= 0))

  type 'a t = Z.t

  (* FIXME Add [Z.pow] to the environment v1 *)
  let rec z_pow v e =
    if Compare.Int.(e = 0) then Z.one else Z.mul v (z_pow v (e - 1))

  let scaling_factor = z_pow (Z.of_int 10) Arg.decimals

  type fp = fp_tag t

  type integral = integral_tag t

  let integral z = Z.mul z scaling_factor

  let integral_of_int int = integral @@ Z.of_int int

  (* FIXME Add [Z.ediv] to the environment v1 *)
  let integral_to_z x = Z.ediv_rem x scaling_factor |> fst

  let unsafe_fp x = x

  let zero = Z.zero

  let add = Z.add

  let sub = Z.sub

  (* FIXME Add [Z.erem] to the environment v1 *)
  let ceil x =
    let r = Z.ediv_rem x scaling_factor |> snd in
    if Z.equal r Z.zero then x else Z.add x (Z.sub scaling_factor r)

  let floor x =
    let r = Z.ediv_rem x scaling_factor |> snd in
    if Z.equal r Z.zero then x else Z.sub x r

  let fp x = x

  let ( = ) = Compare.Z.( = )

  let ( <> ) = Compare.Z.( <> )

  let ( < ) = Compare.Z.( < )

  let ( <= ) = Compare.Z.( <= )

  let ( >= ) = Compare.Z.( >= )

  let ( > ) = Compare.Z.( > )

  let compare = Z.compare

  let equal = Z.equal

  let max = Compare.Z.max

  let min = Compare.Z.min

  let pp_positive_fp fmtr milligas =
    if Compare.Int.(Arg.decimals <> 3) then
      Format.fprintf fmtr "pp_positive_fp: cannot print (decimals <> 3)"
    else
      let (q, r) = Z.ediv_rem milligas scaling_factor in
      if Z.equal r Z.zero then Format.fprintf fmtr "%s" (Z.to_string q)
      else Format.fprintf fmtr "%s.%03d" (Z.to_string q) (Z.to_int r)

  let pp fmtr fp =
    if Compare.Z.(fp >= Z.zero) then pp_positive_fp fmtr fp
    else Format.fprintf fmtr "-%a" pp_positive_fp (Z.neg fp)

  let pp_integral = pp

  let n_fp_encoding : fp Data_encoding.t = Data_encoding.n

  let z_fp_encoding : fp Data_encoding.t = Data_encoding.z

  let n_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral Data_encoding.n

  let z_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral Data_encoding.z
end
