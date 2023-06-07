(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

module type S = sig
  type scalar

  module Domain : Domain.Domain_sig with type scalar = scalar

  module Polynomial : Polynomial.Polynomial_sig with type scalar = scalar

  module Evaluations :
    Evaluations.Evaluations_sig
      with type scalar = scalar
       and type domain = Domain.t
       and type polynomial = Polynomial.t

  module Srs : sig
    module Srs_g1 : Srs.S with type polynomial = Polynomial.t

    module Srs_g2 : Srs.S with type polynomial = Polynomial.t

    type t = Srs_g1.t * Srs_g2.t

    val generate_insecure : int -> int -> t

    val check : t -> unit
  end

  module G1_carray :
    Ec_carray.EC_carray_sig
      with type elt = Bls12_381.G1.t
       and type domain = Domain.t
       and type evaluations = Evaluations.t

  module G2_carray :
    Ec_carray.EC_carray_sig
      with type elt = Bls12_381.G2.t
       and type domain = Domain.t
       and type evaluations = Evaluations.t
end

include
  S
    with type scalar = Bls12_381.Fr.t
     and type Srs.Srs_g1.elt = Bls12_381.G1.t
     and type Srs.Srs_g2.elt = Bls12_381.G2.t
     and type Evaluations.t = Evaluations.t
