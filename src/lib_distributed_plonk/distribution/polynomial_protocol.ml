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

open Plonk.Bls

module type S = sig
  module PC : Kzg.PC_for_distribution_sig

  include Plonk.Polynomial_protocol.S with module PC := PC

  (** [compute_t ~n ~alpha evaluations] returns a polynomial T splitted in chunks,
     where [T(X) = (sum_i alpha^i evaluations[i]) / (X^n - 1)] and the returned
     chunks [{ 'T_0' -> T0; 'T_1' -> T1; 'T_2' -> T2 }] are such that
     [T = T0 + X^n T1 + X^{2n} T2]. *)
  val compute_t :
    n:int ->
    alpha:Scalar.t ->
    nb_of_t_chunks:int ->
    Evaluations.t Plonk.SMap.t ->
    Evaluations.polynomial Plonk.SMap.t
end

module type Super = sig
  module PC : Kzg_pack.Super_PC_sig

  include Aggregation.Polynomial_protocol.S with module PC := PC

  (** [compute_t ~n ~alpha evaluations] returns a polynomial T splitted in chunks,
     where [T(X) = (sum_i alpha^i evaluations[i]) / (X^n - 1)] and the returned
     chunks [{ 'T_0' -> T0; 'T_1' -> T1; 'T_2' -> T2 }] are such that
     [T = T0 + X^n T1 + X^{2n} T2]. *)
  val compute_t :
    n:int ->
    alpha:Scalar.t ->
    nb_of_t_chunks:int ->
    Evaluations.t Plonk.SMap.t ->
    Evaluations.polynomial Plonk.SMap.t
end

module Make (PC : Kzg.PC_for_distribution_sig) : S with module PC = PC = struct
  module PP = Plonk.Polynomial_protocol.Make_impl (PC)
  module PC = PC

  let compute_t = PP.compute_t

  include (PP : Plonk.Polynomial_protocol.S with module PC := PC)
end

module MakeSuper
    (PC : Kzg_pack.Super_PC_sig)
    (Answers_commitment : Plonk.Input_commitment.S) :
  Super with module PC = PC with module Answers_commitment = Answers_commitment =
struct
  module PP =
    Aggregation.Polynomial_protocol.Make_impl (PC) (Answers_commitment)
  module PC = PC
  module Answers_commitment = Answers_commitment

  include (
    PP :
      module type of PP
        with module PC := PC
        with module Answers_commitment := Answers_commitment)
end
