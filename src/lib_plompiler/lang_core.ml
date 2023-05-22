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

module S = Csir.Scalar

module type NUM = sig
  type scalar

  type 'a repr

  type 'a t

  val custom :
    ?qc:S.t ->
    ?ql:S.t ->
    ?qr:S.t ->
    ?qo:S.t ->
    ?qm:S.t ->
    ?qx2b:S.t ->
    ?qx5a:S.t ->
    scalar repr ->
    scalar repr ->
    scalar repr t

  val assert_custom :
    ?qc:S.t ->
    ?ql:S.t ->
    ?qr:S.t ->
    ?qo:S.t ->
    ?qm:S.t ->
    scalar repr ->
    scalar repr ->
    scalar repr ->
    unit repr t

  val add :
    ?qc:S.t -> ?ql:S.t -> ?qr:S.t -> scalar repr -> scalar repr -> scalar repr t

  val add_constant : ?ql:S.t -> S.t -> scalar repr -> scalar repr t

  val mul : ?qm:S.t -> scalar repr -> scalar repr -> scalar repr t

  val div : ?den_coeff:S.t -> scalar repr -> scalar repr -> scalar repr t

  val pow5 : scalar repr -> scalar repr t

  val is_zero : scalar repr -> bool repr t

  val is_not_zero : scalar repr -> bool repr t

  val assert_nonzero : scalar repr -> unit repr t

  val assert_bool : scalar repr -> unit repr t
end

module type BOOL = sig
  type scalar

  type 'a repr

  type 'a t

  val band : bool repr -> bool repr -> bool repr t

  val xor : bool repr -> bool repr -> bool repr t

  val bor : bool repr -> bool repr -> bool repr t

  val bor_lookup : bool repr -> bool repr -> bool repr t

  val bnot : bool repr -> bool repr t

  val ifthenelse : bool repr -> 'a repr -> 'a repr -> 'a repr t

  val swap : bool repr -> 'a repr -> 'a repr -> ('a * 'a) repr t

  val assert_true : bool repr -> unit repr t

  val assert_false : bool repr -> unit repr t

  val constant_bool : bool -> bool repr t

  val band_list : bool repr list -> bool repr t
end

module type COMMON = sig
  type scalar

  type input_kind = [`InputCom | `Public | `Private]

  type trace_kind = [input_kind | `NoInput]

  type 'a repr

  type 'a t

  val ret : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >* ) : unit repr t -> 'a t -> 'a t

  (* Add a boolean check *)
  val with_bool_check : bool repr t -> unit repr t

  (* Retrieve the boolean representing the conjunction of all previous implicit
     checks.
     WARNING: This will "reset" the implicit check accumulator.
  *)
  val get_checks_wire : bool repr t

  module Input : sig
    type 'a input

    val scalar : S.t -> scalar input

    val to_scalar : scalar input -> S.t

    val bool : bool -> bool input

    val to_bool : bool input -> bool

    val unit : unit input

    val pair : 'a input -> 'b input -> ('a * 'b) input

    val to_pair : ('a * 'b) input -> 'a input * 'b input

    val list : 'a input list -> 'a list input

    val to_list : 'a list input -> 'a input list

    val with_implicit_bool_check :
      ('a repr -> bool repr t) -> 'a input -> 'a input

    val with_assertion : ('a repr -> unit repr t) -> 'a input -> 'a input

    type 'a t = 'a input
  end

  type 'b open_input_com

  val serialize : 'a Input.t -> S.t array

  val input : ?kind:input_kind -> 'a Input.t -> 'a repr t

  val begin_input_com : 'b -> 'b open_input_com

  val ( |: ) : ('c repr -> 'd) open_input_com -> 'c Input.t -> 'd open_input_com

  val end_input_com : 'a open_input_com -> 'a t

  val eq : 'a repr -> 'a repr -> bool

  val foldM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val pair : 'a repr -> 'b repr -> ('a * 'b) repr

  val of_pair : ('a * 'b) repr -> 'a repr * 'b repr

  val to_list : 'a repr list -> 'a list repr

  val of_list : 'a list repr -> 'a repr list

  val unit : unit repr

  val scalar_of_bool : bool repr -> scalar repr

  val unsafe_bool_of_scalar : scalar repr -> bool repr

  val assert_equal : 'a repr -> 'a repr -> unit repr t

  val equal : 'a repr -> 'a repr -> bool repr t

  val constant_scalar : S.t -> scalar repr t

  (** Returns a list of Boolean variables representing the little endian
    bit decomposition of the given scalar (with the least significant bit
    on the head). *)
  val bits_of_scalar :
    ?shift:Z.t -> nb_bits:int -> scalar repr -> bool list repr t

  val with_label : label:string -> 'a t -> 'a t

  module Num :
    NUM
      with type scalar = scalar
       and type 'a repr = 'a repr
       and type 'a t = 'a t

  module Bool :
    BOOL
      with type scalar = scalar
       and type 'a repr = 'a repr
       and type 'a t = 'a t

  module Ecc : sig
    val weierstrass_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      (scalar * scalar) repr t

    val edwards_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      (scalar * scalar) repr t

    val edwards_cond_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      bool repr ->
      (scalar * scalar) repr t
  end

  module Poseidon : sig
    val poseidon128_full_round :
      matrix:S.t array array ->
      k:S.t array ->
      scalar repr * scalar repr * scalar repr ->
      scalar list repr t

    val poseidon128_four_partial_rounds :
      matrix:S.t array array ->
      ks:S.t array array ->
      scalar repr * scalar repr * scalar repr ->
      scalar list repr t
  end

  module Anemoi : sig
    val anemoi_round :
      kx:S.t -> ky:S.t -> scalar repr * scalar repr -> (scalar * scalar) repr t

    val anemoi_double_round :
      kx1:S.t ->
      ky1:S.t ->
      kx2:S.t ->
      ky2:S.t ->
      scalar repr * scalar repr ->
      (scalar * scalar) repr t

    val anemoi_custom :
      kx1:S.t ->
      ky1:S.t ->
      kx2:S.t ->
      ky2:S.t ->
      scalar repr * scalar repr ->
      (scalar * scalar) repr t
  end

  val hd : 'a list repr -> 'a repr t
end
