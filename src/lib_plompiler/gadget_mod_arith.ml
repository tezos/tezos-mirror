(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

open Lang_core
open Lang_stdlib

(* This gadget implements support for non-native modular arithmetic in PlonK.
   We follow the techniques by Lubarov and Baylina, see:
   https://eprint.iacr.org/2022/1470 .

   To implement arithmetic over Z_m, where m is the non-native modulus, we first
   need to be able to represent integers in this domain. (Note that m could be
   greater than the native modulus.)
   We represent an integer over Z_m as a vector of limbs encoding its
   representation in a certain base. Each limb contains a native scalar which
   must be bounded in the range [0, base). The expected number of limbs is the
   first integer k such that base^k >= m.
   A vector of k limbs [a_{k-1}, ..., a_1, a_0] is well-formed iff for every i
   a_i is in [0, base). The vector encodes integer (\sum_i a_i base^i).

   For the sake of simplicity, we focus on modular addition over Z_m.
   Modular multiplication can be implemented analogously, with different bounds.

   Given two vectors of limbs x and y, we witness their modular addition z and
   add PlonK constraints that enforce the following equality over the integers:

     \sum_i (x_i + y_i - z_i) base^i = qm * m

   were qm is an integer.
   The above equation must hold over the integers, but we can only assert
   equalities modulo the native modulus, p. The technique described in 2022/1470
   consists of checking the above equality modulo p, but also modulo a set of
   auxiliary "moduli" (lower than p, we will explain in a moment how to do this).
   By the Chinese Remainder Theorem, the equality must also hold modulo the
   lcm(p :: moduli) and here comes the trick:

   If there are enough moduli we will have:

     | \sum_i (x_i + y_i - z_i) base^i - qm * m | < lcm(p :: moduli)

   which means that if the above equation holds modulo lcm(p :: moduli), then it
   must also hold over the integers!

   In order to enforce that the equation holds modulo a small modulus
   mj in moduli, we apply the exact same technique and also compute bounds to
   enforce that the checked equation does not wrap-around modulo p (thus
   implying an equality over the integers).

   An important difference between our implementation and the above cited work
   (2022/1470) is that we compute much tighter bounds on the possible values of
   e.g. | \sum_i (x_i + y_i - z_i) base^i - qm * m |, which allows us to use
   much bigger auxiliary moduli (e.g. the base is a very good candidate
   auxiliary modulus), an thus many fewer auxiliary moduli.
*)

module type PARAMETERS = sig
  (* label to refer to this set of parameters *)
  val label : string

  (* modulus over which this arithmetic is defined *)
  val modulus : Z.t

  (* base for the limbs-representation of integers over Z_modulus *)
  val base : Z.t

  (* set of auxiliary moduli used to enforce modular addition constraints *)
  val moduli_add : Z.t list

  (* set of auxiliary moduli used to enforce modular multiplication constrs. *)
  val moduli_mul : Z.t list
end

module type MOD_ARITH = functor (L : LIB) -> sig
  open L

  (* type of modular integers *)
  type mod_int

  (* label to refer to this set of parameters *)
  val label : string

  (* modulus over which this arithmetic is defined *)
  val modulus : Z.t

  (* base for the limbs-representation of integers over Z_modulus *)
  val base : Z.t

  (* smallest integer such that base^nb_limbs >= modulus *)
  val nb_limbs : int

  (* set of auxiliary moduli used to enforce modular addition constraints *)
  val moduli_add : Z.t list

  (* set of auxiliary moduli used to enforce modular multiplication constrs. *)
  val moduli_mul : Z.t list

  (* bounds used for modular addition constraints, we refer to function
     [check_addition_parameters] below for details about how they are defined
     and calculated.
     Each integer pair (shift, bound) describes the limits of a corresponding
     variable v. In particular (v + shift) will be asserted to be in the
     interval [0, bound). *)
  val bounds_add : (Z.t * Z.t) * (Z.t * Z.t) list

  (* bounds used for modular multiplication constraints, we refer to function
     [check_multiplication_parameters] below for details about how they are
     defined and calculated.
     Each integer pair (shift, bound) describes the limits of a corresponding
     variable v. In particular (v + shift) will be asserted to be in the
     interval [0, bound). *)
  val bounds_mul : (Z.t * Z.t) * (Z.t * Z.t) list

  val input_mod_int : ?kind:input_kind -> Z.t -> mod_int repr t

  val mod_int_of_scalars : scalar list repr -> mod_int repr t

  val scalars_of_mod_int : mod_int repr -> scalar list repr t

  val constant : Z.t -> mod_int repr t

  val zero : mod_int repr t

  val one : mod_int repr t

  (* val assert_equal : mod_int repr -> mod_int repr -> unit repr t *)

  (* [equal] makes use of an internal function [assert_non_zero] which
     requires [modulus] to be prime. This function would need to be generalized
     for arbitrary moduli *)
  val equal : mod_int repr -> mod_int repr -> bool repr t

  val add : mod_int repr -> mod_int repr -> mod_int repr t

  val sub : mod_int repr -> mod_int repr -> mod_int repr t

  val mul : mod_int repr -> mod_int repr -> mod_int repr t

  (* Division of [x] by [y] will make the circuit unsatisfiable
     if there does not exist [z] such that [x = z * y (mod modulus)]
     or if [y = 0] *)
  val div : mod_int repr -> mod_int repr -> mod_int repr t

  val neg : mod_int repr -> mod_int repr t

  (* Inversion of a non-invertible value (mod [modulus]) will make
     the circuit unsatisfiable *)
  val inv : mod_int repr -> mod_int repr t
end

(* Checks that the parameters are sound for implementing modular arithmetic,
   i.e., that there will be no wrap-around when checking equalities modulo the
   moduli and that such equalities imply an equality over the integers.
   This function returns a pair (qm_shift, qm_ubound):
     - qm_shift  : a constant used in all identities related to modular addition
     - qm_ubound : an upper-bound on the value of the quotient qm (on div. by m)
                   which needs to be asserted to be in the range [0, qm_ubound)
   This function also returns (as a second argument) a list of pairs
   (tj_shift, tj_ubound), one for each mj in moduli:
     - tj_shift  : a constant used in the identity modulo mj
     - tj_ubound : an upper-bound on the value of the quotient tj (on div. by mj)
                   which needs to be asserted to be in the range [0, tj_ubound) *)
let check_addition_parameters ~modulus:m ~base ~nb_limbs ~moduli =
  (* Assert that we can encode any integer in [Z_m] with [nb_limbs] limbs of
     size [base]. If the following is a strict inequality, then some values in
     [Z_m] may have more than one representative in limbs-form. This does not
     harm security, but requires a dedicated mechanism for equality checking. *)
  assert (Z.(pow base nb_limbs >= m)) ;

  (* We enforce z = (x + y) mod m with the equation:
     \sum_i (B^i mod m) * (x_i + y_i - z_i) = qm * m

     In that case, we can establish the following bounds on qm:
       qm_min =   - (B-1) * \sum_i (B^i mod m) / m
       qm_max = 2 * (B-1) * \sum_i (B^i mod m) / m *)
  let sum = List.fold_left Z.add Z.zero in
  let ( %! ) = Z.rem in
  let bs_mod_m = List.init nb_limbs (fun i -> Z.pow base i %! m) in
  let sum_bs_mod_m = sum bs_mod_m in
  let qm_min = Z.(div (neg (base - one) * sum_bs_mod_m) m) in
  let qm_max = Z.(div (of_int 2 * (base - one) * sum_bs_mod_m) m) in

  (* We can thus restrict qm to be in [qm_min, qm_max] or any bigger interval
     (for correctness). In order for the interval to start at 0, let us modify
     the above equation as follows:
     \sum_i (B^i mod m) * (x_i + y_i - z_i) = (qm + qm_min) * m

     We can now bound this new shifted qm in the interval [0, qm_max - qm_min].
     For compatibility with our range-check protocol, we will upper-bound
     the interval by a power of 2^15, the one immediately larger than
     qm_max - qm_min.
     (Note that qm_min is negative, thus (qm_max - qm_min) > qm_max.)
     We choose the next power of 2^15 as the range-check protocol will
     eventually be optimized (combined with Plookup) to process integers
     in chunks of 15 bits. Until this is done, it might be more efficient
     to replace 15 by 1 here.

     We now define:
       qm_shift  := qm_min
       qm_ubound > qm_max - qm_min

     These values are all the information we need to remember about qm limits,
     again, qm will be asserted to be in the range [0, qm_ubound) and qm_shift
     will be used in the identities. *)
  let qm_shift = qm_min in
  let qm_ubound =
    Z.(
      shift_left
        one
        (Utils.next_multiple_of 15 @@ numbits (qm_max - qm_min + one)))
  in

  (* Now, assuming qm is restricted in [0, qm_ubound), let us bound the amount
     \sum_i (B^i mod m) * (x_i + y_i - z_i) - (qm + qm_shift) * m

     lower_bound:   - (B-1) * \sum_i (B^i mod m) - (qm_ubound + qm_shift) * m
     upper_bound: 2 * (B-1) * \sum_i (B^i mod m) - qm_shift * m

     Then, if we define M := native_modulus :: moduli, lcm(M) must be larger
     than (upper_bound - lower_bound) to guarantee that a solution modulo lcm(M)
     implies a solution over the integers. *)
  let lower_bound =
    Z.((neg (base - one) * sum_bs_mod_m) - ((qm_ubound + qm_shift) * m))
  in
  let upper_bound =
    Z.((of_int 2 * (base - one) * sum_bs_mod_m) - (qm_shift * m))
  in
  let lcm_M_lbound = Z.(upper_bound - lower_bound) in

  if List.fold_left Z.lcm Z.one (S.order :: moduli) <= lcm_M_lbound then
    raise
    @@ Failure
         (Format.sprintf
            "Not enough moduli are provided for modular addition, try adding \
             more values to [moduli_add] of the gadget for arithmetic modulo \
             %s"
         @@ Z.to_string m) ;

  (* For every mj in M, we need to enforce the equation:
     \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
       - qm * (m mod mj) - ((qm_shift * m) mod mj) = tj * mj

     with the exception of the native modulus p := S.order, for which
     we can directly check:
      \sum_i ((B^i mod m) mod p) * (x_i + y_i - z_i)
        - (qm + qm_shift) * (m mod p) =_{p} 0

     For the moduli <> p, we need to bound the corresponding auxiliary
     variable tj. As before, we will first bound tj in the interval
     [tj_min, tj_max] and then apply a small modification to shift it to
     the interval [0, tj_ubound) where tj_ubound is the power of 2^15
     immediately above (tj_max - tj_min). *)
  let ts_bounds =
    List.map
      (fun mj ->
        (* We can establish the following bounds on tj:
           tj_min =
           (- (B-1) * (\sum_i (B^i mod m) mod mj)
            - qm_ubound * (m mod mj) - ((qm_shift * m) mod mj)) / mj
           tj_max =
           (2 * (B-1) * (\sum_i (B^i mod m) mod mj)
              - (qm_shift * m) mod mj) / mj *)
        let qm_shift_m_mod_mj = Z.(qm_shift * m %! mj) in
        let bs_mod_m_mod_mj = List.map (fun v -> v %! mj) bs_mod_m in
        let sum_bound = Z.((base - one) * sum bs_mod_m_mod_mj) in
        let tj_min =
          Z.(
            div (neg sum_bound - (qm_ubound * (m %! mj)) - qm_shift_m_mod_mj) mj)
        in
        let tj_max = Z.(div ((of_int 2 * sum_bound) - qm_shift_m_mod_mj) mj) in

        (* We will modify the equation on mj as follows:
           \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
             - qm * (m mod mj) - ((qm_shift * m) mod mj) = (tj + tj_min) * mj

           and bound the new tj in the interval [0, tj_ubound), where tj_ubound
           is the smallest power of 2^15 larger than tj_max - tj_min.
           We also define tj_shift := tj_min. *)
        let tj_shift = tj_min in
        let tj_ubound =
          Z.(
            shift_left
              one
              (Utils.next_multiple_of 15 @@ numbits (tj_max - tj_min + one)))
        in

        (* Now, assuming tj is restricted to [0, tj_ubound), we can bound the
           following amount:
            \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
              - qm * (m mod mj) - ((qm_shift * m) mod mj)
              - (tj + tj_shift) * mj *)
        let lower_bound =
          Z.(
            neg sum_bound
            - (qm_ubound * (m %! mj))
            - qm_shift_m_mod_mj
            - ((tj_ubound + tj_shift) * mj))
        in
        let upper_bound =
          Z.((of_int 2 * sum_bound) - qm_shift_m_mod_mj - (tj_shift * mj))
        in

        (* Assert that there will be no wrap-around *)
        if Z.(upper_bound - lower_bound >= S.order) then
          raise
          @@ Failure
               (Format.sprintf
                  "We cannot make sure that there will be no wrap-around in \
                   the identities modulo %s, try replacing this modulo in \
                   [moduli_add] of the gadget for arithmetic modulo %s"
                  (Z.to_string mj)
                  (Z.to_string m)) ;
        (tj_shift, tj_ubound))
      moduli
  in
  ((qm_shift, qm_ubound), ts_bounds)

(* Checks that the parameters are sound for implementing modular arithmetic,
   i.e., that there will be no wrap-around when checking equalities modulo the
   moduli and that such equalities imply an equality over the integers.
   This function returns a pair (qm_shift, qm_ubound):
     - qm_shift  : a constant used in all identities related to modular mult.
     - qm_ubound : an upper-bound on the value of the quotient qm (on div. by m)
                   which needs to be asserted to be in the range [0, qm_ubound)
   This function also returns (as a second argument) a list of pairs
   (tj_shift, tj_ubound), one for each mj in moduli:
     - tj_shift  : a constant used in the identity modulo mj
     - tj_ubound : an upper-bound on the value of the quotient tj (on div. by mj)
                   which needs to be asserted to be in the range [0, tj_ubound) *)
let check_multiplication_parameters ~modulus:m ~base ~nb_limbs ~moduli =
  (* Assert that we can encode any integer in [Z_m] with [nb_limbs] limbs of
     size [base]. If the following is a strict inequality, then some values in
     [Z_m] may have more than one representative in limbs-form. This does not
     harm security, but requires a dedicated mechanism for equality checking. *)
  assert (Z.(pow base nb_limbs >= m)) ;

  (* We enforce z = (x * y) mod m with the equation:
     \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_j) - (\sum_i (B^i mod m) * z_i)
        = qm * m

     In that case, we can establish the following bounds on qm:
       qm_min = - (B-1) * \sum_i (B^i mod m) / m
       qm_max =   (B-1)^2 * (\sum_i \sum_j (B^{i+j} mod m)) / m *)
  let sum = List.fold_left Z.add Z.zero in
  let ( %! ) = Z.rem in
  let bs_mod_m = List.init nb_limbs (fun i -> Z.pow base i %! m) in
  let bij_mod_m =
    List.init nb_limbs (fun i ->
        List.init nb_limbs (fun j -> Z.pow base (i + j) %! m))
    |> List.concat
  in
  let sum_bs_mod_m = sum bs_mod_m in
  let sum_bij_mod_m = sum bij_mod_m in
  let qm_min = Z.(div (neg (base - one) * sum_bs_mod_m) m) in
  let qm_max = Z.(div (pow (base - one) 2 * sum_bij_mod_m) m) in

  (* We can thus restrict qm to be in [qm_min, qm_max] or any bigger interval
     (for correctness). In order for the interval to start at 0, let us modify
     the above equation as follows:
      \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_j) - (\sum_i (B^i mod m) * z_i)
         = (qm + qm_min) * m

     We can now bound this new shifted qm in the interval [0, qm_max - qm_min].
     For compatibility with our range-check protocol, we will upper-bound
     the interval by a power of 2^15, the one immediately larger than
     qm_max - qm_min.
     (Note that qm_min is negative, thus (qm_max - qm_min) > qm_max.)
     We choose the next power of 2^15 as the range-check protocol will
     eventually be optimized (combined with Plookup) to process integers
     in chunks of 15 bits. Until this is done, it might be more efficient
     to replace 15 by 1 here.

     We now define:
       qm_shift  := qm_min
       qm_ubound > qm_max - qm_min

     These values are all the information we need to remember about qm limits,
     again, qm will be asserted to be in the range [0, qm_ubound) and qm_shift
     will be used in the identities. *)
  let qm_shift = qm_min in
  let qm_ubound =
    Z.(
      shift_left
        one
        (Utils.next_multiple_of 15 @@ numbits (qm_max - qm_min + one)))
  in

  (* Now, assuming qm is restricted in [0, qm_ubound), let us bound the amount
     \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_j) - (\sum_i (B^i mod m) * z_i)
        - (qm + qm_shift) * m

     lower_bound: - (B-1) * \sum_i (B^i mod m) - (qm_ubound + qm_shift) * m
     upper_bound:   (B-1)^2 * (\sum_i \sum_j (B^{i+j} mod m)) - qm_shift * m

     Then, if we define M := native_modulus :: moduli, lcm(M) must be larger
     than (upper_bound - lower_bound) to guarantee that a solution modulo lcm(M)
     implies a solution over the integers. *)
  let lower_bound =
    Z.((neg (base - one) * sum_bs_mod_m) - ((qm_ubound + qm_shift) * m))
  in
  let upper_bound = Z.((pow (base - one) 2 * sum_bij_mod_m) - (qm_shift * m)) in
  let lcm_M_lbound = Z.(upper_bound - lower_bound) in

  if List.fold_left Z.lcm Z.one (S.order :: moduli) <= lcm_M_lbound then
    raise
    @@ Failure
         (Format.sprintf
            "Not enough moduli are provided for modular multiplication, try \
             adding more values to [moduli_mul] of the gadget for arithmetic \
             modulo %s"
         @@ Z.to_string m) ;

  (* For every mj in M, we need to enforce the equation:
     \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_j) - (\sum_i (B^i mod m) * z_i)
        - qm * (m mod mj) - ((qm_shift * m) mod mj) = tj * mj

     with the exception of the native modulus p := S.order, for which
     we can directly check:
      \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_j) - (\sum_i (B^i mod m) * z_i)
         - (qm + qm_shift) * (m mod p) =_{p} 0

     For the moduli <> p, we need to bound the corresponding auxiliary
     variable tj. As before, we will first bound tj in the interval
     [tj_min, tj_max] and then apply a small modification to shift it to
     the interval [0, tj_ubound) where tj_ubound is the power of 2^15
     immediately above (tj_max - tj_min). *)
  let ts_bounds =
    List.map
      (fun mj ->
        (* We can establish the following bounds on tj:
           tj_min =
           (- (B-1) * (\sum_i (B^i mod m) mod mj)
            - qm_ubound * (m mod mj) - ((qm_shift * m) mod mj)) / mj
           tj_max =
           ((B-1)^2 * (\sum_i \sum_j ((B^{i+j} mod m) mod mj))
              - (qm_shift * m) mod mj) / mj *)
        let qm_shift_m_mod_mj = Z.(qm_shift * m %! mj) in
        let bs_mod_m_mod_mj = List.map (fun v -> v %! mj) bs_mod_m in
        let bij_mod_m_mod_mj = List.map (fun v -> v %! mj) bij_mod_m in
        let sum_bound_min = Z.((base - one) * sum bs_mod_m_mod_mj) in
        let sum_bound_max = Z.(pow (base - one) 2 * sum bij_mod_m_mod_mj) in
        let tj_min =
          Z.(
            div
              (neg sum_bound_min - (qm_ubound * (m %! mj)) - qm_shift_m_mod_mj)
              mj)
        in
        let tj_max = Z.(div (sum_bound_max - qm_shift_m_mod_mj) mj) in

        (* We will modify the equation on mj as follows:
           \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_i) - (\sum_i (B^i mod m) * z_i)
              - qm * (m mod mj) - ((qm_shift * m) mod mj) = (tj + tj_min) * mj

           and bound the new tj in the interval [0, tj_ubound), where tj_ubound
           is the smallest power of 2^15 larger than tj_max - tj_min.
           We also define tj_shift := tj_min. *)
        let tj_shift = tj_min in
        let tj_ubound =
          Z.(
            shift_left
              one
              (Utils.next_multiple_of 15 @@ numbits (tj_max - tj_min + one)))
        in

        (* Now, assuming tj is restricted to [0, tj_ubound), we can bound the
           following amount:
           \sum_i (\sum_j (B^{i+j} mod m) * x_i * y_i) - (\sum_i (B^i mod m) * z_i)
              - qm * (m mod mj) - ((qm_shift * m) mod mj)
              - (tj + tj_shift) * mj *)
        let lower_bound =
          Z.(
            neg sum_bound_min
            - (qm_ubound * (m %! mj))
            - qm_shift_m_mod_mj
            - ((tj_ubound + tj_shift) * mj))
        in
        let upper_bound =
          Z.(sum_bound_max - qm_shift_m_mod_mj - (tj_shift * mj))
        in

        (* Assert that there will be no wrap-around *)
        if Z.(upper_bound - lower_bound >= S.order) then
          raise
          @@ Failure
               (Format.sprintf
                  "We cannot make sure that there will be no wrap-around in \
                   the identities modulo %s, try replacing this modulo in \
                   [moduli_mul] of the gadget for arithmetic modulo %s"
                  (Z.to_string mj)
                  (Z.to_string m)) ;
        (tj_shift, tj_ubound))
      moduli
  in
  ((qm_shift, qm_ubound), ts_bounds)

module Make (Params : PARAMETERS) : MOD_ARITH =
functor
  (L : LIB)
  ->
  struct
    open L
    include Params

    type mod_int = scalar list

    let nb_limbs = Utils.min_nb_limbs ~modulus ~base

    let is_prime = not (Z.probab_prime modulus 50 = 0)

    let bounds_add =
      check_addition_parameters ~modulus ~base ~nb_limbs ~moduli:moduli_add

    let bounds_mul =
      check_multiplication_parameters
        ~modulus
        ~base
        ~nb_limbs
        ~moduli:moduli_mul

    let scalar_limbs_of_z n =
      let n = Z.rem n modulus in
      let n = if Z.(compare n zero) < 0 then Z.(n + modulus) else n in
      Utils.z_to_limbs ~len:nb_limbs ~base n |> List.map S.of_z

    let input_mod_int ?(kind = `Private) n =
      let ns = scalar_limbs_of_z n in
      let* i = Input.(list @@ List.map scalar ns) |> input ~kind in
      (* Assert well-formedness: range-check all limbs in [0, base) *)
      assert (Utils.is_power_of_2 Params.base) ;
      iterM (Num.range_check ~nb_bits:(Z.log2 Params.base)) (of_list i) >* ret i

    let mod_int_of_scalars ls =
      assert (List.compare_length_with (of_list ls) nb_limbs = 0) ;
      iterM (Num.range_check ~nb_bits:(Z.log2 Params.base)) (of_list ls)
      >* ret ls

    let scalars_of_mod_int n = ret n

    let constant n = mapM Num.constant @@ scalar_limbs_of_z n <$> to_list

    let zero = constant Z.zero

    let one = constant Z.one

    let add =
      Mod_arith.add
        ~subtraction:false
        ~label
        ~modulus
        ~nb_limbs
        ~base
        ~moduli:moduli_add
        ~qm_bound:(fst bounds_add)
        ~ts_bounds:(snd bounds_add)

    let sub =
      Mod_arith.add
        ~subtraction:true
        ~label
        ~modulus
        ~nb_limbs
        ~base
        ~moduli:moduli_add
        ~qm_bound:(fst bounds_add)
        ~ts_bounds:(snd bounds_add)

    let mul =
      Mod_arith.mul
        ~division:false
        ~label
        ~modulus
        ~nb_limbs
        ~base
        ~moduli:moduli_mul
        ~qm_bound:(fst bounds_mul)
        ~ts_bounds:(snd bounds_mul)

    let div xs ys =
      let moduli = moduli_mul in
      let qm_bound = fst bounds_mul in
      let ts_bounds = snd bounds_mul in
      Mod_arith.assert_non_zero
        ~label
        ~modulus
        ~is_prime
        ~nb_limbs
        ~base
        ~moduli
        ~qm_bound
        ~ts_bounds
        ys
      >* Mod_arith.mul
           ~division:true
           ~label
           ~modulus
           ~nb_limbs
           ~base
           ~moduli
           ~qm_bound
           ~ts_bounds
           xs
           ys

    let neg xs =
      let* zs = zero in
      sub zs xs

    let inv xs =
      let* os = one in
      (* We do not need to assert that [x <> 0], because the equation
         enforced by [Mod_arith.mul] will be [1 = z * x], which is
         unsatisfiable if [x = 0], because we are in an integral domain. *)
      Mod_arith.mul
        ~division:true
        ~label
        ~modulus
        ~nb_limbs
        ~base
        ~moduli:moduli_mul
        ~qm_bound:(fst bounds_mul)
        ~ts_bounds:(snd bounds_mul)
        os
        xs

    let equal xs ys =
      let* diff = sub xs ys in
      Mod_arith.is_zero
        ~label
        ~modulus
        ~is_prime
        ~nb_limbs
        ~base
        ~moduli:moduli_mul
        ~qm_bound:(fst bounds_mul)
        ~ts_bounds:(snd bounds_mul)
        diff
  end

module ArithMod25519 = Make (struct
  let label = "25519"

  let modulus = Z.(shift_left one 255 - of_int 19)

  let base = Z.(shift_left one 85)

  (* We want the moduli to be as large as possible (to reach the lcm bound with
     as few moduli as possible). But they cannot be too large (or else we will
     not be able to prevent wrap-arounds). The base := 2^85 is a fantastic
     modulus for algebra over 2^255-19, since it is much larger than what it is
     usually possible, but there is no wrap-around in its identity because
     its powers are small modulo 2^255-19. *)
  let moduli_add = [base]

  let moduli_mul = [base; Z.(base - one)]
end)

module ArithMod64 = Make (struct
  let label = "64"

  let modulus = Z.(shift_left one 64)

  let base = Z.(shift_left one 64)

  let moduli_add = []

  let moduli_mul = []
end)
