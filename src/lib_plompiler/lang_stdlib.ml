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

open Lang_core

module type LIB = sig
  include COMMON

  val foldiM : ('a -> int -> 'a t) -> 'a -> int -> 'a t

  val fold2M : ('a -> 'b -> 'c -> 'a t) -> 'a -> 'b list -> 'c list -> 'a t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t

  val map2M : ('a -> 'b -> 'c t) -> 'a list -> 'b list -> 'c list t

  val iterM : ('a -> unit repr t) -> 'a list -> unit repr t

  val iter2M : ('a -> 'b -> unit repr t) -> 'a list -> 'b list -> unit repr t

  module Bool : sig
    include BOOL

    (** Returns the pair (s, c_out), as per
     https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder *)
    val full_adder : bool repr -> bool repr -> bool repr -> (bool * bool) repr t
  end
  with type scalar = scalar
   and type 'a repr = 'a repr
   and type 'a t = 'a t

  module Num : sig
    include NUM

    val square : scalar repr -> scalar repr t

    val pow : scalar repr -> bool repr list -> scalar repr t

    val add_list :
      ?qc:S.t -> ?coeffs:S.t list -> scalar list repr -> scalar repr t

    val mul_list : scalar list repr -> scalar repr t

    val mul_by_constant : S.t -> scalar repr -> scalar repr t

    val scalar_of_bytes : bool list repr -> scalar repr t

    val is_eq_const : scalar repr -> S.t -> bool repr t

    val assert_eq_const : scalar repr -> S.t -> unit repr t

    (** [is_upper_bounded ~bound x] returns whether the scalar [x] is
        strictly lower than [bound] when [x] is interpreted as an integer
        from [0] to [p-1] (being [p] the scalar field order).
        This circuit is total (and more expensive than our version below). *)
    val is_upper_bounded : bound:Z.t -> scalar repr -> bool repr t

    (** Same as [is_upper_bounded] but cheaper and partial.
        [is_upper_bounded_unsafe ~bound l] is unsatisfiable if l cannot be
        represented in binary with [Z.numbits bound] bits. *)
    val is_upper_bounded_unsafe :
      ?nb_bits:int -> bound:Z.t -> scalar repr -> bool repr t

    (** [geq (a, bound_a) (b, bound_b)] returns the boolean wire representing
        a >= b.
        Pre-condition: [a ∈ \[0, bound_a) ∧ b ∈ \[0, bound_b)] *)
    val geq : scalar repr * Z.t -> scalar repr * Z.t -> bool repr t
  end
  with type scalar = scalar
   and type 'a repr = 'a repr
   and type 'a t = 'a t

  module Enum (N : sig
    val n : int
  end) : sig
    (* [switch_case k l] returns the k-th element of the list [l] if k ∈ [0,n)
       or the first element of [l] otherwise. *)
    val switch_case : scalar repr -> 'a list repr -> 'a repr t
  end

  module Bytes : sig
    type bl = bool list

    val add : ?ignore_carry:bool -> bl repr -> bl repr -> bl repr t

    val xor : bl repr -> bl repr -> bl repr t

    val rotate : bl repr -> int -> bl repr
  end

  val add2 :
    (scalar * scalar) repr -> (scalar * scalar) repr -> (scalar * scalar) repr t

  val constant_bool : bool -> bool repr t

  val constant_bytes : ?le:bool -> bytes -> Bytes.bl repr t

  val constant_uint32 : ?le:bool -> Stdint.uint32 -> Bytes.bl repr t
end

module Lib (C : COMMON) = struct
  include C

  let foldiM : ('a -> int -> 'a t) -> 'a -> int -> 'a t =
   fun f e n -> foldM f e (List.init n (fun i -> i))

  let fold2M f acc ls rs =
    foldM (fun acc (l, r) -> f acc l r) acc (List.combine ls rs)

  let mapM f l =
    let* l =
      foldM
        (fun acc e ->
          let* e = f e in
          ret @@ (e :: acc))
        []
        l
    in
    ret @@ List.rev l

  let map2M f ls rs = mapM (fun (l, r) -> f l r) (List.combine ls rs)

  let iterM f l = foldM (fun _ a -> f a) unit l

  let iter2M f l r = iterM (fun (l, r) -> f l r) (List.combine l r)

  module Bool = struct
    include Bool

    let full_adder a b c_in =
      let* a_xor_b = xor a b in
      let* a_xor_b_xor_c = xor a_xor_b c_in in
      let* a_xor_b_and_c = band a_xor_b c_in in
      let* a_and_b = band a b in
      let* c = bor a_xor_b_and_c a_and_b in
      ret (pair a_xor_b_xor_c c)
  end

  module Num = struct
    include Num

    let square l = mul l l

    let pow x n_list =
      let* init =
        let* left = constant_scalar S.one in
        ret (left, x)
      in
      let* res, _acc =
        foldM
          (fun (res, acc) bool ->
            let* res_true = mul res acc in
            let* res = Bool.ifthenelse bool res_true res in
            let* acc = mul acc acc in
            ret (res, acc))
          init
          n_list
      in
      ret res

    let add_list ?(qc = S.zero) ?(coeffs = []) l =
      let l = of_list l in
      let q =
        if coeffs != [] then coeffs
        else List.init (List.length l) (fun _ -> S.one)
      in
      assert (List.compare_lengths q l = 0) ;
      match (l, q) with
      | x1 :: x2 :: xs, ql :: qr :: qs ->
          let* res = Num.add ~qc ~ql ~qr x1 x2 in
          fold2M (fun acc x ql -> Num.add ~ql x acc) res xs qs
      | [x], [ql] -> Num.add_constant ~ql qc x
      | [], [] -> constant_scalar qc
      | _, _ -> assert false

    let mul_list l =
      match of_list l with [] -> assert false | x :: xs -> foldM Num.mul x xs

    let mul_by_constant s x = Num.add_constant ~ql:s S.zero x

    (* Evaluates P(X) = \sum_i bᵢ Xⁱ at 2 with Horner's method:
       P(2) = b₀ + 2 (b₁+ 2 (b₂ + 2(…))). *)
    let scalar_of_bytes b =
      let* zero = constant_scalar S.zero in
      foldM
        (fun acc b -> add acc (scalar_of_bool b) ~ql:S.(one + one) ~qr:S.one)
        zero
        (List.rev (of_list b))

    let assert_eq_const l s = Num.assert_custom ~ql:S.mone ~qc:s l l l

    let is_eq_const l s =
      let* diff = add_constant ~ql:S.mone s l in
      is_zero diff

    (* Function used by [is_upper_bounded(_unsafe)] *)
    let ignore_leading_zeros ~nb_bits ~bound xbits =
      (* We can ignore all leading zeros in the bound's little-endian binary
         decomposition. The assertion cannot be satisfied if they are all zeros. *)
      let rec shave_zeros = function
        | [] ->
            raise
              (Invalid_argument
                 "is_upper_bounded cannot be satisfied on bound = 0")
        | (x, true) :: tl -> (x, tl)
        | (_, false) :: tl -> shave_zeros tl
      in
      List.combine (of_list xbits) (Utils.bool_list_of_z ~nb_bits bound)
      |> shave_zeros

    (* Let [(bn,...,b0)] and [(xn,...,x0)] be binary representations
       of [bound] and [x] respectively where the least significant bit is
       indexed by [0]. Let [op_i = if b_i = 1 then band else bor] for all [i].
       Predicate [x] < [bound] can be expressed as the negation of predicate:
       [ xn op_n (... (x1 op_1 (x0 op_0 true))) ].
       Intuitively we need to carry through a flag that indicates if up to
       step i, x is greater than b. In order for x[0,i] to be greater than
       b[0,i]:
       - if b_i = one then x_i will need to match it and the flag must be true.
       - if b_i = zero then x needs to be one if the flag is false or it can
         have any value if the flag is already true. *)
    let is_upper_bounded_unsafe ?nb_bits ~bound x =
      assert (Z.zero <= bound && bound < S.order) ;
      let nb_bits =
        Option.value
          ~default:(if Z.(equal bound zero) then 1 else Z.numbits bound)
          nb_bits
      in
      let* xbits = bits_of_scalar ~nb_bits x in
      let init, xibi = ignore_leading_zeros ~nb_bits ~bound xbits in
      let* geq =
        foldM
          (fun acc (xi, bi) ->
            let op = if bi then Bool.band else Bool.bor in
            op xi acc)
          init
          xibi
      in
      Bool.bnot geq

    let is_upper_bounded ~bound x =
      assert (Z.zero <= bound && bound < S.order) ;
      let nb_bits = Z.numbits S.order in
      let bound_plus_alpha = Z.(bound + Utils.alpha) in
      let* xbits = bits_of_scalar ~shift:Utils.alpha ~nb_bits x in
      let init, xibi =
        ignore_leading_zeros ~nb_bits ~bound:bound_plus_alpha xbits
      in
      let* geq =
        foldM
          (fun acc (xi, bi) ->
            let op = if bi then Bool.band else Bool.bor in
            op xi acc)
          init
          xibi
      in
      Bool.bnot geq

    let geq (a, bound_a) (b, bound_b) =
      (* (a - b) + bound_b - 1 ∈ [0, bound_a + bound_b - 1) *)
      let* shifted_diff =
        Num.add ~qr:S.mone ~qc:(S.of_z Z.(pred bound_b)) a b
      in
      let nb_bits = Z.(numbits @@ pred (add bound_a bound_b)) in
      let* bits = bits_of_scalar ~nb_bits shifted_diff in
      let init, xibi =
        ignore_leading_zeros ~nb_bits ~bound:Z.(pred bound_b) bits
      in
      foldM
        (fun acc (xi, bi) ->
          let op = if bi then Bool.band else Bool.bor in
          op xi acc)
        init
        xibi
  end

  module Enum (N : sig
    val n : int
  end) =
  struct
    let switch_case k cases =
      let cases = of_list cases in
      assert (List.compare_length_with cases N.n = 0) ;
      let indexed = List.mapi (fun i x -> (i, x)) cases in
      foldM
        (fun c (i, ci) ->
          let* f = Num.is_eq_const k (S.of_z (Z.of_int i)) in
          Bool.ifthenelse f ci c)
        (snd @@ List.hd indexed)
        (List.tl indexed)
  end

  module Bytes = struct
    type bl = bool list

    let add ?(ignore_carry = false) a b =
      let ha, ta = (List.hd (of_list a), List.tl (of_list a)) in
      let hb, tb = (List.hd (of_list b), List.tl (of_list b)) in
      let* a_xor_b = Bool.xor ha hb in
      let* a_and_b = Bool.band ha hb in
      let* res, carry =
        fold2M
          (fun (res, c) a b ->
            let* p = Bool.full_adder a b c in
            let s, c = of_pair p in
            ret (s :: res, c))
          ([a_xor_b], a_and_b)
          ta
          tb
      in
      ret @@ to_list @@ List.rev (if ignore_carry then res else carry :: res)

    let xor a b =
      let* l = map2M Bool.xor (of_list a) (of_list b) in
      ret @@ to_list l

    let rotate a i =
      let split_n n l =
        let rec aux acc k l =
          if k = n then (List.rev acc, l)
          else
            match l with
            | h :: t -> aux (h :: acc) (k + 1) t
            | [] ->
                raise
                  (Invalid_argument
                     (Printf.sprintf "split_n: n=%d >= List.length l=%d" n k))
        in
        aux [] 0 l
      in
      let head, tail = split_n i (of_list a) in
      to_list @@ tail @ head
  end

  let add2 p1 p2 =
    let x1, y1 = of_pair p1 in
    let x2, y2 = of_pair p2 in
    let* x3 = Num.add x1 x2 in
    let* y3 = Num.add y1 y2 in
    ret (pair x3 y3)

  let constant_bool b =
    let* bit = constant_scalar (if b then S.one else S.zero) in
    ret @@ unsafe_bool_of_scalar bit

  let constant_bytes ?(le = false) b =
    let bl = Utils.bitlist ~le b in
    let* ws =
      foldM
        (fun ws bit ->
          let* w = constant_bool bit in
          ret (w :: ws))
        []
        bl
    in
    ret @@ to_list @@ List.rev ws

  let constant_uint32 ?(le = false) u32 =
    let b = Stdlib.Bytes.create 4 in
    Stdint.Uint32.to_bytes_big_endian u32 b 0 ;
    constant_bytes ~le b
end
