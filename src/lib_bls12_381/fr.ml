(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

module Stubs = struct
  type fr

  type scalar

  external allocate_scalar : unit -> scalar = "allocate_scalar_stubs"

  external callocate_fr : unit -> fr = "callocate_fr_stubs"

  external mallocate_fr : unit -> fr = "mallocate_fr_stubs"

  external scalar_of_fr : scalar -> fr -> int = "caml_blst_scalar_from_fr_stubs"

  external fr_of_scalar : fr -> scalar -> int = "caml_blst_fr_from_scalar_stubs"

  external fr_of_bytes_le : fr -> Bytes.t -> bool
    = "caml_blst_fr_from_lendian_stubs"

  external fr_to_bytes_le : Bytes.t -> fr -> int
    = "caml_blst_lendian_from_fr_stubs"

  external scalar_of_bytes_le : scalar -> Bytes.t -> int
    = "caml_blst_scalar_of_bytes_stubs"

  external scalar_to_bytes_le : Bytes.t -> scalar -> int
    = "caml_blst_scalar_to_bytes_stubs"

  external check_scalar : scalar -> bool = "caml_blst_check_scalar_stubs"

  external add : fr -> fr -> fr -> int = "caml_blst_fr_add_stubs"

  external eq : fr -> fr -> bool = "caml_blst_fr_is_equal_stubs"

  external cneg : fr -> fr -> bool -> bool = "caml_blst_fr_cneg_stubs"

  external is_zero : fr -> bool = "caml_blst_fr_is_zero_stubs"

  external is_one : fr -> bool = "caml_blst_fr_is_one_stubs"

  external sub : fr -> fr -> fr -> int = "caml_blst_fr_sub_stubs"

  external mul : fr -> fr -> fr -> int = "caml_blst_fr_mul_stubs"

  external pow : fr -> fr -> Bytes.t -> int -> int = "caml_blst_fr_pow_stubs"

  external sqr : fr -> fr -> int = "caml_blst_fr_sqr_stubs"

  external eucl_inverse : fr -> fr -> int = "caml_blst_fr_eucl_inverse_stubs"

  external memcpy : fr -> fr -> int = "caml_blst_fr_memcpy_stubs"

  external inner_product : fr -> fr array -> fr array -> int -> int
    = "caml_blst_fr_inner_product_stubs"
end

(* module = Blst_bindings.r (Blst_stubs) *)

module Fr = struct
  exception Not_in_field of Bytes.t

  type t = Stubs.fr

  let copy src =
    let dst = Stubs.mallocate_fr () in
    ignore @@ Stubs.memcpy dst src ;
    dst

  let memcpy a b = ignore @@ Stubs.memcpy a b

  let size_in_bytes = 32

  let order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"

  let pad_if_require bs =
    (* Pad to 32 bytes. In anycase, copy the bytes to a new buffer *)
    if Bytes.length bs < size_in_bytes then (
      let padded_bytes = Bytes.make size_in_bytes '\000' in
      Bytes.blit bs 0 padded_bytes 0 (Bytes.length bs) ;
      padded_bytes)
    else Bytes.copy bs

  let of_bytes_opt bs =
    if Bytes.length bs > size_in_bytes then None
    else
      let bs = pad_if_require bs in
      let buffer = Stubs.mallocate_fr () in
      let is_ok = Stubs.fr_of_bytes_le buffer bs in
      if is_ok then Some buffer else None

  let of_bytes_exn bs =
    let buffer_opt = of_bytes_opt bs in
    match buffer_opt with
    | None -> raise (Not_in_field bs)
    | Some buffer -> buffer

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then (
      let buffer_scalar = Stubs.allocate_scalar () in
      ignore @@ Stubs.scalar_of_bytes_le buffer_scalar bs ;
      Stubs.check_scalar buffer_scalar)
    else false

  let zero = of_bytes_exn (Bytes.make size_in_bytes '\000')

  let one =
    let bytes = Bytes.make size_in_bytes '\000' in
    Bytes.set bytes 0 '\001' ;
    of_bytes_exn bytes

  let to_bytes x =
    let buffer_bytes = Bytes.make size_in_bytes '\000' in
    ignore @@ Stubs.fr_to_bytes_le buffer_bytes x ;
    buffer_bytes

  let size_in_memory = Obj.reachable_words (Obj.repr one) * 8

  let eq x y = Stubs.eq x y

  let ( = ) = eq

  let is_zero s = Stubs.is_zero s

  let is_one s = Stubs.is_one s

  let rec random ?state () =
    let random_int =
      match state with
      | None -> Random.int
      | Some state -> Random.State.int state
    in
    let random_bytes =
      Bytes.init size_in_bytes (fun _ -> char_of_int @@ random_int 256)
    in
    let res = of_bytes_opt random_bytes in
    match res with None -> random ?state () | Some res -> res

  let rec non_null_random ?state () =
    let r = random ?state () in
    if is_zero r then non_null_random ?state () else r

  let add x y =
    let buffer = Stubs.mallocate_fr () in
    ignore @@ Stubs.add buffer x y ;
    buffer

  let add_inplace res a b = ignore @@ Stubs.add res a b

  let add_bulk xs =
    let buffer = Stubs.callocate_fr () in
    List.iter (fun x -> ignore @@ Stubs.add buffer buffer x) xs ;
    buffer

  let ( + ) = add

  let mul x y =
    let buffer = Stubs.mallocate_fr () in
    ignore @@ Stubs.mul buffer x y ;
    buffer

  let mul_inplace res a b = ignore @@ Stubs.mul res a b

  let mul_bulk xs =
    let buffer = Stubs.callocate_fr () in
    ignore @@ Stubs.add buffer buffer one ;
    List.iter (fun x -> ignore @@ Stubs.mul buffer buffer x) xs ;
    buffer

  let ( * ) = mul

  let inverse_opt x =
    if is_zero x then None
    else
      let buffer = Stubs.mallocate_fr () in
      ignore @@ Stubs.eucl_inverse buffer x ;
      Some buffer

  let inverse_exn x =
    match inverse_opt x with None -> raise Division_by_zero | Some x -> x

  let inverse_exn_inplace res x =
    if is_zero x then raise Division_by_zero
    else ignore @@ Stubs.eucl_inverse res x

  let sub a b =
    let buffer = Stubs.mallocate_fr () in
    ignore @@ Stubs.sub buffer a b ;
    buffer

  let sub_inplace res x y = ignore @@ Stubs.sub res x y

  let square x =
    let buffer = Stubs.mallocate_fr () in
    ignore @@ Stubs.sqr buffer x ;
    buffer

  let square_inplace res x = ignore @@ Stubs.sqr res x

  let double x = x + x

  let double_inplace res x = ignore @@ Stubs.add res x x

  let negate x =
    let buffer = Stubs.mallocate_fr () in
    ignore @@ Stubs.cneg buffer x true ;
    buffer

  let negate_inplace res x = ignore @@ Stubs.cneg res x true

  let ( - ) = negate

  let div_exn x y = x * inverse_exn y

  let div_opt x y =
    match inverse_opt y with None -> None | Some inv_y -> Some (x * inv_y)

  let ( / ) = div_exn

  let two_z = Z.(one + one)

  let pow x n =
    let n = Z.erem n (Z.pred order) in
    let buffer = Stubs.mallocate_fr () in
    let exp = Z.to_bits n |> Bytes.unsafe_of_string in
    let exp_len = Z.numbits n in
    ignore @@ Stubs.pow buffer x exp exp_len ;
    buffer

  let ( ** ) = pow

  let to_string s =
    let bytes = to_bytes s in
    let z = Z.of_bits (Bytes.to_string bytes) in
    Z.to_string z

  let of_z z =
    let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
    let x = Bytes.make size_in_bytes '\000' in
    Bytes.blit z 0 x 0 (min (Bytes.length z) size_in_bytes) ;
    of_bytes_exn x

  let to_z b =
    let bytes = to_bytes b in
    Z.of_bits (Bytes.to_string bytes)

  let of_string s = of_z (Z.of_string s)

  let factor_power_of_two =
    let rec aux i n =
      let q, r = Z.ediv_rem n two_z in
      if Z.equal r Z.zero then aux Int.(succ i) q else (i, n)
    in
    aux 0 (Z.pred order)

  let legendre_symbol x =
    if is_zero x then Z.zero
    else if is_one (pow x (Z.divexact (Z.pred order) (Z.of_int 2))) then Z.one
    else Z.neg Z.one

  let is_quadratic_residue x =
    if is_zero x then true else Z.equal (legendre_symbol x) Z.one

  let rec pick_non_square () =
    let z = random ?state:None () in
    if Z.equal (legendre_symbol z) (Z.of_int (-1)) then z
    else pick_non_square ()

  let sqrt_opt x =
    if not (is_quadratic_residue x) then None
    else
      (* https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm *)
      let s, q = factor_power_of_two in
      (* implies p = 3 mod 4 *)
      if Int.equal s 1 then
        (* r = x^((p + 1) / 4) *)
        let r = pow x (Z.divexact (Z.succ order) (Z.of_string "4")) in
        Some r
      else
        let rec compute_lowest_n_2th_root_of_unity (i : int) x upper : int =
          let x = square x in
          if is_one x then i
          else if Int.(equal i upper) then
            failwith "Upperbound should be higher"
            (* should never happen in this case, just being explicit *)
          else compute_lowest_n_2th_root_of_unity (Int.succ i) x upper
        in
        let z = pick_non_square () in
        let c = pow z q in
        let rec aux m c t r =
          if eq t zero then zero (* case x is zero *)
          else if eq t one then r (* base case *)
          else
            let i = compute_lowest_n_2th_root_of_unity 1 t m in
            let b = pow c (Z.pow two_z Int.(pred (sub m i))) in
            let m = i in
            let c = mul b b in
            let t = mul t c in
            let r = mul r b in
            aux m c t r
        in
        Some (aux s c (pow x q) (pow x (Z.divexact (Z.succ q) two_z)))

  let compare x y = Stdlib.compare (to_bytes x) (to_bytes y)

  let inner_product_opt a b =
    if Array.length a <> Array.length b then None
    else
      let res = copy zero in
      ignore @@ Stubs.inner_product res a b (Array.length a) ;
      Some res

  let inner_product_exn a b =
    match inner_product_opt a b with
    | None ->
        raise (Invalid_argument "Both parameters must be of the same length")
    | Some x -> x

  let of_int x = of_z (Z.of_int x)
end

include Fr
