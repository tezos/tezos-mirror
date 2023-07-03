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
  type fp12

  external allocate_fq12 : unit -> fp12 = "allocate_fp12_stubs"

  external mul : fp12 -> fp12 -> fp12 -> int = "caml_blst_fp12_mul_stubs"

  external one : fp12 -> int = "caml_blst_fp12_one_stubs"

  external equal : fp12 -> fp12 -> bool = "caml_blst_fp12_is_equal_stubs"

  external is_zero : fp12 -> bool = "caml_blst_fp12_is_zero_stubs"

  external inverse : fp12 -> fp12 -> int = "caml_blst_fp12_inverse_stubs"

  external sqr : fp12 -> fp12 -> int = "caml_blst_fp12_sqr_stubs"

  external pow : fp12 -> fp12 -> Bytes.t -> int -> int
    = "caml_blst_fp12_pow_stubs"

  external to_bytes : Bytes.t -> fp12 -> int = "caml_blst_fp12_to_bytes_stubs"

  external of_bytes : fp12 -> Bytes.t -> int = "caml_blst_fp12_of_bytes_stubs"
end

module Fq12 = struct
  exception Not_in_field of Bytes.t

  type t = Stubs.fp12

  let order =
    let fq_order =
      Z.of_string
        "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
    in
    Z.pow fq_order 12

  let size_in_bytes = 48 * 12

  let to_bytes p =
    let buffer = Bytes.make size_in_bytes '\000' in
    ignore @@ Stubs.to_bytes buffer p ;
    buffer

  let of_bytes_opt bs =
    if Bytes.length bs <> size_in_bytes then None
    else
      let buffer = Stubs.allocate_fq12 () in
      ignore @@ Stubs.of_bytes buffer bs ;
      Some buffer

  let random ?state () =
    let buffer = Stubs.allocate_fq12 () in
    let bs =
      Bytes.concat
        Bytes.empty
        (List.init 12 (fun _ -> Fq.(to_bytes (random ?state ()))))
    in
    ignore @@ Stubs.of_bytes buffer bs ;
    buffer

  let of_bytes_exn bs =
    match of_bytes_opt bs with None -> raise (Not_in_field bs) | Some p -> p

  let one =
    let buffer = Stubs.allocate_fq12 () in
    ignore @@ Stubs.one buffer ;
    buffer

  let zero = Stubs.allocate_fq12 ()

  let size_in_memory = Obj.reachable_words (Obj.repr one) * 8

  let eq x y =
    let res = Stubs.equal x y in
    res

  let is_zero p =
    let res = eq p zero in
    res

  let is_one p = eq p one

  let mul x y =
    let buffer = Stubs.allocate_fq12 () in
    ignore @@ Stubs.mul buffer x y ;
    buffer

  let inverse_opt x =
    if is_zero x then None
    else
      let buffer = Stubs.allocate_fq12 () in
      ignore @@ Stubs.inverse buffer x ;
      Some buffer

  let inverse_exn x =
    if is_zero x then raise Division_by_zero
    else
      let buffer = Stubs.allocate_fq12 () in
      ignore @@ Stubs.inverse buffer x ;
      buffer

  let of_z x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    let coordinates = [x0; x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] in
    let coordinates =
      List.map (fun x -> Bytes.of_string (Z.to_bits x)) coordinates
    in
    let bs = Bytes.concat Bytes.empty coordinates in
    let buffer = Stubs.allocate_fq12 () in
    ignore @@ Stubs.of_bytes buffer bs ;
    buffer

  let of_string x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    let x0 = Z.of_string x0 in
    let x1 = Z.of_string x1 in
    let x2 = Z.of_string x2 in
    let x3 = Z.of_string x3 in
    let x4 = Z.of_string x4 in
    let x5 = Z.of_string x5 in
    let x6 = Z.of_string x6 in
    let x7 = Z.of_string x7 in
    let x8 = Z.of_string x8 in
    let x9 = Z.of_string x9 in
    let x10 = Z.of_string x10 in
    let x11 = Z.of_string x11 in
    of_z x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11

  let pow x n =
    let n = Z.erem n (Z.pred order) in
    let buffer = Stubs.allocate_fq12 () in
    let exp = Z.to_bits n |> Bytes.unsafe_of_string in
    let exp_len = Z.numbits n in
    ignore @@ Stubs.pow buffer x exp exp_len ;
    buffer
end

include Fq12
