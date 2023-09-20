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
  type t

  external allocate_fp2 : unit -> t = "allocate_fp2_stubs"

  external add : t -> t -> t -> int = "caml_blst_fp2_add_stubs"

  external mul : t -> t -> t -> int = "caml_blst_fp2_mul_stubs"

  external sqrt : t -> t -> bool = "caml_blst_fp2_sqrt_stubs"

  external cneg : t -> t -> bool -> int = "caml_blst_fp2_cneg_stubs"

  external to_bytes : Bytes.t -> t -> int = "caml_blst_fp2_to_bytes_stubs"

  external of_bytes_components : t -> Bytes.t -> Bytes.t -> int
    = "caml_blst_fp2_of_bytes_components_stubs"

  external zero : t -> int = "caml_blst_fp2_zero_stubs"

  external one : t -> int = "caml_blst_fp2_one_stubs"
end

module Fq2 = struct
  exception Not_in_field of Bytes.t

  type t = Stubs.t

  let size_in_bytes = 96

  let of_bytes_opt bs =
    if Bytes.length bs <> size_in_bytes then None
    else
      let buffer = Stubs.allocate_fp2 () in
      let x_bytes = Bytes.sub bs 0 48 in
      let y_bytes = Bytes.sub bs 48 48 in
      ignore @@ Stubs.of_bytes_components buffer x_bytes y_bytes ;
      Some buffer

  let of_bytes_exn bs : t =
    let buffer_opt = of_bytes_opt bs in
    match buffer_opt with
    | None -> raise (Not_in_field bs)
    | Some buffer -> buffer

  let zero =
    let buffer = Stubs.allocate_fp2 () in
    ignore @@ Stubs.zero buffer ;
    buffer

  let one =
    let buffer = Stubs.allocate_fp2 () in
    ignore @@ Stubs.one buffer ;
    buffer

  let to_bytes p =
    let buffer = Bytes.make size_in_bytes '\000' in
    ignore @@ Stubs.to_bytes buffer p ;
    buffer

  let random ?state () =
    let x = Fq.random ?state () in
    let y = Fq.random ?state () in
    let buffer = Stubs.allocate_fp2 () in
    let x_bytes = Fq.to_bytes x in
    let y_bytes = Fq.to_bytes y in
    ignore @@ Stubs.of_bytes_components buffer x_bytes y_bytes ;
    buffer

  let add x y =
    let buffer = Stubs.allocate_fp2 () in
    ignore @@ Stubs.add buffer x y ;
    buffer

  let ( + ) = add

  let mul x y =
    let buffer = Stubs.allocate_fp2 () in
    ignore @@ Stubs.mul buffer x y ;
    buffer

  let ( * ) = mul

  let sqrt_opt x =
    let buffer = Stubs.allocate_fp2 () in
    let res = Stubs.sqrt buffer x in
    if res then Some buffer else None

  let negate x =
    let buffer = Stubs.allocate_fp2 () in
    ignore @@ Stubs.cneg buffer x true ;
    buffer
end

include Fq2
