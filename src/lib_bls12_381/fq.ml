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
  type fp

  external allocate_fq : unit -> fp = "allocate_fp_stubs"

  external of_bytes_le : fp -> Bytes.t -> int = "caml_blst_fp_of_bytes_stubs"

  external to_bytes_le : Bytes.t -> fp -> int = "caml_blst_fp_to_bytes_stubs"

  external add : fp -> fp -> fp -> int = "caml_blst_fp_add_stubs"

  external mul : fp -> fp -> fp -> int = "caml_blst_fp_mul_stubs"

  external sqrt : fp -> fp -> bool = "caml_blst_fp_sqrt_stubs"

  external cneg : fp -> fp -> bool -> int = "caml_blst_fp_cneg_stubs"
end

module Fq = struct
  exception Not_in_field of Bytes.t

  type t = Stubs.fp

  let order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"

  let size_in_bytes = 48

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
      let buffer = Stubs.allocate_fq () in
      ignore @@ Stubs.of_bytes_le buffer bs ;
      Some buffer

  let of_bytes_exn bs =
    let buffer_opt = of_bytes_opt bs in
    match buffer_opt with
    | None -> raise (Not_in_field bs)
    | Some buffer -> buffer

  let zero =
    let bytes = Bytes.make size_in_bytes '\000' in
    of_bytes_exn bytes

  let one =
    let bytes = Bytes.make size_in_bytes '\000' in
    Bytes.set bytes 0 '\001' ;
    of_bytes_exn bytes

  let to_bytes x =
    let buffer_bytes = Bytes.make size_in_bytes '\000' in
    ignore @@ Stubs.to_bytes_le buffer_bytes x ;
    buffer_bytes

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

  let add x y =
    let buffer = Stubs.allocate_fq () in
    ignore @@ Stubs.add buffer x y ;
    buffer

  let ( + ) = add

  let mul x y =
    let buffer = Stubs.allocate_fq () in
    ignore @@ Stubs.mul buffer x y ;
    buffer

  let ( * ) = mul

  let negate x =
    let buffer = Stubs.allocate_fq () in
    ignore @@ Stubs.cneg buffer x true ;
    buffer

  let sqrt_opt x =
    let buffer = Stubs.allocate_fq () in
    let res = Stubs.sqrt buffer x in
    if res then Some buffer else None

  let of_z z =
    let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
    let x = Bytes.make size_in_bytes '\000' in
    Bytes.blit z 0 x 0 (min (Bytes.length z) size_in_bytes) ;
    of_bytes_exn x
end

include Fq
