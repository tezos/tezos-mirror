(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
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

type t = Z.t

let shift = 18

let wei_pow = String.make shift '0'

let zero = Z.zero

let one = Z.one

let one_eth = Z.of_string @@ "1" ^ wei_pow

let to_string = Z.to_string

let of_string = Z.of_string

let to_wei_z z = z

let of_wei_z z = z

let of_tez z =
  z |> Tez.mutez_int64 |> Z.of_int64 |> Z.mul Z.(pow (of_int 10) 12) |> to_wei_z

let of_eth_int eth = Z.of_string @@ Int.to_string eth ^ wei_pow

let of_eth_string eth =
  match String.split_on_char '.' (String.trim eth) with
  | [eth] -> of_string (eth ^ wei_pow)
  | [eth; decimal] ->
      let decimal = decimal ^ String.make (shift - String.length decimal) '0' in
      Z.add (of_string eth) (of_string decimal)
  | _ -> Test.fail "Invalid ETH amount: %s" eth

let ( + ) = Z.add

let ( - ) = Z.sub

let ( * ) = Z.mul

let ( / ) = Z.div

let cdiv = Z.cdiv

let typ =
  Check.comparable
    (fun fmt t -> Format.fprintf fmt "%s" (to_string t))
    (fun a b -> Z.compare a b)

let to_le_bytes z =
  let buffer = Bytes.make 32 '\000' in
  let bits = Z.to_bits z |> Bytes.of_string in
  Bytes.blit bits 0 buffer 0 (Bytes.length bits) ;
  buffer

let truncate_to_mutez z =
  let open Z in
  div z (pow (of_int 10) 12) |> to_int
