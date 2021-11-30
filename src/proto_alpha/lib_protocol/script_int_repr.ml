(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type n = Natural_tag

type z = Integer_tag

(* We could define `num` as a GADT with constructors for `n` and `z`.
  This would enable factorizing the code a bit in the Michelson interpreter and
  also make formal the claim that `num` is only instantiated with `n` and `z`,
  but it would result in space and time overheads when manipulating `num`s, by
  having to deconstruct to and reconstruct from `Z.t`. *)
type 't num = Z.t

let compare x y = Z.compare x y

let zero = Z.zero

let zero_n = Z.zero

let one_n = Z.one

let to_string x = Z.to_string x

let of_string s = Option.catch (fun () -> Z.of_string s)

let of_int32 n = Z.of_int64 @@ Int64.of_int32 n

let to_int64 x = Option.catch (fun () -> Z.to_int64 x)

let of_int64 n = Z.of_int64 n

let to_int x = Option.catch (fun () -> Z.to_int x)

let of_int n = Z.of_int n

let of_zint x = x

let to_zint x = x

let add x y = Z.add x y

let sub x y = Z.sub x y

let mul x y = Z.mul x y

let ediv x y = Option.catch (fun () -> Z.ediv_rem x y)

let add_n = add

let succ_n = Z.succ

let mul_n = mul

let ediv_n = ediv

let abs x = Z.abs x

let is_nat x = if Compare.Z.(x < Z.zero) then None else Some x

let neg x = Z.neg x

let int x = x

let shift_left x y =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then None
  else
    let y = Z.to_int y in
    Some (Z.shift_left x y)

let shift_right x y =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then None
  else
    let y = Z.to_int y in
    Some (Z.shift_right x y)

let shift_left_n = shift_left

let shift_right_n = shift_right

let logor x y = Z.logor x y

let logxor x y = Z.logxor x y

let logand x y = Z.logand x y

let lognot x = Z.lognot x

let z_encoding : z num Data_encoding.encoding = Data_encoding.z

let n_encoding : n num Data_encoding.encoding = Data_encoding.n
