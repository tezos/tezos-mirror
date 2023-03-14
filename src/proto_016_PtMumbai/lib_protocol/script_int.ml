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
type 't repr = Z.t

type 't num = Num_tag of 't repr [@@ocaml.unboxed]

let compare (Num_tag x) (Num_tag y) = Z.compare x y

let zero = Num_tag Z.zero

let one = Num_tag Z.one

let zero_n = Num_tag Z.zero

let one_n = Num_tag Z.one

let to_string (Num_tag x) = Z.to_string x

let of_string s = Option.catch (fun () -> Num_tag (Z.of_string s))

let of_int32 n = Num_tag (Z.of_int64 @@ Int64.of_int32 n)

let to_int64 (Num_tag x) = Option.catch (fun () -> Z.to_int64 x)

let of_int64 n = Num_tag (Z.of_int64 n)

let to_int (Num_tag x) = Option.catch (fun () -> Z.to_int x)

let of_int n = Num_tag (Z.of_int n)

let of_zint x = Num_tag x

let to_zint (Num_tag x) = x

let add (Num_tag x) (Num_tag y) = Num_tag (Z.add x y)

let sub (Num_tag x) (Num_tag y) = Num_tag (Z.sub x y)

let mul (Num_tag x) (Num_tag y) = Num_tag (Z.mul x y)

let ediv (Num_tag x) (Num_tag y) =
  let ediv_tagged x y =
    let quo, rem = Z.ediv_rem x y in
    (Num_tag quo, Num_tag rem)
  in
  Option.catch (fun () -> ediv_tagged x y)

let add_n = add

let succ_n (Num_tag x) = Num_tag (Z.succ x)

let mul_n = mul

let ediv_n = ediv

let abs (Num_tag x) = Num_tag (Z.abs x)

let is_nat (Num_tag x) =
  if Compare.Z.(x < Z.zero) then None else Some (Num_tag x)

let neg (Num_tag x) = Num_tag (Z.neg x)

let int (Num_tag x) = Num_tag x

let shift_left (Num_tag x) (Num_tag y) =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then None
  else
    let y = Z.to_int y in
    Some (Num_tag (Z.shift_left x y))

let shift_right (Num_tag x) (Num_tag y) =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then None
  else
    let y = Z.to_int y in
    Some (Num_tag (Z.shift_right x y))

let shift_left_n = shift_left

let shift_right_n = shift_right

let logor (Num_tag x) (Num_tag y) = Num_tag (Z.logor x y)

let logxor (Num_tag x) (Num_tag y) = Num_tag (Z.logxor x y)

let logand (Num_tag x) (Num_tag y) = Num_tag (Z.logand x y)

let lognot (Num_tag x) = Num_tag (Z.lognot x)

let z_encoding : z num Data_encoding.encoding =
  Data_encoding.(conv (fun (Num_tag z) -> z) (fun z -> Num_tag z) z)

let n_encoding : n num Data_encoding.encoding =
  Data_encoding.(conv (fun (Num_tag n) -> n) (fun n -> Num_tag n) n)
