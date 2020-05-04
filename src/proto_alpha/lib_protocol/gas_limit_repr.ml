(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = Unaccounted | Limited of {remaining : Z.t}

type cost = Z.t

let encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Limited"
        z
        (function Limited {remaining} -> Some remaining | _ -> None)
        (fun remaining -> Limited {remaining});
      case
        (Tag 1)
        ~title:"Unaccounted"
        (constant "unaccounted")
        (function Unaccounted -> Some () | _ -> None)
        (fun () -> Unaccounted) ]

let pp ppf = function
  | Unaccounted ->
      Format.fprintf ppf "unaccounted"
  | Limited {remaining} ->
      Format.fprintf ppf "%a units remaining" Z.pp remaining

let cost_encoding = Data_encoding.z

let pp_cost = Z.pp

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

let rescaling_bits = 7

let scale (z : Z.t) = Z.shift_left z rescaling_bits

let allocation_weight = scale (Z.of_int 2)

let atomic_step_weight = Z.of_int 1

let step_weight = scale (Z.of_int 1)

let read_base_weight = scale (Z.of_int 100)

let write_base_weight = scale (Z.of_int 160)

let byte_read_weight = scale (Z.of_int 10)

let byte_written_weight = scale (Z.of_int 15)

let cost_to_gas (cost : cost) : Z.t = cost

let consume block_gas operation_gas cost =
  match operation_gas with
  | Unaccounted ->
      ok (block_gas, Unaccounted)
  | Limited {remaining} ->
      let gas = cost_to_gas cost in
      if Compare.Z.(gas > Z.zero) then
        let remaining = Z.sub remaining gas in
        let block_remaining = Z.sub block_gas gas in
        if Compare.Z.(remaining < Z.zero) then error Operation_quota_exceeded
        else if Compare.Z.(block_remaining < Z.zero) then
          error Block_quota_exceeded
        else ok (block_remaining, Limited {remaining})
      else ok (block_gas, operation_gas)

let check_enough block_gas operation_gas cost =
  consume block_gas operation_gas cost
  >|? fun (_block_remaining, _remaining) -> ()

let alloc_cost n = Z.mul allocation_weight (Z.of_int (n + 1))

let alloc_bytes_cost n = alloc_cost ((n + 7) / 8)

let alloc_bits_cost n = alloc_cost ((n + 63) / 64)

let atomic_step_cost n = Z.mul atomic_step_weight (Z.of_int (2 * n))

let step_cost n = Z.mul step_weight (Z.of_int n)

let free = Z.zero

let read_bytes_cost n =
  Z.add (Z.mul read_base_weight Z.one) (Z.mul byte_read_weight n)

let write_bytes_cost n =
  Z.add (Z.mul write_base_weight Z.one) (Z.mul byte_written_weight n)

let ( +@ ) x y = Z.add x y

let ( *@ ) x y = Z.mul (Z.of_int x) y

let alloc_mbytes_cost n = alloc_cost 12 +@ alloc_bytes_cost n

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.operation"
    ~title:"Gas quota exceeded for the operation"
    ~description:
      "A script or one of its callee took more time than the operation said \
       it would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.block"
    ~title:"Gas quota exceeded for the block"
    ~description:
      "The sum of gas consumed by all the operations in the block exceeds the \
       hard gas limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded)
