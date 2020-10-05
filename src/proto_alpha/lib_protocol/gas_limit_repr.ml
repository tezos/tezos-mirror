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

let scaling_factor = 1000

let decimals = 3

module Arith = Fixed_point_repr.Make (struct
  let decimals = decimals
end)

type t = Unaccounted | Limited of {remaining : Arith.fp}

type cost = Z.t

let encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Limited"
        Arith.z_fp_encoding
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
      Format.fprintf ppf "%a units remaining" Arith.pp remaining

let cost_encoding = Data_encoding.z

let pp_cost fmt z = Z.pp_print fmt z

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

let allocation_weight = Z.of_int (scaling_factor * 2)

let step_weight = Z.of_int scaling_factor

let read_base_weight = Z.of_int (scaling_factor * 100)

let write_base_weight = Z.of_int (scaling_factor * 160)

let byte_read_weight = Z.of_int (scaling_factor * 10)

let byte_written_weight = Z.of_int (scaling_factor * 15)

let cost_to_milligas (cost : cost) : Arith.fp = Arith.unsafe_fp cost

let raw_consume block_gas operation_gas cost =
  match operation_gas with
  | Unaccounted ->
      ok (block_gas, Unaccounted)
  | Limited {remaining} ->
      let gas = cost_to_milligas cost in
      if Arith.(gas > zero) then
        let remaining = Arith.sub remaining gas in
        let block_remaining = Arith.sub block_gas gas in
        if Arith.(remaining < zero) then error Operation_quota_exceeded
        else if Arith.(block_remaining < zero) then error Block_quota_exceeded
        else ok (block_remaining, Limited {remaining})
      else ok (block_gas, operation_gas)

let raw_check_enough block_gas operation_gas cost =
  raw_consume block_gas operation_gas cost
  >|? fun (_block_remaining, _remaining) -> ()

let alloc_cost n = Z.mul allocation_weight (Z.succ n)

let alloc_bytes_cost n = alloc_cost (Z.of_int ((n + 7) / 8))

let atomic_step_cost n = n

let step_cost n = Z.mul step_weight n

let free = Z.zero

let read_bytes_cost n = Z.add read_base_weight (Z.mul byte_read_weight n)

let write_bytes_cost n = Z.add write_base_weight (Z.mul byte_written_weight n)

let ( +@ ) x y = Z.add x y

let ( *@ ) x y = Z.mul x y

let alloc_mbytes_cost n = alloc_cost (Z.of_int 12) +@ alloc_bytes_cost n

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
