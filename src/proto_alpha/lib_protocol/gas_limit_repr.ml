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

let decimals = 3

type fp_tag

type integral_tag

let safe_const x =
  match Saturation_repr.(Option.bind (of_int_opt x) mul_safe) with
  | None ->
      (* because [safe_const] is always called with small enough constants. *)
      assert false
  | Some x ->
      x

let safe_int x =
  match Saturation_repr.(of_int_opt x) with
  | None ->
      Saturation_repr.saturated
  | Some x ->
      x

let scaling_factor = safe_const 1000

module Arith = struct
  type 'a t = Saturation_repr.may_saturate Saturation_repr.t

  type fp = fp_tag t

  type integral = integral_tag t

  let scaling_factor = scaling_factor

  let sub = Saturation_repr.sub

  let add = Saturation_repr.add

  let zero = Saturation_repr.(may_saturate zero)

  let min = Saturation_repr.min

  let max = Saturation_repr.max

  let compare = Saturation_repr.compare

  let ( < ) = Saturation_repr.( < )

  let ( <> ) = Saturation_repr.( <> )

  let ( > ) = Saturation_repr.( > )

  let ( <= ) = Saturation_repr.( <= )

  let ( >= ) = Saturation_repr.( >= )

  let ( = ) = Saturation_repr.( = )

  let equal = Saturation_repr.equal

  let of_int_opt = Saturation_repr.of_int_opt

  let fatally_saturated_int i =
    failwith (string_of_int i ^ " should not be saturated.")

  let fatally_saturated_z z =
    failwith (Z.to_string z ^ " should not be saturated.")

  let integral_of_int_exn i =
    Saturation_repr.(
      match of_int_opt i with
      | None ->
          fatally_saturated_int i
      | Some i' ->
          let r = scale_fast scaling_factor i' in
          if r = saturated then fatally_saturated_int i else r)

  let integral_exn z =
    match Z.to_int z with
    | i ->
        integral_of_int_exn i
    | exception Z.Overflow ->
        fatally_saturated_z z

  let integral_to_z (i : integral) : Z.t =
    Saturation_repr.(to_z (ediv i scaling_factor))

  let ceil x =
    let r = Saturation_repr.erem x scaling_factor in
    if r = zero then x else add x (sub scaling_factor r)

  let floor x = sub x (Saturation_repr.erem x scaling_factor)

  let fp x = x

  let pp fmtr fp =
    let q = Saturation_repr.(ediv fp scaling_factor |> to_int) in
    let r = Saturation_repr.(erem fp scaling_factor |> to_int) in
    if Compare.Int.(r = 0) then Format.fprintf fmtr "%d" q
    else Format.fprintf fmtr "%d.%0*d" q decimals r

  let pp_integral = pp

  let n_fp_encoding : fp Data_encoding.t = Saturation_repr.n_encoding

  let z_fp_encoding : fp Data_encoding.t = Saturation_repr.z_encoding

  let n_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral_exn Data_encoding.n

  let z_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral_exn Data_encoding.z

  let unsafe_fp x =
    match of_int_opt (Z.to_int x) with
    | Some int ->
        int
    | None ->
        fatally_saturated_z x

  let sub_opt = Saturation_repr.sub_opt
end

type t = Unaccounted | Limited of {remaining : Arith.fp}

module S = Saturation_repr

type cost = S.may_saturate S.t

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

let cost_encoding = S.z_encoding

let pp_cost fmt z = S.pp fmt z

let allocation_weight = S.(mul_fast scaling_factor (safe_const 2))

let step_weight = scaling_factor

let read_base_weight = S.(mul_fast scaling_factor (safe_const 100))

let write_base_weight = S.(mul_fast scaling_factor (safe_const 160))

let byte_read_weight = S.(mul_fast scaling_factor (safe_const 10))

let byte_written_weight = S.(mul_fast scaling_factor (safe_const 15))

let cost_to_milligas (cost : cost) : Arith.fp = cost

let raw_consume gas_counter cost =
  let gas = cost_to_milligas cost in
  Arith.sub_opt gas_counter gas

let alloc_cost n = S.mul allocation_weight S.(add n (safe_const 1))

let alloc_bytes_cost n =
  match S.of_int_opt ((n + 7) / 8) with
  | None ->
      (* Since [n] is supposed to be positive, the following case should
         never occur. In case this assumption does not hold, we return
         [saturated], which is always a safe cost. *)
      S.saturated
  | Some x ->
      alloc_cost x

let atomic_step_cost : 'a S.t -> cost = S.may_saturate

let step_cost n = S.mul step_weight n

let free = S.zero |> S.may_saturate

let read_bytes_cost n =
  S.add read_base_weight (S.mul byte_read_weight (safe_int n))

let write_bytes_cost n =
  S.add write_base_weight (S.mul byte_written_weight (safe_int n))

let ( +@ ) x y = S.add x y

let ( *@ ) x y = S.mul x y

let alloc_mbytes_cost n = alloc_cost (safe_const 12) +@ alloc_bytes_cost n
