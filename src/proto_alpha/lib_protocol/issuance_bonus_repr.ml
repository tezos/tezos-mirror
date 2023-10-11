(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Q.t

type max_bonus = t

(* Bonus will represent a fraction of the total supply, so its precision
   should be in the order of magnitude of the total supply. *)
(* Order of magnitude of the total supply in mutez
   Approximately 2^50 *)
let bonus_unit = Q.of_int64 1_000_000_000_000_000L

let zero = Q.zero

let check_bounds ~max_bonus q = Q.(q >= zero && q <= max_bonus)
  [@@inline always]

type error += Out_of_bounds_bonus

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"out_of_bound_issuance_bonus"
    ~title:"Out of bound issuance bonus"
    ~description:"Computed issuance bonus is out of bound"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Computed issuance bonus is out of bound")
    Data_encoding.unit
    (function Out_of_bounds_bonus -> Some () | _ -> None)
    (fun () -> Out_of_bounds_bonus)

let of_Q ~max_bonus q =
  let open Result_syntax in
  if check_bounds ~max_bonus q then return q else tzfail Out_of_bounds_bonus
  [@@inline always]

let of_Q_unbounded q = if check_bounds ~max_bonus:Q.one q then Some q else None

let of_int64_repr i = of_Q_unbounded Q.(div (of_int64 i) bonus_unit)

let of_int64_repr_err i =
  let open Result_syntax in
  match of_int64_repr i with
  | Some bonus -> return bonus
  | None -> fail "Issuance bonus must be between 0 and 1"

let to_int64_repr q = Q.(mul q bonus_unit |> to_int64)

let encoding =
  Data_encoding.conv_with_guard
    to_int64_repr
    of_int64_repr_err
    Data_encoding.int64

let max_bonus_encoding = encoding

let max_bonus_parameter_of_Q_exn q =
  match of_Q_unbounded q with
  | Some max_bonus -> max_bonus
  | None ->
      failwith "Invalid parameter: max_bonus parameter must be between 0 and 1"
