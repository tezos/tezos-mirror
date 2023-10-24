(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*****************************************************************************)

type signer = {
  alias : string;
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Client_keys.sk_uri;
}

let get_signer cctxt pkh =
  let open Lwt_result_syntax in
  let* alias, pk, sk = Client_keys.get_key cctxt pkh in
  return {alias; pkh; pk; sk}

type tez = {mutez : int64}

type fee_parameter = {
  minimal_fees : tez;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : tez;
  burn_cap : tez;
}

(* Encoding for Tez amounts, replicated from mempool. *)
let tez_encoding =
  let open Data_encoding in
  let decode {mutez} = Z.of_int64 mutez in
  let encode = Json.wrap_error (fun i -> {mutez = Z.to_int64 i}) in
  Data_encoding.def
    "mutez"
    ~title:"A millionth of a tez"
    ~description:"One million mutez make a tez (1 tez = 1e6 mutez)"
    (conv decode encode n)

(* Encoding for nano-Tez amounts, replicated from mempool. *)
let nanotez_encoding =
  let open Data_encoding in
  def
    "nanotez"
    ~title:"A thousandth of a mutez"
    ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
    (conv
       (fun q -> (q.Q.num, q.Q.den))
       (fun (num, den) -> {Q.num; den})
       (tup2 z z))

let fee_parameter_encoding ~(default_fee_parameter : fee_parameter) =
  let open Data_encoding in
  conv
    (fun {
           minimal_fees;
           minimal_nanotez_per_byte;
           minimal_nanotez_per_gas_unit;
           force_low_fee;
           fee_cap;
           burn_cap;
         } ->
      ( minimal_fees,
        minimal_nanotez_per_byte,
        minimal_nanotez_per_gas_unit,
        force_low_fee,
        fee_cap,
        burn_cap ))
    (fun ( minimal_fees,
           minimal_nanotez_per_byte,
           minimal_nanotez_per_gas_unit,
           force_low_fee,
           fee_cap,
           burn_cap ) ->
      {
        minimal_fees;
        minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit;
        force_low_fee;
        fee_cap;
        burn_cap;
      })
    (obj6
       (dft
          "minimal-fees"
          ~description:"Exclude operations with lower fees"
          tez_encoding
          default_fee_parameter.minimal_fees)
       (dft
          "minimal-nanotez-per-byte"
          ~description:"Exclude operations with lower fees per byte"
          nanotez_encoding
          default_fee_parameter.minimal_nanotez_per_byte)
       (dft
          "minimal-nanotez-per-gas-unit"
          ~description:"Exclude operations with lower gas fees"
          nanotez_encoding
          default_fee_parameter.minimal_nanotez_per_gas_unit)
       (dft
          "force-low-fee"
          ~description:
            "Don't check that the fee is lower than the estimated default"
          bool
          default_fee_parameter.force_low_fee)
       (dft
          "fee-cap"
          ~description:"The fee cap"
          tez_encoding
          default_fee_parameter.fee_cap)
       (dft
          "burn-cap"
          ~description:"The burn cap"
          tez_encoding
          default_fee_parameter.burn_cap))
