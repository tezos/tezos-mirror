(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezlink_imports

type t = Imported_context.Constants.t

(* Kept equal to [hard_gas_limit_per_operation] (also 660k) so the validation
   invariant exercised by [michelson_runtime.ml] "Test michelson runtime
   validation of block gas limit" holds. Sandbox callers can override via the
   optional [?hard_gas_limit_per_block] argument; production paths leave it at
   None and observe the default. *)
let default_hard_gas_limit_per_block = 660_000

let base_parametric_repr : Imported_protocol.Constants_parametric_repr.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"parametric_constants"
      ~dst:Imported_protocol.Constants_parametric_repr.encoding
      ~src:Imported_context.Constants.Parametric.encoding
      Imported_protocol_parameters.Default_parameters.constants_mainnet
  with
  | Ok param -> param
  | Error _ -> assert false

let parametric_repr ?hard_gas_limit_per_block () :
    Imported_protocol.Constants_parametric_repr.t =
  let hard_gas_limit_per_block =
    Option.value
      hard_gas_limit_per_block
      ~default:default_hard_gas_limit_per_block
  in
  {
    base_parametric_repr with
    (* This is a small patch to trick tzkt into indexing asap. We can't do
       less than 1sec though, as [Period_repr] is in seconds. *)
    minimal_block_delay = Imported_protocol.Period_repr.one_second;
    (* Defined to support constraints of cross-runtime calls from the EVM
       runtime, where the per-transaction cap is 30_000_000 EVM gas.
       With an EVM gas to Michelson milligas conversion set to 22 in the kernel,
       this leads to a 30_000_000 * 22 / 1000 = 660_000 gas limit in the
       Michelson runtime. *)
    hard_gas_limit_per_operation =
      Imported_protocol.Gas_limit_repr.Arith.integral_of_int_exn 660_000;
    hard_gas_limit_per_block =
      Imported_protocol.Gas_limit_repr.Arith.integral_of_int_exn
        hard_gas_limit_per_block;
  }

let all_constants_repr ?hard_gas_limit_per_block () :
    Imported_protocol.Constants_repr.t =
  Imported_protocol.Constants_repr.all_of_parametric
    (parametric_repr ?hard_gas_limit_per_block ())

let all_constants ?hard_gas_limit_per_block () : Imported_context.Constants.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"all_constants"
      ~dst:Imported_context.Constants.encoding
      ~src:Imported_protocol.Constants_repr.encoding
      (all_constants_repr ?hard_gas_limit_per_block ())
  with
  | Ok param -> param
  | Error _ -> assert false

(* Construct a protocol-compatible fitness. Locked_round is mocked to None. *)
let fitness ~level ~predecessor_round ~round : Fitness.t =
  let level = Imported_protocol.Raw_level_repr.of_int32_exn level in
  Imported_protocol.Fitness_repr.to_raw
    (Imported_protocol.Fitness_repr.create_without_locked_round
       ~level
       ~predecessor_round
       ~round)
