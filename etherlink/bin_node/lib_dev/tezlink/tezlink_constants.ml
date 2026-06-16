(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezlink_imports

type t = Imported_context.Constants.t

let parametric_repr : Imported_protocol.Constants_parametric_repr.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"parametric_constants"
      ~dst:Imported_protocol.Constants_parametric_repr.encoding
      ~src:Imported_context.Constants.Parametric.encoding
      Imported_protocol_parameters.Default_parameters.constants_mainnet
  with
  | Ok param -> param
  | Error _ -> assert false

let parametric_repr =
  {
    parametric_repr with
    (* This is a small patch to trick tzkt into indexing asap. We can't do
       less than 1sec though, as [Period_repr] is in seconds. *)
    minimal_block_delay = Imported_protocol.Period_repr.one_second;
    (* Raised above the L1 mainnet default (1_040_000) so that a Tezos
       operation can absorb the gas budget propagated from a cross-runtime
       call originated on the EVM side, where the per-transaction cap is
       30_000_000 EVM gas (= 3_000_000_000 milligas, i.e. 3_000_000 gas
       units). Without this, [runtime_gateway] forwards
       [remaining_evm_gas * 100] as [X-Tezos-Gas-Limit] and the Michelson
       runtime rejects the request as soon as the EVM caller has more than
       ~10.4M gas left. *)
    hard_gas_limit_per_operation =
      Imported_protocol.Gas_limit_repr.Arith.integral_of_int_exn 3_000_000;
    (* Kept equal to [hard_gas_limit_per_operation] for now even though
       there is no architectural reason for the per-block cap to track
       the per-op cap once the per-op cap has been decoupled from L1
       mainnet. Making it effectively unbounded would silence the
       prevalidation batch-sum check at [tezlink_prevalidation.ml] and
       drop the assertion exercised by [michelson_runtime.ml] "Test
       michelson runtime validation of block gas limit", which is built
       on [per_block == per_op]. To be revisited together with that test
       in a follow-up. *)
    hard_gas_limit_per_block =
      Imported_protocol.Gas_limit_repr.Arith.integral_of_int_exn 3_000_000;
  }

let all_constants_repr : Imported_protocol.Constants_repr.t =
  Imported_protocol.Constants_repr.all_of_parametric parametric_repr

let all_constants : Imported_context.Constants.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"all_constants"
      ~dst:Imported_context.Constants.encoding
      ~src:Imported_protocol.Constants_repr.encoding
      all_constants_repr
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
