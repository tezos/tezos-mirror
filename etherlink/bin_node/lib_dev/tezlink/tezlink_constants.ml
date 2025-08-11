(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports

type t = Alpha_context.Constants.t

let parametric_repr : Imported_protocol.Constants_parametric_repr.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"parametric_constants"
      ~dst:Imported_protocol.Constants_parametric_repr.encoding
      ~src:Alpha_context.Constants.Parametric.encoding
      Imported_protocol_parameters.Default_parameters.constants_mainnet
  with
  | Ok param -> param
  | Error _ -> assert false

(* This is a small patch to trick tzkt into indexing asap. We can't do less
   than 1sec though, as Period_repr is in seconds. *)
let parametric_repr =
  {
    parametric_repr with
    minimal_block_delay = Imported_protocol.Period_repr.one_second;
  }

let all_constants_repr : Imported_protocol.Constants_repr.t =
  Imported_protocol.Constants_repr.all_of_parametric parametric_repr

let all_constants : Alpha_context.Constants.t =
  match
    Tezos_types.convert_using_serialization
      ~name:"all_constants"
      ~dst:Alpha_context.Constants.encoding
      ~src:Imported_protocol.Constants_repr.encoding
      all_constants_repr
  with
  | Ok param -> param
  | Error _ -> assert false
