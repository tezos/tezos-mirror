(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports.Alpha_context

(* WIP just do decoding for now, to maintain current working conditions *)
let validate_tezlink_operation ~read raw =
  let open Lwt_result_syntax in
  (* TODO check size *)
  (* Operation deserialization. To avoid breakage during the prevalidation
       implementation we use the `Operation.decode` helper but we'll need to
       simplify it when the validation can return all the necessary information. *)
  (* TODO: simplify decoding, at the moment it does some verification as
       a side-effect *)
  let*? (operation : Tezos_types.Operation.t) =
    raw |> Bytes.of_string |> Tezos_types.Operation.decode
  in
  let {protocol_data = Operation_data {contents; signature}; _} =
    operation.op
  in
  ignore contents ;
  ignore read ;
  (* TODO check pk (potentially look if first operation is a reveal) *)
  (* - if not revealed, check the first operation for reveal *)
  (* - TODO check pk against pkh *)
  (* TODO check the batch: *)
  (* - TODO check supported by tezlink *)
  (* - TODO check source *)
  (* - TODO check counter is ok *)
  (* - TODO check gas limit high enough *)
  (* - TODO check only one reveal, at the start *)
  (* - TODO check manager solvent for fees *)
  (* - TODO check signature *)
  ignore signature ;
  return (Ok operation)
