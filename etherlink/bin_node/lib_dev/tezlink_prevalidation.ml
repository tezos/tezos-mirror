(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports.Alpha_context

let ( let** ) v f =
  let open Lwt_result_syntax in
  let* r = v in
  match r with Ok v -> f v | Error err -> return (Error err)

(** Information required to validate an operation. Some will change while folding
   on the batch. We'll need :
       - the [source]
       - the [next_counter] to check the operation of each counter are in order.
         This value will increase by one for each operation.
       - the [balance_left] to check fees can be payed. This value will decrease
         by the amount of fees of each operation.
*)
type batch_validation_context = unit

let validate_operation_in_batch ~(ctxt : batch_validation_context)
    (Contents operation : packed_contents) =
  let open Lwt_result_syntax in
  ignore operation ;
  (* TODO check supported by tezlink *)
  (* TODO check source *)
  (* TODO check counter is ok *)
  (* TODO check gas limit high enough *)
  (* TODO check only one reveal, at the start *)
  (* TODO check manager solvent for fees *)
  (* the update will be updated during the validation steps *)
  return (Ok ctxt)

let rec validate_batch ~(ctxt : batch_validation_context)
    (rest : packed_contents list) =
  let open Lwt_result_syntax in
  match rest with
  | [] -> return (Ok ctxt)
  | c :: rest ->
      let** ctxt = validate_operation_in_batch ~ctxt c in
      (validate_batch [@ocaml.tailcall]) ~ctxt rest

let validate_tezlink_operation ~read raw =
  let open Tezlink_imports.Alpha_context in
  let open Lwt_result_syntax in
  ignore read ;
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
  let first, rest =
    match contents with
    | Single only_operation -> (Contents only_operation, [])
    | Cons (first_operation, rest) ->
        (Contents first_operation, Operation.to_list (Contents_list rest))
  in
  (* TODO validate pk using first *)
  (* TODO build validation context *)
  let** _ = validate_batch ~ctxt:() (first :: rest) in
  (* TODO check signature *)
  ignore signature ;
  return (Ok operation)
