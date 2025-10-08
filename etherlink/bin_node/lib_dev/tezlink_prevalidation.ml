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

let validate_manager_info ~read (Contents op : packed_contents) =
  let open Lwt_result_syntax in
  match op with
  | Manager_operation {source; operation; _} -> (
      let contract = Tezos_types.Contract.of_implicit source in
      let* manager = Tezlink_durable_storage.manager read contract in
      match (manager, operation) with
      | Some (Public_key _), Reveal _ -> failwith "TODO already revealed"
      | Some (Public_key pk), _op -> return (Ok (pk, source))
      | Some (Hash _), Reveal {public_key; _} ->
          (* the revealed public key might be the one we're searching for *)
          let open Signature in
          let pkh_revealed = Public_key.hash public_key in
          if Public_key_hash.equal source pkh_revealed then
            return @@ Ok (public_key, source)
          else failwith "TODO not revealed"
      | Some (Hash _), _op -> failwith "TODO not revealed"
      | None, _ -> failwith "TODO unknown source")
  | _ -> failwith "TODO not a manager operation"

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
  let** pk, source = validate_manager_info ~read first in
  ignore source ;
  (* TODO build validation context *)
  let** _ = validate_batch ~ctxt:() (first :: rest) in
  (* TODO check signature *)
  ignore pk ;
  ignore signature ;
  return (Ok operation)
