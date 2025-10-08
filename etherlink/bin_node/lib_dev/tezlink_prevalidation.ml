(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports
open Tezlink_imports.Alpha_context

let ( let** ) v f =
  let open Lwt_result_syntax in
  let* r = v in
  match r with Ok v -> f v | Error err -> return (Error err)

type clue = Operation_hash.t

let pp_clue = Operation_hash.pp

let clue_encoding = Operation_hash.encoding

let tzfail_p e = Lwt_result_syntax.tzfail @@ Imported_env.Ecoproto_error e

type error += Not_a_manager_operation of clue

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.not_a_manager_operation"
    ~title:
      "Failed to prevalidate an operation: only manager operation are allowed"
    ~description:
      "Prevalidation of the operation failed because it was not recognized as \
       a manager operation."
    ~pp:(fun ppf raw ->
      Format.fprintf
        ppf
        "Failed to prevaliate an operation: not a manager_operation %a"
        pp_clue
        raw)
    Data_encoding.(obj1 (req "operation" clue_encoding))
    (function Not_a_manager_operation raw -> Some raw | _ -> None)
    (fun raw -> Not_a_manager_operation raw)

let validate_manager_info ~read ~error_clue (Contents op : packed_contents) =
  let open Lwt_result_syntax in
  match op with
  | Manager_operation {source; operation; _} -> (
      let contract = Tezos_types.Contract.of_implicit source in
      let* manager = Tezlink_durable_storage.manager read contract in
      match (manager, operation) with
      | Some (Public_key _), Reveal _ ->
          tzfail_p
          @@ Imported_protocol.Contract_manager_storage.Previously_revealed_key
               (Implicit source)
      | Some (Public_key pk), _op -> return (Ok (pk, source))
      | Some (Hash _), Reveal {public_key; _} ->
          (* the revealed public key might be the one we're searching for *)
          let open Signature in
          let pkh_revealed = Public_key.hash public_key in
          if Public_key_hash.equal source pkh_revealed then
            return @@ Ok (public_key, source)
          else
            tzfail_p
            @@ Imported_protocol.Contract_manager_storage.(
                 Inconsistent_hash
                   {
                     public_key;
                     expected_hash = source;
                     provided_hash = pkh_revealed;
                   })
      | Some (Hash _), _op ->
          tzfail_p
          @@ Imported_protocol.Contract_manager_storage.Unrevealed_manager_key
               (Implicit source)
      | None, _ ->
          tzfail_p
          @@ Imported_protocol.Contract_storage.Empty_implicit_contract source)
  | _ -> tzfail @@ Not_a_manager_operation error_clue

(** Information required to validate an operation. Some will change while folding
   on the batch. We'll need :
       - the [source]
       - the [next_counter] to check the operation of each counter are in order.
         This value will increase by one for each operation.
       - the [balance_left] to check fees can be payed. This value will decrease
         by the amount of fees of each operation.
*)
type batch_validation_context = {
  source : public_key_hash;
  balance_left : Tez.t;
  next_counter : Manager_counter.t;
  error_clue : clue;
}

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
  return (Ok {ctxt with next_counter = Manager_counter.succ ctxt.next_counter})

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
  (* TODO check size *)
  let raw = Bytes.of_string raw in
  let error_clue = Operation_hash.hash_bytes [raw] in
  (* Operation deserialization. To avoid breakage during the prevalidation
       implementation we use the `Operation.decode` helper but we'll need to
       simplify it when the validation can return all the necessary information. *)
  (* TODO: simplify decoding, at the moment it does some verification as
       a side-effect *)
  let*? (operation : Tezos_types.Operation.t) =
    raw |> Tezos_types.Operation.decode
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
  let** pk, source = validate_manager_info ~error_clue ~read first in
  let contract_of = Tezos_types.Contract.of_implicit in
  let* counter = Tezlink_durable_storage.counter read @@ contract_of source in
  (* We convert the counter to the native counter type, to limit the number of
     such conversion. We use an internal function to avoid going through
     serialization to convert. *)
  let counter = Manager_counter.Internal_for_tests.of_int @@ Z.to_int counter in
  let* balance_left =
    Tezlink_durable_storage.balance read @@ contract_of source
  in
  let** _ =
    validate_batch
      ~ctxt:
        {
          source;
          balance_left;
          next_counter = Manager_counter.succ counter;
          error_clue;
        }
      (first :: rest)
  in
  (* TODO check signature *)
  ignore pk ;
  ignore signature ;
  return (Ok operation)
