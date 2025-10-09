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

type error +=
  | Parsing_failure of clue * Data_encoding.Binary.read_error
  | Not_a_manager_operation of clue
  | Unsupported_manager_operation of clue

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.parsing_failure"
    ~title:"Failed to prevalidate an operation: could not parse the operation"
    ~description:"Prevalidation of the operation failed during parsing."
    ~pp:(fun ppf (clue, err) ->
      Format.fprintf
        ppf
        "Failed to prevaliate operation %a: error during parsing %a"
        pp_clue
        clue
        Data_encoding.Binary.pp_read_error
        err)
    Data_encoding.(
      obj2
        (req "operation" clue_encoding)
        (req "error" Data_encoding.Binary.read_error_encoding))
    (function Parsing_failure (clue, err) -> Some (clue, err) | _ -> None)
    (fun (clue, err) -> Parsing_failure (clue, err)) ;
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
    (fun raw -> Not_a_manager_operation raw) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.unsupported_manager_operation"
    ~title:"Failed to prevalidate an operation: unsupported manager operation."
    ~description:
      "Prevalidation of the operation failed because it contains at least one \
       unsupported operation. The only supported operations are reveal, \
       transaction and origination."
    ~pp:(fun ppf op ->
      Format.fprintf
        ppf
        "Failed to prevalidate an operation: it contains at least one \
         unsupported operation %a"
        pp_clue
        op)
    Data_encoding.(obj1 (req "operation" clue_encoding))
    (function Unsupported_manager_operation op -> Some op | _ -> None)
    (fun op -> Unsupported_manager_operation op)

let validate_manager_info ~read ~error_clue (Contents op : packed_contents) =
  let open Lwt_result_syntax in
  match op with
  | Manager_operation {source; operation; counter; _} -> (
      let contract = Tezos_types.Contract.of_implicit source in
      let* manager = Tezlink_durable_storage.manager read contract in
      match (manager, operation) with
      | Some (Public_key _), Reveal _ ->
          tzfail_p
          @@ Imported_protocol.Contract_manager_storage.Previously_revealed_key
               (Implicit source)
      | Some (Public_key pk), _op -> return (Ok (pk, source, counter))
      | Some (Hash _), Reveal {public_key; _} ->
          (* the revealed public key might be the one we're searching for *)
          let open Signature in
          let pkh_revealed = Public_key.hash public_key in
          if Public_key_hash.equal source pkh_revealed then
            return @@ Ok (public_key, source, counter)
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
    To build the object Operation.t to include in the tx_queue we'll need:
       - the [source]
       - the [first_counter]
       - the [length] of the batch
*)
type batch_validation_context = {
  source : public_key_hash;
  balance_left : Tez.t;
  next_counter : Manager_counter.t;
      (* invariant: next_counter = first_counter + length *)
  error_clue : clue;
  first_counter : Manager_counter.t;
  length : int;
}

let validate_supported_operation (type kind) ~ctxt
    (operation : kind manager_operation) =
  let open Lwt_result_syntax in
  match operation with
  | Reveal _ | Transaction _ | Origination _ -> return (Ok ())
  | _ -> tzfail @@ Unsupported_manager_operation ctxt.error_clue

let validate_source ~ctxt second_source =
  let open Lwt_result_syntax in
  if Signature.Public_key_hash.equal ctxt.source second_source then
    return (Ok ())
  else
    tzfail_p
    @@ Imported_protocol.Validate_errors.Manager.(
         Inconsistent_sources
           {expected_source = ctxt.source; source = second_source})

let validate_operation_in_batch ~(ctxt : batch_validation_context)
    (Contents operation : packed_contents) =
  let open Lwt_result_syntax in
  match operation with
  | Manager_operation {operation = Reveal _; _} when not (ctxt.length = 0) ->
      tzfail_p
      @@ Imported_protocol.Validate_errors.Manager.Incorrect_reveal_position
  | Manager_operation
      {
        source;
        fee = _;
        counter = _;
        operation;
        gas_limit = _;
        storage_limit = _;
      } ->
      let** () = validate_supported_operation ~ctxt operation in
      let** () = validate_source ~ctxt source in
      (* TODO check counter is ok *)
      (* TODO check gas limit high enough *)
      (* TODO check manager solvent for fees *)
      (* the update will be updated during the validation steps *)
      return
        (Ok
           {
             ctxt with
             next_counter = Manager_counter.succ ctxt.next_counter;
             length = ctxt.length + 1;
           })
  | _ -> tzfail @@ Not_a_manager_operation ctxt.error_clue

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
  let* op =
    match
      Data_encoding.Binary.of_bytes
        Tezlink_imports.Alpha_context.Operation.encoding
        raw
    with
    | Error e -> tzfail @@ Parsing_failure (error_clue, e)
    | Ok op -> return op
  in
  let {protocol_data = Operation_data {contents; signature}; _} = op in
  let first, rest =
    match contents with
    | Single only_operation -> (Contents only_operation, [])
    | Cons (first_operation, rest) ->
        (Contents first_operation, Operation.to_list (Contents_list rest))
  in
  let** pk, source, first_counter =
    validate_manager_info ~read ~error_clue first
  in
  let* balance_left =
    Tezlink_durable_storage.balance read
    @@ Tezos_types.Contract.of_implicit source
  in
  let** ctxt =
    validate_batch
      ~ctxt:
        {
          source;
          balance_left;
          next_counter = first_counter;
          error_clue;
          first_counter;
          length = 0;
        }
      (first :: rest)
  in
  (* TODO check signature *)
  let*? first_counter = Tezos_types.Operation.counter_to_z ctxt.first_counter in
  let operation =
    Tezos_types.Operation.
      {length = ctxt.length; source = ctxt.source; raw; op; first_counter}
  in
  ignore pk ;
  ignore signature ;
  return (Ok operation)
