(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports
open Tezlink_imports.Alpha_context
open Validation_types

let ( let** ) v f =
  let open Lwt_result_syntax in
  let* r = v in
  match r with Ok v -> f v | Error err -> return (Error err)

type clue = Operation_hash.t

let pp_clue = Operation_hash.pp

let clue_encoding = Operation_hash.encoding

(** Fail with a protocol error *)
let tzfail_p e = Lwt_result_syntax.tzfail @@ Imported_env.wrap_tzerror e

type error +=
  | Parsing_failure of clue * Data_encoding.Binary.read_error
  | Not_a_manager_operation of clue
  | Unsupported_manager_operation of clue
  | Oversized_operation of clue * int * int
  | Bls_is_not_allowed of clue * public_key_hash

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.bls_is_not_allowed"
    ~title:"Failed to prevalidate an operation: tz4 are forbidden"
    ~description:
      "Prevalidation of the operation failed because tz4 addresses are not \
       supported."
    ~pp:(fun ppf (clue, key) ->
      Format.fprintf
        ppf
        "Failed to prevaliate operation %a: tz4 keys are forbidden %a"
        pp_clue
        clue
        Signature.Public_key_hash.pp
        key)
    Data_encoding.(
      obj2
        (req "operation" clue_encoding)
        (req "key" Signature.Public_key_hash.encoding))
    (function Bls_is_not_allowed (clue, err) -> Some (clue, err) | _ -> None)
    (fun (clue, err) -> Bls_is_not_allowed (clue, err)) ;
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
    (fun op -> Unsupported_manager_operation op) ;
  register_error_kind
    `Temporary
    ~id:"evm_node.dev.tezlink.too_big"
    ~title:"Failed to prevalidate an operation: too big."
    ~description:
      "Prevalidation of the operation failed because the operation is too big."
    ~pp:(fun ppf (op, size, max) ->
      Format.fprintf
        ppf
        "Failed to prevalidate an operation: oversized operation %a (size: %d, \
         max: %d)"
        pp_clue
        op
        size
        max)
    Data_encoding.(
      obj3 (req "operation" clue_encoding) (req "size" int31) (req "max" int31))
    (function
      | Oversized_operation (clue, size, max) -> Some (clue, size, max)
      | _ -> None)
    (fun (clue, size, max) -> Oversized_operation (clue, size, max))

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
    To build the blueprint we'll need:
       - the total [fee]
*)
type batch_validation_context = {
  source : public_key_hash;
  balance_left : Tez.t;
  previous_counter : Manager_counter.t option;
      (* Used for [Inconsistent_counter] error, so only relevant starting from
         the second operation. *)
  next_counter : Manager_counter.t;
      (* invariant: next_counter = first_counter + length *)
  error_clue : clue;
  first_counter : Manager_counter.t;
  length : int;
  fee : Tez.t;
  gas_limit : Z.t;
  signature_check_cost : Gas.cost;
}

let is_tz4 pkh =
  match
    Imported_protocol.Michelson_v1_gas.Cost_of.Interpreter
    .algo_of_public_key_hash
      pkh
  with
  | Bls -> true
  | _ -> false

let validate_reveal ctxt (Reveal {public_key; _}) =
  let open Lwt_result_syntax in
  match public_key with
  | Bls _ ->
      tzfail
      @@ Bls_is_not_allowed
           (ctxt.error_clue, Signature.Public_key.hash public_key)
  | _ -> return (Ok ())

let validate_transaction ctxt (Transaction {destination; _}) =
  let open Lwt_result_syntax in
  match destination with
  | Implicit destination when is_tz4 destination ->
      tzfail @@ Bls_is_not_allowed (ctxt.error_clue, destination)
  | _ -> return (Ok ())

let validate_supported_operation (type kind) ~ctxt
    (operation : kind manager_operation) =
  let open Lwt_result_syntax in
  match operation with
  | Reveal _ -> validate_reveal ctxt operation
  | Transaction _ -> validate_transaction ctxt operation
  | Origination _ -> return (Ok ())
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

let validate_balance ~ctxt ~fee =
  let open Lwt_result_syntax in
  let tezrep_of t =
    let open Imported_protocol in
    t |> Tez.to_mutez |> Tez_repr.of_mutez
    |> Option.value ~default:Tez_repr.zero
    (* The conversion should not fail so the zero value won't be used *)
  in
  match Tez.sub_opt ctxt.balance_left fee with
  | Some balance_left -> (
      match Tez.(ctxt.fee +? fee) with
      | Ok fee -> return (Ok {ctxt with balance_left; fee})
      | Error _ ->
          tzfail_p
          @@ Imported_protocol.Tez_repr.Addition_overflow
               (tezrep_of ctxt.fee, tezrep_of fee))
  | None ->
      tzfail_p
      @@ Imported_protocol.Contract_storage.(
           Balance_too_low
             (Implicit ctxt.source, tezrep_of ctxt.balance_left, tezrep_of fee))

let validate_counter ~ctxt counter =
  let open Lwt_result_syntax in
  if Manager_counter.equal ctxt.next_counter counter then return (Ok ())
  else
    match ctxt.previous_counter with
    | Some previous_counter ->
        tzfail_p
        @@ Imported_protocol.Validate_errors.Manager.(
             Inconsistent_counters
               {source = ctxt.source; previous_counter; counter})
    | None ->
        (* the first counter in the batch is equal to ctxt.counter by
           construction *)
        failwith "unreachable"

let hard_gas_limit_per_operation =
  Tezos_types.Operation.gas_limit_to_z
    Tezlink_constants.all_constants.parametric.hard_gas_limit_per_operation

let consume_decoding_gas gas_limit expr =
  let open Result_syntax in
  match Script.consume_decoding_gas gas_limit expr with
  | Ok r -> return r
  | Error _ ->
      tzfail
      @@ Imported_env.wrap_tzerror
           Imported_protocol.Validate_errors.Manager
           .Gas_quota_exceeded_init_deserialize

let validate_variable_gas_cost ~ctxt remaining_gas (Manager operation) =
  let open Result_syntax in
  match operation with
  | Transaction {parameters; _} ->
      let* _ = consume_decoding_gas remaining_gas parameters in
      return_unit
  | Origination {script; _} ->
      let* remaining_gas = consume_decoding_gas remaining_gas script.code in
      let* _ = consume_decoding_gas remaining_gas script.storage in
      return_unit
  | Reveal _ ->
      (* only checking a Bls proof has a cost here, and we don't do Bls *)
      return_unit
  | _ -> tzfail @@ Unsupported_manager_operation ctxt.error_clue

let validate_gas_limit ~ctxt overall_gas_limit gas_limit operation =
  let open Lwt_result_syntax in
  if
    not
      Z.Compare.(
        hard_gas_limit_per_operation >= overall_gas_limit
        && overall_gas_limit >= Z.zero)
  then tzfail_p @@ Imported_protocol.Gas_limit_repr.Gas_limit_too_high
  else
    let fixed_cost =
      if ctxt.length = 0 then
        (* The gas cost of signature verification is included in the first
           operation cost *)
        Gas.(
          Imported_protocol.Michelson_v1_gas.Cost_of.manager_operation
          +@ ctxt.signature_check_cost)
      else Imported_protocol.Michelson_v1_gas.Cost_of.manager_operation
    in
    let*? remaining_gas =
      Imported_env.wrap_tzresult
      @@ Gas.consume_from (Gas.Arith.fp gas_limit) fixed_cost
    in
    let*? () = validate_variable_gas_cost ~ctxt remaining_gas operation in
    return (Ok ())

let validate_operation_in_batch ~(ctxt : batch_validation_context)
    (Contents operation : packed_contents) =
  let open Lwt_result_syntax in
  match operation with
  | Manager_operation {operation = Reveal _; _} when not (ctxt.length = 0) ->
      tzfail_p
      @@ Imported_protocol.Validate_errors.Manager.Incorrect_reveal_position
  | Manager_operation
      {source; fee; counter; operation; gas_limit; storage_limit = _} ->
      let** () = validate_supported_operation ~ctxt operation in
      let** () = validate_source ~ctxt source in
      let** () = validate_counter ~ctxt counter in
      let overall_gas_limit =
        Z.(ctxt.gas_limit + Tezos_types.Operation.gas_limit_to_z gas_limit)
      in
      let** () =
        validate_gas_limit ~ctxt overall_gas_limit gas_limit (Manager operation)
      in
      (* TODO check storage limit too *)
      let** ctxt = validate_balance ~ctxt ~fee in
      (* the update will be updated during the validation steps *)
      return
        (Ok
           {
             ctxt with
             previous_counter = Some ctxt.next_counter;
             next_counter = Manager_counter.succ ctxt.next_counter;
             length = ctxt.length + 1;
             gas_limit = overall_gas_limit;
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

let validate_first_counter ~read ~source ~first_counter =
  let open Lwt_result_syntax in
  let* counter =
    Tezlink_durable_storage.counter read
    @@ Tezos_types.Contract.of_implicit source
  in
  let*? first_counter = Tezos_types.Operation.counter_to_z first_counter in
  if Z.gt first_counter counter then
    (* we allow the first counter to be in the future *) return (Ok ())
  else
    let expected =
      Imported_protocol.Manager_counter_repr.Internal_for_tests.of_int
      @@ Z.to_int counter
    in
    let found =
      Imported_protocol.Manager_counter_repr.Internal_for_tests.of_int
      @@ Z.to_int first_counter
    in

    tzfail_p
    @@ Imported_protocol.Contract_storage.(
         Counter_in_the_past {contract = Implicit source; expected; found})

let validate_size ~raw ~error_clue =
  let open Lwt_result_syntax in
  let length = Bytes.length raw in
  if length <= Constants.max_operation_data_length then return (Ok ())
  else
    tzfail
    @@ Oversized_operation
         (error_clue, length, Constants.max_operation_data_length)

let get_signature signature =
  let open Lwt_result_syntax in
  match signature with
  | None -> tzfail_p @@ Imported_protocol.Operation_repr.Missing_signature
  | Some signature -> return (Ok signature)

(** Checks only existence of signature if `check_signature` is true. Used to fail early if there is no signature but one is required. *)
let signature_exists ~check_signature signature =
  let open Lwt_result_syntax in
  if not check_signature then return (Ok ())
  else
    let** _signature = get_signature signature in
    return (Ok ())

(** We can't reuse [Alpha_context.check_signature] because it's asking for the
   context to check attestation and preattestation Bls signatures. So we
   implement a simplified version of the underlying
   [Operation_repr.check_signature] which is unavailable to us because of type.
*)
let validate_signature ~check_signature shell contents pk signature =
  let open Lwt_result_syntax in
  if not check_signature then return (Ok ())
  else
    let** signature = get_signature signature in
    let unsigned_operation =
      Data_encoding.Binary.to_bytes_exn
        Operation.unsigned_encoding
        (shell, contents)
    in
    if
      (* That watermark is used for all operations except endorsement, which we
       don't support. *)
      Signature.check
        ~watermark:Generic_operation
        pk
        signature
        unsigned_operation
    then return (Ok ())
    else tzfail_p @@ Imported_protocol.Operation_repr.Invalid_signature

(** [signature_cost pk op] unpacks [op] then returns the gas cost of the
    signature check.*)
let signature_cost pk {shell; protocol_data = Operation_data protocol_data} =
  Imported_protocol.Operation_costs.check_signature_cost
    (Imported_protocol.Michelson_v1_gas.Cost_of.Interpreter.algo_of_public_key
       pk)
    {shell; protocol_data}

let validate_tezlink_operation ?(check_signature = true) ~read raw =
  let open Lwt_result_syntax in
  let raw = Bytes.of_string raw in
  let error_clue = Operation_hash.hash_bytes [raw] in
  let** () = validate_size ~raw ~error_clue in
  let* op =
    match Data_encoding.Binary.of_bytes Operation.encoding raw with
    | Error e -> tzfail @@ Parsing_failure (error_clue, e)
    | Ok op -> return op
  in
  let {protocol_data = Operation_data {contents; signature}; shell} = op in
  let first, rest =
    match contents with
    | Single only_operation -> (Contents only_operation, [])
    | Cons (first_operation, rest) ->
        (Contents first_operation, Operation.to_list (Contents_list rest))
  in
  let** () = signature_exists ~check_signature signature in
  let** pk, source, first_counter =
    validate_manager_info ~read ~error_clue first
  in
  let signature_check_cost = signature_cost pk op in
  let* balance_left =
    Tezlink_durable_storage.balance read
    @@ Tezos_types.Contract.of_implicit source
  in
  let** () = validate_first_counter ~read ~source ~first_counter in
  let** ctxt =
    validate_batch
      ~ctxt:
        {
          source;
          balance_left;
          previous_counter = None;
          next_counter = first_counter;
          error_clue;
          first_counter;
          length = 0;
          fee = Tez.zero;
          gas_limit = Z.zero;
          signature_check_cost;
        }
      (first :: rest)
  in
  let** () =
    validate_signature
      ~check_signature
      shell
      (Contents_list contents)
      pk
      signature
  in
  let*? first_counter = Tezos_types.Operation.counter_to_z ctxt.first_counter in
  let operation : Tezos_types.Operation.t =
    Tezos_types.Operation.
      {
        length = ctxt.length;
        source = ctxt.source;
        raw;
        op;
        first_counter;
        fee = ctxt.fee;
        gas_limit = ctxt.gas_limit;
      }
  in
  return (Ok operation)

let init_blueprint_validation read () =
  let get_counter = Tezlink_durable_storage.counter read in
  let get_balance = Tezlink_durable_storage.balance_z read in
  let michelson_config = {get_balance; get_counter} in
  empty_validation_state
    ~michelson_config
    ~evm_config:Validation_types.dummy_evm_config

let maximum_gas_per_block =
  Tezos_types.Operation.gas_limit_to_z
    Tezlink_constants.all_constants.parametric.hard_gas_limit_per_block

let could_fit state (operation : Tezos_types.Operation.t) =
  Z.(state.gas + operation.gas_limit <= maximum_gas_per_block)

let add_ source = String.Map.add (Signature.Public_key_hash.to_b58check source)

let get_ read cache source =
  let open Lwt_result_syntax in
  match
    String.Map.find (Signature.Public_key_hash.to_b58check source) cache
  with
  | Some v -> return v
  | None -> read (Tezlink_imports.Alpha_context.Contract.Implicit source)

let validate_for_blueprint state (operation : Tezos_types.Operation.t) =
  let open Lwt_result_syntax in
  let* counter =
    get_ state.michelson_config.get_counter state.addr_nonce operation.source
  in
  let** new_counter =
    if not Z.(equal (succ counter) operation.first_counter) then
      return
      @@ Error
           (Format.asprintf
              "Operation counter %a does not follow the current counter %a"
              Z.pp_print
              operation.first_counter
              Z.pp_print
              counter)
    else return @@ Ok Z.(add counter (Z.of_int operation.length))
  in
  let* balance =
    get_ state.michelson_config.get_balance state.addr_balance operation.source
  in
  let** new_balance =
    let new_balance = Z.(balance - Tezos_types.Tez.to_mutez_z operation.fee) in
    if new_balance >= Z.zero then return @@ Ok new_balance
    else
      return
      @@ Error
           (Format.asprintf
              "Not enough funds to pay the fees %a"
              Tez.pp
              operation.fee)
  in
  return
    (Ok
       {
         state with
         addr_nonce = add_ operation.source new_counter state.addr_nonce;
         addr_balance = add_ operation.source new_balance state.addr_balance;
         gas = Z.(state.gas + operation.gas_limit);
       })
