(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports
open Imported_context
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
  | Bls_is_not_allowed of clue * Signature.V2.public_key_hash

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.bls_is_not_allowed"
    ~title:"Failed to validate an operation: tz4 are forbidden"
    ~description:
      "Validation of the operation failed because tz4 addresses are not \
       supported."
    ~pp:(fun ppf (clue, key) ->
      Format.fprintf
        ppf
        "Failed to validate operation %a: tz4 keys are forbidden %a"
        pp_clue
        clue
        Signature.V2.Public_key_hash.pp
        key)
    Data_encoding.(
      obj2
        (req "operation" clue_encoding)
        (req "key" Signature.V2.Public_key_hash.encoding))
    (function Bls_is_not_allowed (clue, err) -> Some (clue, err) | _ -> None)
    (fun (clue, err) -> Bls_is_not_allowed (clue, err)) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.parsing_failure"
    ~title:"Failed to validate an operation: could not parse the operation"
    ~description:"Validation of the operation failed during parsing."
    ~pp:(fun ppf (clue, err) ->
      Format.fprintf
        ppf
        "Failed to validate operation %a: error during parsing %a"
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
    ~title:"Failed to validate an operation: only manager operation are allowed"
    ~description:
      "Validation of the operation failed because it was not recognized as a \
       manager operation."
    ~pp:(fun ppf raw ->
      Format.fprintf
        ppf
        "Failed to validate operation %a: not a manager_operation"
        pp_clue
        raw)
    Data_encoding.(obj1 (req "operation" clue_encoding))
    (function Not_a_manager_operation raw -> Some raw | _ -> None)
    (fun raw -> Not_a_manager_operation raw) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezlink.unsupported_manager_operation"
    ~title:"Failed to validate an operation: unsupported manager operation."
    ~description:
      "Validation of the operation failed because it contains at least one \
       unsupported operation. The only supported operations are reveal, \
       transaction and origination."
    ~pp:(fun ppf op ->
      Format.fprintf
        ppf
        "Failed to validate operation %a: it contains at least one unsupported \
         operation"
        pp_clue
        op)
    Data_encoding.(obj1 (req "operation" clue_encoding))
    (function Unsupported_manager_operation op -> Some op | _ -> None)
    (fun op -> Unsupported_manager_operation op) ;
  register_error_kind
    `Temporary
    ~id:"evm_node.dev.tezlink.too_big"
    ~title:"Failed to validate an operation: too big."
    ~description:
      "Validation of the operation failed because the operation is too big."
    ~pp:(fun ppf (op, size, max) ->
      Format.fprintf
        ppf
        "Failed to validate operation %a: oversized operation (size: %d, max: \
         %d)"
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

(** Search for the manager info (public key, counter), and checks that it is
    valid: the public key must be already revealed, or be the reveal must be
    the first operation of the batch. *)
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
      | (None | Some (Hash _)), Reveal {public_key; _} ->
          (* The operation might be the reveal of the public key we're
             searching for. *)
          let open Signature.V2 in
          let pkh_revealed = Public_key.hash public_key in
          if Public_key_hash.equal source pkh_revealed then
            return @@ Ok (public_key, source, counter)
          else
            (* Each public key must be revealed by the corresponding address. *)
            tzfail_p
            @@ Imported_protocol.Contract_manager_storage.(
                 Inconsistent_hash
                   {
                     public_key;
                     expected_hash = source;
                     provided_hash = pkh_revealed;
                   })
      | (None | Some (Hash _)), _op ->
          tzfail_p
          @@ Imported_protocol.Contract_manager_storage.Unrevealed_manager_key
               (Implicit source))
  | _ -> tzfail @@ Not_a_manager_operation error_clue

(** Information required to validate a batch of operations. Some will change
    while folding on the batch. We'll need :
       - the [source]
       - the [next_counter] to check the operation of each counter are in order.
         This value will increase by one for each operation.
       - the [balance_left] to check fees can be payed. This value will decrease
         by the amount of fees of each operation.
    To build the object Operation.t to include in the tx_queue we'll need:
       - the [source]
       - the [first_counter]
       - the [length] of the batch
       - the total [fee_sum] (to check the validity when inserting in a
         blueprint, as the source balance might have changed)
       - the total [gas_limit_sum] (to check if there is room in the blueprint)
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
  fee_sum : Tez.t;
  gas_limit_sum : Z.t;
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
           (ctxt.error_clue, Signature.V2.Public_key.hash public_key)
  | _ -> return (Ok ())

let validate_transaction ctxt (Transaction {destination; _}) =
  let open Lwt_result_syntax in
  match destination with
  | Implicit destination when is_tz4 destination ->
      tzfail @@ Bls_is_not_allowed (ctxt.error_clue, destination)
  | _ -> return (Ok ())

(** Not all the operation defined in the protocol are supported by Tezlink. *)
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
  if Signature.V2.Public_key_hash.equal ctxt.source second_source then
    return (Ok ())
  else
    tzfail_p
    @@ Imported_protocol.Validate_errors.Manager.(
         Inconsistent_sources
           {expected_source = ctxt.source; source = second_source})

let validate_balance ~ctxt ~fee =
  let open Lwt_result_syntax in
  (* Convert a Tez.t into a Tez_repr.t: we reuse the appropriate errors from
     the protocol but they are not defined on Tez.t *)
  let tezrep_of t =
    let open Imported_protocol in
    t |> Tez.to_mutez |> Tez_repr.of_mutez
    |> Option.value ~default:Tez_repr.zero
    (* The conversion should not fail so the zero value won't be used. *)
  in
  match Tez.sub_opt ctxt.balance_left fee with
  | Some balance_left -> (
      (* The source can pay for the fees, we compute the total fee for the
         batch. *)
      match Tez.(ctxt.fee_sum +? fee) with
      | Ok fee_sum -> return (Ok {ctxt with balance_left; fee_sum})
      | Error _ ->
          tzfail_p
          @@ Imported_protocol.Tez_repr.Addition_overflow
               (tezrep_of ctxt.fee_sum, tezrep_of fee))
  | None ->
      (* The source can't pay for the fees. *)
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
        (* The first counter in the batch is equal to ctxt.counter by
           construction. *)
        failwith "unreachable"

let hard_gas_limit_per_operation =
  Tezos_types.Operation.gas_limit_to_z
    Tezlink_constants.all_constants.parametric.hard_gas_limit_per_operation

let hard_gas_limit_per_block =
  Tezos_types.Operation.gas_limit_to_z
    Tezlink_constants.all_constants.parametric.hard_gas_limit_per_block

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
      (* Only checking a Bls proof has a cost here, and we don't do Bls. *)
      return_unit
  | _ -> tzfail @@ Unsupported_manager_operation ctxt.error_clue

let validate_gas_limit ~ctxt gas_limit operation =
  let open Lwt_result_syntax in
  let gas_limit_z = Tezos_types.Operation.gas_limit_to_z gas_limit in
  let overall_gas_limit = Z.(ctxt.gas_limit_sum + gas_limit_z) in
  (* We have two limits to check:
     - the operation can't have a gas limit higher than
     hard_gas_limit_per_operation,
     - the batch can't have a gas limit higher than hard_gas_limit_per_block.
  *)
  if
    not
      Z.Compare.(
        hard_gas_limit_per_operation >= gas_limit_z
        && hard_gas_limit_per_block >= overall_gas_limit
        && gas_limit_z >= Z.zero)
  then tzfail_p @@ Imported_protocol.Gas_limit_repr.Gas_limit_too_high
  else
    (* There is a fixed cost for all manager operations. See
           [check_contents] in {!lib_protocol/validate.ml}.*)
    let fixed_cost =
      if ctxt.length = 0 then
        (* The gas cost of signature verification is included in the first
           operation's cost. *)
        Gas.(
          Imported_protocol.Michelson_v1_gas.Cost_of.manager_operation
          +@ ctxt.signature_check_cost)
      else Imported_protocol.Michelson_v1_gas.Cost_of.manager_operation
    in
    (* validate fixed cost *)
    let*? remaining_gas =
      Imported_env.wrap_tzresult
      @@ Gas.consume_from (Gas.Arith.fp gas_limit) fixed_cost
    in
    let*? () = validate_variable_gas_cost ~ctxt remaining_gas operation in
    return (Ok overall_gas_limit)

let validate_storage_limit storage_limit =
  error_unless
    Compare.Z.(
      storage_limit
      <= Tezlink_constants.all_constants.parametric
           .hard_storage_limit_per_operation
      && storage_limit >= Z.zero)
    (Imported_env.wrap_tzerror Fees.Storage_limit_too_high)

let validate_operation_in_batch ~(ctxt : batch_validation_context)
    (Contents operation : packed_contents) =
  let open Lwt_result_syntax in
  match operation with
  | Manager_operation {operation = Reveal _; _} when not (ctxt.length = 0) ->
      tzfail_p
      @@ Imported_protocol.Validate_errors.Manager.Incorrect_reveal_position
  | Manager_operation
      {source; fee; counter; operation; gas_limit; storage_limit} ->
      let** () = validate_supported_operation ~ctxt operation in
      let** () = validate_source ~ctxt source in
      let** () = validate_counter ~ctxt counter in
      let** gas_limit_sum =
        validate_gas_limit ~ctxt gas_limit (Manager operation)
      in
      let*? () = validate_storage_limit storage_limit in
      let** ctxt = validate_balance ~ctxt ~fee in
      return
        (Ok
           {
             ctxt with
             previous_counter = Some ctxt.next_counter;
             next_counter = Manager_counter.succ ctxt.next_counter;
             length = ctxt.length + 1;
             gas_limit_sum;
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
  let counter = Option.value ~default:Z.zero counter in
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

(** Checks only existence of signature if `check_signature` is true. Used to
    fail early if there is no signature but one is required. The option
    `check_signature` is used during simulation, where the signature is not
    available. *)
let signature_exists ~check_signature signature =
  let open Lwt_result_syntax in
  if not check_signature then return (Ok ())
  else
    let** _signature = get_signature signature in
    return (Ok ())

(** We can't reuse {!Alpha_context.check_signature} because it's asking for the
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
      (* That watermark ({!Generic_operation}) is used for all operations
         except endorsement, which we don't support. *)
      Signature.V2.check
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

let parse_and_validate_for_queue ?(check_signature = true) ~read raw =
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
  let initial_context =
    {
      source;
      balance_left;
      previous_counter = None;
      next_counter = first_counter;
      error_clue;
      first_counter;
      length = 0;
      fee_sum = Tez.zero;
      gas_limit_sum = Z.zero;
      signature_check_cost;
    }
  in
  let** ctxt = validate_batch ~ctxt:initial_context (first :: rest) in
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
        fee = ctxt.fee_sum;
        gas_limit = ctxt.gas_limit_sum;
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

let gas_limit_could_fit state (operation : Tezos_types.Operation.t) =
  Z.(state.gas + operation.gas_limit <= maximum_gas_per_block)

let add_ source cache =
  String.Map.add (Signature.V2.Public_key_hash.to_b58check source) cache

let get_ read cache source =
  let open Lwt_result_syntax in
  match
    String.Map.find (Signature.V2.Public_key_hash.to_b58check source) cache
  with
  | Some v -> return v
  | None -> read (Tezlink_imports.Imported_context.Contract.Implicit source)

let validate_for_blueprint state (operation : Tezos_types.Operation.t) =
  let open Lwt_result_syntax in
  let* counter =
    get_
      (fun c ->
        let* counter_opt = state.michelson_config.get_counter c in
        (* FIXME: #7960
           This default should be the global counter *)
        return (Option.value ~default:Z.one counter_opt))
      state.addr_nonce
      operation.source
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
