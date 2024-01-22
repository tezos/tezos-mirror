(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let max_block_length = 100

let max_operation_data_length = 100

let validation_passes = Updater.[{max_size = 1000; max_op = None}]

let acceptable_pass _op = Some 0

type block_header_data = Header.t

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

let block_header_data_encoding = Header.encoding

type block_header_metadata = State.t

let block_header_metadata_encoding_with_legacy_attestation_name =
  State.encoding

let block_header_metadata_encoding = State.encoding

type operation_data = Proto_operation.t

let operation_data_encoding = Proto_operation.encoding

let operation_data_encoding_with_legacy_attestation_name =
  operation_data_encoding

type operation_receipt = Receipt.t

let operation_receipt_encoding = Receipt.encoding

let operation_receipt_encoding_with_legacy_attestation_name =
  operation_receipt_encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2 (req "data" Proto_operation.encoding) (req "receipt" Receipt.encoding))

let operation_data_and_receipt_encoding_with_legacy_attestation_name =
  operation_data_and_receipt_encoding

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

type validation_state = {context : Context.t; fitness : Fitness.t}

type application_state = validation_state

type mode =
  | Application of block_header
  | Partial_validation of block_header
  | Construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
      block_header_data : block_header_data;
    }
  | Partial_construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
    }

let mode_str = function
  | Application _ -> "application"
  | Partial_validation _ -> "partial_validation"
  | Construction _ -> "construction"
  | Partial_construction _ -> "partial_construction"

let validation_or_application_str = function
  | `Validation -> "validation"
  | `Application -> "application"

let begin_any_application_mode validation_or_application mode context
    ~(predecessor : Block_header.shell_header) (block_header : block_header) =
  let fitness = block_header.shell.fitness in
  Logging.log
    Notice
    "begin_%s (%s mode): pred_fitness = %a  block_fitness = %a%!"
    (validation_or_application_str validation_or_application)
    (mode_str mode)
    Fitness.pp
    predecessor.fitness
    Fitness.pp
    fitness ;
  (* Note: Logging is only available for debugging purposes and should
     not appear in a real protocol. *)
  return {context; fitness}

(* we use here the same fitness format than proto alpha,
   but with higher [version_number] to allow testing
   migration from alpha to demo_counter. *)
let version_number = "\255"

let int64_to_bytes i =
  let b = Bytes.make 8 '\000' in
  TzEndian.set_int64 b 0 i ;
  b

let fitness_from_level level =
  [
    Bytes.of_string version_number;
    Bytes.of_string "\000";
    Bytes.of_string "\000";
    Bytes.of_string "\000";
    int64_to_bytes level;
  ]

let begin_any_construction_mode validation_or_application mode context
    ~(predecessor : Block_header.shell_header) =
  let fitness = fitness_from_level Int64.(succ (of_int32 predecessor.level)) in
  Logging.log
    Notice
    "begin_%s (%s mode): pred_fitness = %a  constructed fitness = %a%!"
    (validation_or_application_str validation_or_application)
    (mode_str mode)
    Fitness.pp
    predecessor.fitness
    Fitness.pp
    fitness ;
  return {context; fitness}

let begin_validation_or_application validation_or_application ctxt _chain_id
    mode ~predecessor =
  match mode with
  | Application block_header | Partial_validation block_header ->
      begin_any_application_mode
        validation_or_application
        mode
        ctxt
        ~predecessor
        block_header
  | Construction _ | Partial_construction _ ->
      begin_any_construction_mode
        validation_or_application
        mode
        ctxt
        ~predecessor

let begin_validation = begin_validation_or_application `Validation

let begin_application = begin_validation_or_application `Application

let apply_operation_aux application_state operation =
  let open Lwt_result_syntax in
  let {context; fitness} = application_state in
  let*! state = State.get_state context in
  match Apply.apply state operation.protocol_data with
  | None -> Error_monad.tzfail Error.Invalid_operation
  | Some state ->
    let*! context = State.update_state context state in
    return {context; fitness}

let validate_operation ?check_signature:_ validation_state _oph operation =
  Logging.log Notice "validate_operation" ;
  apply_operation_aux validation_state operation

let apply_operation application_state _oph operation =
  let open Lwt_result_syntax in
  Logging.log Notice "apply_operation" ;
  let* application_state = apply_operation_aux application_state operation in
  let receipt = Receipt.create "operation applied successfully" in
  return (application_state, receipt)

let log_finalize validation_or_application validation_state =
  Logging.log
    Notice
    "finalize_%s: fitness = %a%!"
    (validation_or_application_str validation_or_application)
    Fitness.pp
    validation_state.fitness

let finalize_validation validation_state =
  log_finalize `Validation validation_state ;
  return_unit

let finalize_application application_state _shell_header =
  let open Lwt_result_syntax in
  log_finalize `Application application_state ;
  let fitness = application_state.fitness in
  let message = Some (Format.asprintf "fitness <- %a" Fitness.pp fitness) in
  let context = application_state.context in
  let*! state = State.get_state context in
  return
    ( {
        Updater.message;
        context;
        fitness;
        max_operations_ttl = 0;
        last_allowed_fork_level = 0l;
      },
      state )

let decode_json json =
  match Proto_params.from_json json with
  | exception _ ->
    tzfail Error.Invalid_protocol_parameters
  | proto_params ->
    return proto_params

let get_init_state context : State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let protocol_params_key = ["protocol_parameters"] in
  let*! params_bytes = Context.find context protocol_params_key in
  let* Proto_params.{init_a; init_b} =
    match params_bytes with
      | None ->
        return Proto_params.default
      | Some bytes -> (
          match Data_encoding.Binary.of_bytes_opt Data_encoding.json bytes with
          | None ->
            tzfail (Error.Failed_to_parse_parameter bytes)
          | Some json ->
            decode_json json )
  in
  match State.create init_a init_b with
  | None ->
    tzfail Error.Invalid_protocol_parameters
  | Some state ->
    return state

let init _chain_id context block_header =
  let open Lwt_result_syntax in
  let open Block_header in
  let fitness = block_header.fitness in
  Logging.log Notice "init: fitness = %a%!" Fitness.pp fitness ;
  let* init_state = get_init_state context in
  let*! init_context = State.update_state context init_state in
  return
    {
      Updater.message = None;
      context = init_context;
      fitness;
      max_operations_ttl = 0;
      last_allowed_fork_level = block_header.level;
    }

let compare_operations _ _ = 0

type Context.Cache.value += Demo of int

let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
    ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
  return (fun _ -> return (Demo 123))

let rpc_services = Services.rpc_services

module Mempool = struct
  type t = State.t

  type validation_info = unit

  type conflict_handler =
    existing_operation:Operation_hash.t * operation ->
    new_operation:Operation_hash.t * operation ->
    [`Keep | `Replace]

  type operation_conflict =
    | Operation_conflict of {
        existing : Operation_hash.t;
        new_operation : Operation_hash.t;
      }

  type add_result =
    | Added
    | Replaced of {removed : Operation_hash.t}
    | Unchanged

  type add_error =
    | Validation_error of error trace
    | Add_conflict of operation_conflict

  type merge_error =
    | Incompatible_mempool
    | Merge_conflict of operation_conflict

  let init ctxt _chain_id ~head_hash:_ ~(head : Block_header.shell_header) =
    let open Lwt_result_syntax in
    Logging.log
      Notice
      "Mempool.init: head fitness = %a%!"
      Fitness.pp
      head.fitness ;
    let*! state = State.get_state ctxt in
    return ((), state)

  let encoding = State.encoding

  let add_operation ?check_signature:_ ?conflict_handler:_
      (_info : validation_info) state ((_oph : Operation_hash.t), op) =
    match Apply.apply state op.protocol_data with
    | None ->
        Lwt.return_error
          (Validation_error (trace_of_error Error.Invalid_operation))
    | Some state -> return (state, Added)

  (* This mempool does not currently support removing an operation. *)
  let remove_operation _ _ = assert false

  (* This mempool does not currently support merging. *)
  let merge ?conflict_handler:_ _ _ = assert false

  (* This function is not currently used in the context of
     [proto_demo_counter]. If it is needed in the future, the type [t]
     will need to be extended to remember all added operations. *)
  let operations _ = assert false
end
