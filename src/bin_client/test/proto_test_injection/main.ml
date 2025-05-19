(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type block_header_data = bytes

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

let block_header_data_encoding =
  Data_encoding.(obj1 (req "random_data" (Variable.bytes Hex)))

type block_header_metadata = unit

let block_header_metadata_encoding = Data_encoding.unit

type operation_data = unit

let operation_data_encoding = Data_encoding.unit

type operation_receipt = unit

let operation_receipt_encoding = Data_encoding.unit

let operation_data_and_receipt_encoding =
  Data_encoding.conv
    (function (), () -> ())
    (fun () -> ((), ()))
    Data_encoding.unit

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let max_block_length = 42

let max_operation_data_length = 42

let validation_passes = []

let compare_operations _ _ = 0

let acceptable_pass _ = Some 0

type validation_state = unit

type application_state = {context : Context.t; fitness : Int64.t}

module Fitness = struct
  type error += Invalid_fitness

  type error += Invalid_fitness2

  let int64_to_bytes i =
    let b = Bytes.make 8 '0' in
    TzEndian.set_int64 b 0 i ;
    b

  let int64_of_bytes b =
    if Compare.Int.(Bytes.length b <> 8) then tzfail Invalid_fitness2
    else return (TzEndian.get_int64 b 0)

  let from_int64 fitness = [int64_to_bytes fitness]

  let to_int64 = function
    | [fitness] -> int64_of_bytes fitness
    | [] -> return 0L
    | _ -> tzfail Invalid_fitness

  let get {fitness; _} = fitness
end

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

let begin_validation _ctxt _chain_id _mode ~predecessor:_ = return ()

let validate_operation ?check_signature:_ _validation_state _oph _op = return ()

let finalize_validation _validation_state = return ()

let begin_application context _chain_id mode
    ~(predecessor : Block_header.shell_header) =
  let open Lwt_result_syntax in
  let* fitness =
    match mode with
    | Application block_header | Partial_validation block_header ->
        Fitness.to_int64 block_header.shell.fitness
    | Construction _ | Partial_construction _ ->
        let* predecessor_fitness = Fitness.to_int64 predecessor.fitness in
        return (Int64.succ predecessor_fitness)
  in
  return {context; fitness}

let apply_operation application_state _oph _op = return (application_state, ())

let finalize_application application_state _block_header =
  let fitness = Fitness.get application_state in
  let message = Some (Format.asprintf "fitness <- %Ld" fitness) in
  let fitness = Fitness.from_int64 fitness in
  return
    ( {
        Updater.message;
        context = application_state.context;
        fitness;
        max_operations_ttl = 0;
        last_finalized_block_level = 0l;
        last_preserved_block_level = 0l;
      },
      () )

let rpc_services = RPC_directory.empty

let init _chain_id ctxt block_header =
  let fitness = block_header.Block_header.fitness in
  let message = None in
  return
    {
      Updater.message;
      context = ctxt;
      fitness;
      max_operations_ttl = 0;
      last_finalized_block_level = 0l;
      last_preserved_block_level = 0l;
    }

type error += Missing_value_in_cache

let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
    ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
  return (fun _ -> tzfail Missing_value_in_cache)

(* Fake mempool *)
module Mempool = struct
  type t = unit

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

  let init _ _ ~head_hash:_ ~head:_ = Lwt.return_ok ((), ())

  let encoding = Data_encoding.unit

  let partial_op_validation ?check_signature:_ _ _ = Lwt.return_ok []

  let add_valid_operation ?conflict_handler:_ _ _ = Ok ((), Unchanged)

  let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
    Lwt.return_ok ((), Unchanged)

  let remove_operation () _ = ()

  let merge ?conflict_handler:_ () () = Ok ()

  let operations () = Operation_hash.Map.empty
end
