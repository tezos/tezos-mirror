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

let acceptable_passes _op = [0]

type block_header_data = Header.t

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

let block_header_data_encoding = Header.encoding

type block_header_metadata = State.t

let block_header_metadata_encoding = State.encoding

type operation_data = Proto_operation.t

let operation_data_encoding = Proto_operation.encoding

type operation_receipt = Receipt.t

let operation_receipt_encoding = Receipt.encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2 (req "data" Proto_operation.encoding) (req "receipt" Receipt.encoding))

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

type validation_state = {context : Context.t; fitness : Fitness.t}

let begin_application ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_fitness (raw_block : block_header) =
  let fitness = raw_block.shell.fitness in
  Logging.log Notice
    "begin_application: pred_fitness = %a  block_fitness = %a%!"
    Fitness.pp
    predecessor_fitness
    Fitness.pp
    fitness ;
  (* Note: Logging is only available for debugging purposes and should
     not appear in a real protocol. *)
  return {context; fitness}

let begin_partial_application ~chain_id ~ancestor_context
    ~predecessor_timestamp ~predecessor_fitness block_header =
  Logging.log Notice "begin_partial_application%!" ;
  begin_application
    ~chain_id
    ~predecessor_context:ancestor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    block_header

(* we use here the same fitness format than proto alpha,
   but with higher [version_number] to allow testing
   migration from alpha to demo_counter. *)
let version_number = "\255"

let int64_to_bytes i =
  let b = Bytes.make 8 '\000' in
  TzEndian.set_int64 b 0 i ;
  b

let fitness_from_level level =
  [Bytes.of_string version_number;
   Bytes.of_string "\000";
   Bytes.of_string "\000";
   Bytes.of_string "\000";
   int64_to_bytes level]

let begin_construction ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_level ~predecessor_fitness
    ~predecessor:_ ~timestamp:_ ?protocol_data () =
  let fitness = fitness_from_level Int64.(succ (of_int32 predecessor_level)) in
  let mode =
    match protocol_data with Some _ -> "block" | None -> "mempool"
  in
  Logging.log Notice
    "begin_construction (%s): pred_fitness = %a  constructed fitness = %a%!"
    mode
    Fitness.pp
    predecessor_fitness
    Fitness.pp
    fitness ;
  return {context; fitness}

let apply_operation validation_state operation =
  Logging.log Notice "apply_operation" ;
  let {context; fitness} = validation_state in
  State.get_state context
  >>= fun state ->
  match Apply.apply state operation.protocol_data with
  | None ->
      Error_monad.fail Error.Invalid_operation
  | Some state ->
      let receipt = Receipt.create "operation applied successfully" in
      State.update_state context state
      >>= fun context -> return ({context; fitness}, receipt)

let finalize_block validation_state _header  =
  let fitness = validation_state.fitness in
  Logging.log Notice "finalize_block: fitness = %a%!" Fitness.pp fitness ;
  let fitness = validation_state.fitness in
  let message = Some (Format.asprintf "fitness <- %a" Fitness.pp fitness) in
  let context = validation_state.context in
  State.get_state context
  >>= fun state ->
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
      fail Error.Invalid_protocol_parameters
  | proto_params ->
      return proto_params

let get_init_state context : State.t tzresult Lwt.t =
  let protocol_params_key = ["protocol_parameters"] in
  Context.find context protocol_params_key
  >>= (function
        | None ->
            return Proto_params.default
        | Some bytes -> (
          match Data_encoding.Binary.of_bytes_opt Data_encoding.json bytes with
          | None ->
              fail (Error.Failed_to_parse_parameter bytes)
          | Some json ->
              decode_json json ))
  >>=? function
  | Proto_params.{init_a; init_b} -> (
    match State.create init_a init_b with
    | None ->
        fail Error.Invalid_protocol_parameters
    | Some state ->
        return state )

let init context block_header =
  let open Block_header in
  let fitness = block_header.fitness in
  Logging.log Notice "init: fitness = %a%!" Fitness.pp fitness ;
  get_init_state context
  >>=? fun init_state ->
  State.update_state context init_state
  >>= fun init_context ->
  return
    {
      Updater.message = None;
      context = init_context;
      fitness;
      max_operations_ttl = 0;
      last_allowed_fork_level = block_header.level;
    }

let relative_position_within_block _ _ = 0

type Context.Cache.value += Demo of int

let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
    ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
  return (fun _ -> return (Demo 123))

let rpc_services = Services.rpc_services
