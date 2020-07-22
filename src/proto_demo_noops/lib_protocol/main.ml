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

let max_operation_data_length = 0

let validation_passes = []

let acceptable_passes _op = []

type block_header_data = string

let block_header_data_encoding =
  Data_encoding.(obj1 (req "block_header_data" string))

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

type block_header_metadata = unit

let block_header_metadata_encoding = Data_encoding.unit

type operation_data = unit

let operation_data_encoding = Data_encoding.unit

type operation_receipt = unit

let operation_receipt_encoding = Data_encoding.unit

let operation_data_and_receipt_encoding =
  Data_encoding.conv
    (function ((), ()) -> ())
    (fun () -> ((), ()))
    Data_encoding.unit

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let compare_operations _ _ = 0

type validation_state = {context : Context.t; fitness : Fitness.t}

let current_context {context; _} = return context

let begin_application ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_fitness (raw_block : block_header) =
  let fitness = raw_block.shell.fitness in
  Logging.log_notice
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
  Logging.log_notice "begin_partial_application%!" ;
  begin_application
    ~chain_id
    ~predecessor_context:ancestor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    block_header

let version_number = "\001"

let int64_to_bytes i =
  let b = Bytes.make 8 '0' in
  TzEndian.set_int64 b 0 i ; b

let fitness_from_level level =
  [Bytes.of_string version_number; int64_to_bytes level]

let begin_construction ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_level ~predecessor_fitness
    ~predecessor:_ ~timestamp:_ ?protocol_data () =
  let fitness = fitness_from_level Int64.(succ (of_int32 predecessor_level)) in
  let mode =
    match protocol_data with Some _ -> "block" | None -> "mempool"
  in
  Logging.log_notice
    "begin_construction (%s): pred_fitness = %a  constructed fitness = %a%!"
    mode
    Fitness.pp
    predecessor_fitness
    Fitness.pp
    fitness ;
  return {context; fitness}

let apply_operation _state _op = Lwt.return (Error [])

let finalize_block state =
  let fitness = state.fitness in
  Logging.log_notice "finalize_block: fitness = %a%!" Fitness.pp fitness ;
  return
    ( {
        Updater.message = None;
        context = state.context;
        fitness;
        max_operations_ttl = 0;
        last_allowed_fork_level = 0l;
      },
      () )

let init context block_header =
  let open Block_header in
  let fitness = block_header.fitness in
  Logging.log_notice "init: fitness = %a%!" Fitness.pp fitness ;
  return
    {
      Updater.message = None;
      context;
      fitness;
      max_operations_ttl = 0;
      last_allowed_fork_level = 0l;
    }

let rpc_services = RPC_directory.empty
