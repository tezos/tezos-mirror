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

let relative_position_within_block _ _ = 0

type validation_state = {context : Context.t; fitness : Fitness.t}

let begin_application ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_fitness:_ (raw_block : block_header) =
  let fitness = raw_block.shell.fitness in
  return {context; fitness}

let begin_partial_application ~chain_id ~ancestor_context ~predecessor_timestamp
    ~predecessor_fitness block_header =
  begin_application
    ~chain_id
    ~predecessor_context:ancestor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    block_header

let version_number = "\001"

let int64_to_bytes i =
  let b = Bytes.make 8 '0' in
  TzEndian.set_int64 b 0 i ;
  b

let fitness_from_level level =
  [Bytes.of_string version_number; int64_to_bytes level]

let begin_construction ~chain_id:_ ~predecessor_context:context
    ~predecessor_timestamp:_ ~predecessor_level ~predecessor_fitness:_
    ~predecessor:_ ~timestamp:_ ?protocol_data () =
  let fitness = fitness_from_level Int64.(succ (of_int32 predecessor_level)) in
  let _mode =
    match protocol_data with Some _ -> "block" | None -> "mempool"
  in
  return {context; fitness}

type error += No_error

let () =
  register_error_kind
    `Permanent
    ~id:"no-error"
    ~title:"No error"
    ~description:"There is no error, this is a no-op protocol"
    ~pp:(fun ppf () -> Format.fprintf ppf "@[<h 0>No error in no-op protocol@]")
    Data_encoding.unit
    (function No_error -> Some () | _ -> None)
    (fun () -> No_error)

let apply_operation _state _op = fail No_error

let finalize_block state _ =
  let fitness = state.fitness in
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
  return
    {
      Updater.message = None;
      context;
      fitness;
      max_operations_ttl = 0;
      last_allowed_fork_level = 0l;
    }

let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
    ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
  return (fun _ -> fail No_error)

let rpc_services = RPC_directory.empty
