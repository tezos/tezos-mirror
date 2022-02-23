(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module Internal_for_tests = struct
  (** An implementation of {!Tezos_protocol_environment.PROTOCOL} used
    in tests. It sets all types to [unit] and implement all functions
    as [assert false]. Users of this module should [include] it
    and override the functions they need. *)
  module Mock_all_unit :
    Tezos_protocol_environment.PROTOCOL
      with type block_header_data = unit
       and type operation_data = unit
       and type operation_receipt = unit
       and type validation_state = unit = struct
    type nonrec validation_state = unit

    type block_header_data = unit

    type operation = {
      shell : Tezos_base.Operation.shell_header;
      protocol_data : block_header_data;
    }

    type operation_receipt = unit

    type operation_data = unit

    type block_header_metadata = unit

    type block_header = {
      shell : Tezos_base.Block_header.shell_header;
      protocol_data : block_header_data;
    }

    let environment_version = Protocol.V0

    let init _ = assert false

    let rpc_services = RPC_directory.empty

    let finalize_block _ = assert false

    let apply_operation _ = assert false

    let begin_construction ~chain_id:_ ~predecessor_context:_
        ~predecessor_timestamp:_ ~predecessor_level:_ ~predecessor_fitness:_
        ~predecessor:_ ~timestamp:_ ?protocol_data:_ ~cache:_ _ =
      assert false

    let begin_application ~chain_id:_ ~predecessor_context:_
        ~predecessor_timestamp:_ ~predecessor_fitness:_ ~cache:_ _ =
      assert false

    let begin_partial_application ~chain_id:_ ~ancestor_context:_ ~predecessor:_
        ~predecessor_hash:_ ~cache:_ _ =
      assert false

    let relative_position_within_block _ = assert false

    let acceptable_passes _ = assert false

    let operation_data_and_receipt_encoding =
      Data_encoding.conv (Fun.const ()) (Fun.const ((), ())) Data_encoding.unit

    let operation_receipt_encoding = Data_encoding.unit

    let operation_data_encoding = Data_encoding.unit

    let block_header_metadata_encoding = Data_encoding.unit

    let block_header_data_encoding = Data_encoding.unit

    let validation_passes = []

    (* Size of the shell header + size of the encoding of {!block_header_data}
       (unit, i.e. 0). *)
    let max_operation_data_length = 32

    let max_block_length = 0

    let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
        ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_
        =
      assert false

    let set_log_message_consumer _ = ()
  end
end
