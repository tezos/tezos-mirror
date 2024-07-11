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

(** An implementation of {!Tezos_protocol_environment.PROTOCOL} used
    in tests. It sets all types to [unit] and implement all functions
    as [assert false]. Users of this module should [include] it
    and override the functions they need. *)
module Mock_all_unit :
  Environment_protocol_T.PROTOCOL
    with type block_header_data = unit
     and type operation_data = unit
     and type operation_receipt = unit
     and type validation_state = unit
     and type application_state = unit
     and type Mempool.t = unit
     and type Mempool.validation_info = unit = struct
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

  let expected_context_hash = Environment_context.Resulting_context

  let init _ = assert false

  type nonrec validation_state = unit

  type nonrec application_state = unit

  type mode =
    | Application of block_header
    | Partial_validation of block_header
    | Construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
        block_header_data : block_header_data;
      }
    | Partial_construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
      }

  let begin_validation _ = assert false

  let validate_operation ?check_signature:_ = assert false

  let finalize_validation _ = assert false

  let begin_application _ = assert false

  let apply_operation _ = assert false

  let finalize_application _ = assert false

  let rpc_services = Tezos_rpc.Directory.empty

  let compare_operations _ = assert false

  let acceptable_pass _ = assert false

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
      ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
    assert false

  let set_log_message_consumer _ = ()

  module Mempool = struct
    type t = unit

    type validation_info = unit

    type conflict_handler =
      existing_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      new_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      [`Keep | `Replace]

    type operation_conflict =
      | Operation_conflict of {
          existing : Tezos_crypto.Hashed.Operation_hash.t;
          new_operation : Tezos_crypto.Hashed.Operation_hash.t;
        }

    type add_result =
      | Added
      | Replaced of {removed : Tezos_crypto.Hashed.Operation_hash.t}
      | Unchanged

    type add_error =
      | Validation_error of error trace
      | Add_conflict of operation_conflict

    type merge_error =
      | Incompatible_mempool
      | Merge_conflict of operation_conflict

    let init _ _ ~head_hash:_ ~head:_ ~cache:_ = assert false

    let encoding = Data_encoding.unit

    let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
      assert false

    let remove_operation () _ = assert false

    let merge ?conflict_handler:_ () () = assert false

    let operations () = assert false
  end
end
