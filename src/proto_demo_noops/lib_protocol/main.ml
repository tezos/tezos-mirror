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

let acceptable_pass _op = None

type block_header_data = string

let block_header_data_encoding =
  Data_encoding.(obj1 (req "block_header_data" (string Plain)))

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

type block_header_metadata = unit

let block_header_metadata_encoding_with_legacy_attestation_name =
  Data_encoding.unit

let block_header_metadata_encoding = Data_encoding.unit

type operation_data = unit

let operation_data_encoding = Data_encoding.unit

let operation_data_encoding_with_legacy_attestation_name =
  operation_data_encoding

type operation_receipt = unit

let operation_receipt_encoding = Data_encoding.unit

let operation_receipt_encoding_with_legacy_attestation_name =
  operation_receipt_encoding

let operation_data_and_receipt_encoding =
  Data_encoding.conv
    (function ((), ()) -> ())
    (fun () -> ((), ()))
    Data_encoding.unit

let operation_data_and_receipt_encoding_with_legacy_attestation_name =
  operation_data_and_receipt_encoding

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let compare_operations _ _ = 0

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

(* Same as genesis *)
let fitness_from_level level =
  let version_number = "\002" in
  let int32_to_bytes i =
    let b = Bytes.make 4 '\000' in
    TzEndian.set_int32 b 0 i ;
    b
  in
  [
    Bytes.of_string version_number;
    int32_to_bytes level ;
    Bytes.empty ;
    int32_to_bytes (-1l) ;
    int32_to_bytes 0l ;
  ]

let begin_validation context _chain_id mode
    ~(predecessor : Block_header.shell_header) =
  let fitness =
    match mode with
    | Application block_header | Partial_validation block_header ->
        block_header.shell.fitness
    | Construction _ | Partial_construction _ ->
        fitness_from_level Int32.(succ predecessor.level)
  in
  return {context; fitness}

let begin_application = begin_validation

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

let validate_operation ?check_signature:_ _state _oph _op = tzfail No_error

let apply_operation _state _oph _op = tzfail No_error

let finalize_validation _state = return_unit

let finalize_application application_state _shell_header =
  return
    ( {
        Updater.message = None;
        context = application_state.context;
        fitness = application_state.fitness;
        max_operations_ttl = 0;
        last_allowed_fork_level = 0l;
      },
      () )

let init _chain_id context block_header =
  let open Lwt_result_syntax in
  let open Block_header in
  let fitness = block_header.fitness in
  let*! context = Context.Cache.set_cache_layout context [] in
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
  return (fun _ -> tzfail No_error)

let rpc_services = RPC_directory.empty

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

  let init _ _ ~head_hash:_ ~head:_  = Lwt.return_ok ((), ())

  let encoding = Data_encoding.unit

  let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
    Lwt.return_ok ((), Unchanged)

  let remove_operation () _ = ()

  let merge ?conflict_handler:_ () () = Ok ()

  let operations () = Operation_hash.Map.empty
end
