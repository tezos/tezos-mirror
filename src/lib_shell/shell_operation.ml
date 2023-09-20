(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type 'protocol_operation operation = {
  hash : Operation_hash.t;
  raw : Operation.t;
  protocol : 'protocol_operation;
  signature_checked : bool;
  size : int;
}

let record_successful_signature_check op = {op with signature_checked = true}

(** Doesn't depend on heavy [Registered_protocol.T] for testability. *)
let safe_binary_of_bytes (encoding : 'a Data_encoding.t) (bytes : bytes) :
    'a tzresult =
  let open Result_syntax in
  match Data_encoding.Binary.of_bytes_opt encoding bytes with
  | None -> tzfail Validation_errors.Parse_error
  | Some protocol_data -> return protocol_data

module type PARSER = sig
  type protocol_operation

  val parse :
    Operation_hash.t -> Operation.t -> protocol_operation operation tzresult
end

module MakeParser (Proto : Tezos_protocol_environment.PROTOCOL) :
  PARSER with type protocol_operation = Proto.operation = struct
  type protocol_operation = Proto.operation

  let parse_unsafe (proto : bytes) : Proto.operation_data tzresult =
    safe_binary_of_bytes
      Proto.operation_data_encoding_with_legacy_attestation_name
      proto

  let parse hash (raw : Operation.t) =
    let open Result_syntax in
    let size = Data_encoding.Binary.length Operation.encoding raw in
    if size > Proto.max_operation_data_length then
      tzfail
        (Validation_errors.Oversized_operation
           {size; max = Proto.max_operation_data_length})
    else
      let+ protocol_data = parse_unsafe raw.proto in
      {
        hash;
        raw;
        protocol = {Proto.shell = raw.Operation.shell; protocol_data};
        signature_checked = false;
        size;
      }
end

module Internal_for_tests = struct
  let make_operation ?(signature_checked = false) ?(size = 0) hash raw protocol
      =
    {hash; raw; protocol; signature_checked; size}

  let safe_binary_of_bytes = safe_binary_of_bytes
end
