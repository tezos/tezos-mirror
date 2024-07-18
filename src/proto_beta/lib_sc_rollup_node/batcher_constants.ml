(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

let message_size_limit = Protocol.Constants_repr.sc_rollup_message_size_limit

let protocol_max_batch_size =
  let open Protocol in
  let open Alpha_context in
  let empty_message_op : _ Operation.t =
    let open Operation in
    {
      shell = {branch = Block_hash.zero};
      protocol_data =
        {
          signature = Some Signature.zero;
          contents =
            Single
              (Manager_operation
                 {
                   source = Signature.Public_key_hash.zero;
                   fee = Tez.of_mutez_exn Int64.max_int;
                   counter = Manager_counter.Internal_for_tests.of_int max_int;
                   gas_limit =
                     Gas.Arith.integral_of_int_exn ((max_int - 1) / 1000);
                   storage_limit = Z.of_int max_int;
                   operation = Sc_rollup_add_messages {messages = [""]};
                 });
        };
    }
  in
  Protocol.Constants_repr.max_operation_data_length
  - Data_encoding.Binary.length
      Operation.encoding
      (Operation.pack empty_message_op)
