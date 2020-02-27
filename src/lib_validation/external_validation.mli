(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type parameters = {
  context_root : string;
  protocol_root : string;
  genesis : Genesis.t;
  sandbox_parameters : Data_encoding.json option;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
}

type request =
  | Init
  | Validate of {
      chain_id : Chain_id.t;
      block_header : Block_header.t;
      predecessor_block_header : Block_header.t;
      operations : Operation.t list list;
      max_operations_ttl : int;
    }
  | Commit_genesis of {chain_id : Chain_id.t}
  | Fork_test_chain of {
      context_hash : Context_hash.t;
      forked_header : Block_header.t;
    }
  | Terminate
  | Restore_context_integrity

val request_pp : Format.formatter -> request -> unit

val magic : MBytes.t

val parameters_encoding : parameters Data_encoding.t

val request_encoding : request Data_encoding.t

val send : Lwt_io.output_channel -> 'a Data_encoding.t -> 'a -> unit Lwt.t

val recv : Lwt_io.input_channel -> 'a Data_encoding.t -> 'a Lwt.t

val recv_result :
  Lwt_io.input_channel -> 'a Data_encoding.t -> 'a tzresult Lwt.t
