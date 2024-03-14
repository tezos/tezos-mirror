(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
  internal_events : Tezos_base.Internal_event_config.t;
}

type never = |

type _ request =
  | Validate : {
      chain_id : Chain_id.t;
      block_header : Block_header.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      predecessor_resulting_context_hash : Context_hash.t;
      operations : Block_validation.operation list list;
      max_operations_ttl : int;
      should_precheck : bool;
      simulate : bool;
    }
      -> Block_validation.result request
  | Preapply : {
      chain_id : Chain_id.t;
      timestamp : Time.Protocol.t;
      protocol_data : bytes;
      live_blocks : Block_hash.Set.t;
      live_operations : Operation_hash.Set.t;
      predecessor_shell_header : Block_header.shell_header;
      predecessor_hash : Block_hash.t;
      predecessor_max_operations_ttl : int;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      predecessor_resulting_context_hash : Context_hash.t;
      operations : Block_validation.operation list list;
    }
      -> (Block_header.shell_header * error Preapply_result.t list) request
  | Precheck : {
      chain_id : Chain_id.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_hash : Block_hash.t;
      predecessor_resulting_context_hash : Context_hash.t;
      header : Block_header.t;
      operations : Block_validation.operation list list;
      hash : Block_hash.t;
    }
      -> unit request
  | Commit_genesis : {chain_id : Chain_id.t} -> Context_hash.t request
  | Fork_test_chain : {
      chain_id : Chain_id.t;
      context_hash : Context_hash.t;
      forked_header : Block_header.t;
    }
      -> Block_header.t request
  | Context_garbage_collection : {
      context_hash : Context_hash.t;
      gc_lockfile_path : string;
    }
      -> unit request
  | Context_split : unit request
  | Terminate : never request
  | Reconfigure_event_logging :
      Tezos_base_unix.Internal_event_unix.Configuration.t
      -> unit request

val request_pp : Format.formatter -> 'a request -> unit

val magic : Bytes.t

val parameters_encoding : parameters Data_encoding.t

type packed_request = Erequest : _ request -> packed_request

val request_encoding : packed_request Data_encoding.t

val result_encoding : 'a request -> 'a Data_encoding.t

val send : Lwt_io.output_channel -> 'a Data_encoding.t -> 'a -> unit Lwt.t

val recv : Lwt_io.input_channel -> 'a Data_encoding.t -> 'a Lwt.t

val recv_result :
  Lwt_io.input_channel -> 'a Data_encoding.t -> 'a tzresult Lwt.t

(** The prefix for the validation socket filename.

    Do not use it directly except for documentation purposes; use
    [socket_path] instead. *)
val socket_path_prefix : string

(** Get the validation socket path.

    [socket_dir] is the directory where the file should be put.
    [pid] is the process ID of the validator process. *)
val socket_path : socket_dir:string -> pid:int -> string

val create_socket_listen :
  canceler:Lwt_canceler.t ->
  max_requests:int ->
  socket_path:string ->
  Lwt_unix.file_descr tzresult Lwt.t

val create_socket_connect :
  canceler:Lwt_canceler.t ->
  socket_path:string ->
  Lwt_unix.file_descr tzresult Lwt.t
