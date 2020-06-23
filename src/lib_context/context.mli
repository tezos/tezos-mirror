(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
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

(** Tezos - Versioned, block indexed (key x value) store *)

(** {2 Generic interface} *)

module type S = sig
  (** @inline *)
  include Tezos_context_sigs.Context.S
end

include S

type context = t

(** A block-indexed (key x value) store directory.  *)
type index

(** Open or initialize a versioned store at a given path. *)
val init :
  ?patch_context:(context -> context tzresult Lwt.t) ->
  ?mapsize:int64 ->
  ?readonly:bool ->
  string ->
  index Lwt.t

(** Close the index. Does not fail when the context is already closed. *)
val close : index -> unit Lwt.t

(** Sync the context with disk. Only useful for read-only instances.
    Does not fail when the context is not in read-only mode. *)
val sync : index -> unit Lwt.t

val compute_testchain_chain_id : Block_hash.t -> Chain_id.t

val compute_testchain_genesis : Block_hash.t -> Block_hash.t

val commit_genesis :
  index ->
  chain_id:Chain_id.t ->
  time:Time.Protocol.t ->
  protocol:Protocol_hash.t ->
  Context_hash.t tzresult Lwt.t

val commit_test_chain_genesis :
  context -> Block_header.t -> Block_header.t Lwt.t

(** {2 Accessing and Updating Versions} *)

(** [restore_integrity ppf index] attempts to restore the context
    integrity of [index]. Returns [None] when nothing needs to be fixed and
    [Some n] with [n] the number of entries fixed. If needs be, the
    progress might be printed via [ppf]
    If the context integrity cannot be restored, [Failure msg] is thrown. *)
val restore_integrity : ?ppf:Format.formatter -> index -> int option tzresult

val exists : index -> Context_hash.t -> bool Lwt.t

val checkout : index -> Context_hash.t -> context option Lwt.t

val checkout_exn : index -> Context_hash.t -> context Lwt.t

val hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t

val commit :
  time:Time.Protocol.t -> ?message:string -> context -> Context_hash.t Lwt.t

val set_head : index -> Chain_id.t -> Context_hash.t -> unit Lwt.t

val set_master : index -> Context_hash.t -> unit Lwt.t

(** {2 Predefined Fields} *)

val get_protocol : context -> Protocol_hash.t Lwt.t

val add_protocol : context -> Protocol_hash.t -> context Lwt.t

val get_test_chain : context -> Test_chain_status.t Lwt.t

val add_test_chain : context -> Test_chain_status.t -> context Lwt.t

val remove_test_chain : context -> context Lwt.t

val fork_test_chain :
  context ->
  protocol:Protocol_hash.t ->
  expiration:Time.Protocol.t ->
  context Lwt.t

val clear_test_chain : index -> Chain_id.t -> unit Lwt.t

val find_predecessor_block_metadata_hash :
  context -> Block_metadata_hash.t option Lwt.t

val add_predecessor_block_metadata_hash :
  context -> Block_metadata_hash.t -> context Lwt.t

val find_predecessor_ops_metadata_hash :
  context -> Operation_metadata_list_list_hash.t option Lwt.t

val add_predecessor_ops_metadata_hash :
  context -> Operation_metadata_list_list_hash.t -> context Lwt.t

(** {2 Context dumping} *)

module Pruned_block : sig
  type t = {
    block_header : Block_header.t;
    operations : (int * Operation.t list) list;
    operation_hashes : (int * Operation_hash.t list) list;
  }

  val encoding : t Data_encoding.t

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t option
end

module Block_data : sig
  type t = {block_header : Block_header.t; operations : Operation.t list list}

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t option

  val encoding : t Data_encoding.t
end

module Protocol_data : sig
  type t = Int32.t * data

  and info = {author : string; message : string; timestamp : Time.Protocol.t}

  and data = {
    predecessor_block_metadata_hash : Block_metadata_hash.t option;
    predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
    info : info;
    protocol_hash : Protocol_hash.t;
    test_chain_status : Test_chain_status.t;
    data_key : Context_hash.t;
    parents : Context_hash.t list;
  }

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t option

  val encoding : t Data_encoding.t

  val encoding_1_0_0 : t Data_encoding.t
end

val get_protocol_data_from_header :
  index -> Block_header.t -> Protocol_data.t Lwt.t

val dump_contexts :
  index ->
  Block_header.t
  * Block_data.t
  * Block_metadata_hash.t option
  * Operation_metadata_hash.t list list option
  * History_mode.Legacy.t
  * (Block_header.t ->
    (Pruned_block.t option * Protocol_data.t option) tzresult Lwt.t) ->
  filename:string ->
  unit tzresult Lwt.t

val restore_contexts :
  index ->
  filename:string ->
  ((Block_hash.t * Pruned_block.t) list -> unit tzresult Lwt.t) ->
  (Block_header.t option ->
  Block_hash.t ->
  Pruned_block.t ->
  unit tzresult Lwt.t) ->
  ( Block_header.t
  * Block_data.t
  * Block_metadata_hash.t option
  * Operation_metadata_hash.t list list option
  * History_mode.Legacy.t
  * Block_header.t option
  * Block_hash.t list
  * Protocol_data.t list )
  tzresult
  Lwt.t

val validate_context_hash_consistency_and_commit :
  data_hash:Context_hash.t ->
  expected_context_hash:Context_hash.t ->
  timestamp:Time.Protocol.t ->
  test_chain:Test_chain_status.t ->
  protocol_hash:Protocol_hash.t ->
  message:string ->
  author:string ->
  parents:Context_hash.t list ->
  predecessor_block_metadata_hash:Block_metadata_hash.t option ->
  predecessor_ops_metadata_hash:Operation_metadata_list_list_hash.t option ->
  index:index ->
  bool Lwt.t

val upgrade_0_0_3 : context_dir:string -> unit tzresult Lwt.t

(** Offline integrity checking and statistics for contexts. *)
module Checks : sig
  module Pack : Irmin_pack.Checks.S

  module Index : Index.Checks.S
end
