(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type error +=
  | Cannot_create_file of string
  | Cannot_open_file of string
  | Cannot_find_protocol
  | Suspicious_file of int

(** Tezos - Versioned, block indexed (key x value) store *)
module Make (Encoding : module type of Tezos_context_encoding.Context) : sig
  (** {2 Generic interface} *)

  module type S = sig
    (** @inline *)
    include Tezos_context_sigs.Context.S
  end

  (** A block-indexed (key x value) store directory.  *)
  type index

  include S with type index := index

  type context = t

  val index : context -> index

  (** Open or initialize a versioned store at a given path.

      @param indexing_strategy determines whether newly-exported objects by
      this store handle should also be added to the store's index. [`Minimal]
      (the default) only adds objects to the index when they are {i commits},
      whereas [`Always] indexes every object type. The indexing strategy used
      for existing stores can be changed without issue (as only {i
      newly}-exported objects are impacted). *)
  val init :
    ?patch_context:(context -> context tzresult Lwt.t) ->
    ?readonly:bool ->
    ?indexing_strategy:[`Always | `Minimal] ->
    string ->
    index Lwt.t

  (** Close the index. Does not fail when the context is already closed. *)
  val close : index -> unit Lwt.t

  (** Sync the context with disk. Only useful for read-only instances.
    Does not fail when the context is not in read-only mode. *)
  val sync : index -> unit Lwt.t

  val flush : t -> t Lwt.t

  val compute_testchain_chain_id : Block_hash.t -> Chain_id.t

  val compute_testchain_genesis : Block_hash.t -> Block_hash.t

  (** Build an empty context from an index. The resulting context should not
      be committed. *)
  val empty : index -> t

  (** Returns [true] if the context is empty. *)
  val is_empty : t -> bool

  val commit_genesis :
    index ->
    chain_id:Chain_id.t ->
    time:Time.Protocol.t ->
    protocol:Protocol_hash.t ->
    Context_hash.t tzresult Lwt.t

  val commit_test_chain_genesis :
    context -> Block_header.t -> Block_header.t Lwt.t

  (** Extract a subtree from the {!Tezos_context.Context.t} argument and returns
      it as a {!Tezos_context_memory.Context.tree} (note the the type change!). **)
  val to_memory_tree :
    t -> string list -> Tezos_context_memory.Context.tree option Lwt.t

  (** [merkle_tree t leaf_kind key] returns a Merkle proof for [key] (i.e.
    whose hashes reach [key]). If [leaf_kind] is [Block_services.Hole], the value
    at [key] is a hash. If [leaf_kind] is [Block_services.Raw_context],
    the value at [key] is a [Block_services.raw_context]. Values higher
    in the returned tree are hashes of the siblings on the path to
    reach [key]. *)
  val merkle_tree :
    t ->
    Block_services.merkle_leaf_kind ->
    key ->
    Block_services.merkle_tree Lwt.t

  (** {2 Accessing and Updating Versions} *)

  val exists : index -> Context_hash.t -> bool Lwt.t

  val checkout : index -> Context_hash.t -> context option Lwt.t

  val checkout_exn : index -> Context_hash.t -> context Lwt.t

  val hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t

  val commit :
    time:Time.Protocol.t -> ?message:string -> context -> Context_hash.t Lwt.t

  val set_head : index -> Chain_id.t -> Context_hash.t -> unit Lwt.t

  val set_master : index -> Context_hash.t -> unit Lwt.t

  (** {2 Hash version} *)

  (** Get the hash version used for the context *)
  val get_hash_version : context -> Context_hash.Version.t

  (** Set the hash version used for the context.  It may recalculate the hashes
    of the whole context, which can be a long process.
    Returns an [Error] if the hash version is unsupported. *)
  val set_hash_version :
    context -> Context_hash.Version.t -> context tzresult Lwt.t

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

  val dump_context :
    index -> Context_hash.t -> fd:Lwt_unix.file_descr -> int tzresult Lwt.t

  (** Rebuild a context from a given snapshot. *)
  val restore_context :
    index ->
    expected_context_hash:Context_hash.t ->
    nb_context_elements:int ->
    fd:Lwt_unix.file_descr ->
    legacy:bool ->
    unit tzresult Lwt.t

  val retrieve_commit_info :
    index ->
    Block_header.t ->
    (Protocol_hash.t
    * string
    * string
    * Time.Protocol.t
    * Test_chain_status.t
    * Context_hash.t
    * Block_metadata_hash.t option
    * Operation_metadata_list_list_hash.t option
    * Context_hash.t list)
    tzresult
    Lwt.t

  val check_protocol_commit_consistency :
    expected_context_hash:Context_hash.t ->
    given_protocol_hash:Protocol_hash.t ->
    author:string ->
    message:string ->
    timestamp:Time.Protocol.t ->
    test_chain_status:Test_chain_status.t ->
    predecessor_block_metadata_hash:Block_metadata_hash.t option ->
    predecessor_ops_metadata_hash:Operation_metadata_list_list_hash.t option ->
    data_merkle_root:Context_hash.t ->
    parents_contexts:Context_hash.t list ->
    bool Lwt.t

  (** Offline integrity checking and statistics for contexts. *)
  module Checks : sig
    module Pack : Irmin_pack_unix.Checks.S

    module Index : Index.Checks.S
  end
end
