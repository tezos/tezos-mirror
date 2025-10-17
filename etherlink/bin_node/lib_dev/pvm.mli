(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2025 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Stripped down signature of a Layer 2 store context *)
module type S = sig
  type repo

  type tree

  type 'a index = ('a, repo) Context_sigs.index

  (** Read/write {!type:index}. *)
  type rw_index = [`Read | `Write] index

  (** Read only {!type:index}. *)
  type ro_index = [`Read] index

  type 'a t = ('a, repo, tree) Context_sigs.t

  (** Read/write context {!t}. *)
  type rw = [`Read | `Write] t

  (** A context hash is the hash produced when the data of the context is
      committed to disk, i.e. the {!type:commit} hash. *)
  type hash

  val equality_witness : (repo, tree) Context_sigs.equality_witness

  (** [load cache_size path] initializes from disk a context from [path].
      [cache_size] allows to change the LRU cache size of Irmin
      (100_000 by default at irmin-pack/config.ml *)
  val load :
    cache_size:int ->
    ?async_domain:bool ->
    'a Access_mode.t ->
    string ->
    'a index tzresult Lwt.t

  val reload : ro_index -> unit

  (** [commit ?message context] commits content of the context [context] on disk,
      and return the commit hash. *)
  val commit : ?message:string -> [> `Write] t -> hash Lwt.t

  val checkout_exn : 'a index -> hash -> 'a t Lwt.t

  (** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
  val empty : 'a index -> 'a t

  (** [split ctxt] creates a new suffix file, also called "chunk", into the
      Irmin's file hierarchy. This split function is expected to be called after
      committing a commit that will be a future candidate for a GC target.  *)
  val split : _ index -> unit

  (** [gc index ?callback hash] removes all data older than [hash] from disk.
      If passed, [callback] will be executed when garbage collection finishes. *)
  val gc :
    [> `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

  (** [wait_gc_completion index] will return a blocking thread if a
      GC run is currently ongoing. *)
  val wait_gc_completion : [> `Write] index -> unit Lwt.t

  val context_hash_of_hash : hash -> Smart_rollup_context_hash.t

  val hash_of_context_hash : Smart_rollup_context_hash.t -> hash

  (** State of the PVM that the EVM node handles *)
  module PVMState : sig
    (** The value of a PVM state *)
    type value = tree

    (** [empty ()] is the empty PVM state. *)
    val empty : unit -> value

    val get : _ t -> value Lwt.t

    (** [set context state] saves the PVM state [state] in the context and returns
        the updated context. Note: [set] does not perform any write on disk, this
        information must be committed using {!val:commit}. *)
    val set : 'a t -> value -> 'a t Lwt.t
  end
end

(** This module defines the signature of an abstract PVM durable storage backend
  * type which can be parameterised with a concrete implementation `S` *)
module Context : sig
  type ('repo, 'tree) impl =
    (module S with type repo = 'repo and type tree = 'tree)

  (** The type of trees stored in the context, i.e. the actual data. *)
  type tree = private
    | Tree : {tree : 'tree; impl : ('repo, 'tree) impl} -> tree

  (** The type of indexed repository for contexts. The parameter indicates if the
      index can be written or only read. *)
  type 'a index = private
    | Index : {
        index : ('a, 'repo) Context_sigs.index;
        impl : ('repo, 'tree) impl;
      }
        -> 'a index

  (** Read/write {!type:index}. *)
  type rw_index = [`Read | `Write] index

  (** Read only {!type:index}. *)
  type ro_index = [`Read] index

  (** The type of context with its content. *)
  type 'a t = private
    | Context : {
        index : ('a, 'repo) Context_sigs.index;
        tree : 'tree;
        impl : ('repo, 'tree) impl;
      }
        -> 'a t

  (** Read/write context {!t}. *)
  type rw = [`Read | `Write] t

  (** A context hash is the hash produced when the data of the context is
      committed to disk, i.e. the {!type:commit} hash. *)
  type hash = Smart_rollup_context_hash.t

  (** [load cache_size path] initializes from disk a context from [path].
      [cache_size] allows to change the LRU cache size of Irmin
      (100_000 by default at irmin-pack/config.ml *)
  val load :
    ('repo, 'tree) impl ->
    cache_size:int ->
    ?async_domain:bool ->
    ([< `Read | `Write > `Read] as 'a) Access_mode.t ->
    string ->
    'a index tzresult Lwt.t

  val reload : ro_index -> unit

  (** [commit ?message context] commits content of the context [context] on disk,
      and return the commit hash. *)
  val commit : ?message:string -> [> `Write] t -> hash Lwt.t

  val checkout_exn : 'a index -> hash -> 'a t Lwt.t

  (** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
  val empty : 'a index -> 'a t

  (** [split ctxt] creates a new suffix file, also called "chunk", into the
      Irmin's file hierarchy. This split function is expected to be called after
      committing a commit that will be a future candidate for a GC target.  *)
  val split : _ index -> unit

  (** [gc index ?callback hash] removes all data older than [hash] from disk.
      If passed, [callback] will be executed when garbage collection finishes. *)
  val gc :
    [> `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

  (** [wait_gc_completion index] will return a blocking thread if a
      GC run is currently ongoing. *)
  val wait_gc_completion : [> `Write] index -> unit Lwt.t
end

(** State of the PVM that this rollup node deals with *)
module State : sig
  (** The value of a PVM state *)
  type t = Context.tree

  (** [empty ()] is the empty PVM state. *)
  val empty : ('repo, 'tree) Context.impl -> unit -> t

  val get : _ Context.t -> t Lwt.t

  (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!val:commit}. *)
  val set : 'a Context.t -> t -> 'a Context.t Lwt.t
end

(* TODO TZX-24: Only make `Wasm_internal` available when node is instantiated
 * with the Wasm PVM *)

(** Access to underlying Irmin tree for components which cannot be abstract
   over the PVM durable storage backend type. *)
module Wasm_internal : sig
  val to_irmin_exn : Context.tree -> Irmin_context.tree

  val of_irmin : Irmin_context.tree -> Context.tree
end

module Kernel : sig
  (** Constructs a configuration for rollup execution. *)
  val config :
    ?sender:Tezos_protocol_alpha.Protocol.Contract_hash.t ->
    ?source:Signature.Public_key_hash.t ->
    ?destination:Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Address.t ->
    ?preimage_directory:string ->
    ?preimage_endpoint:Uri.t ->
    ?dal_pages_directory:string ->
    ?kernel_debug:bool ->
    ?flamecharts_directory:string ->
    ?timings_file:string ->
    ?trace_host_funs:bool ->
    unit ->
    Pvm_types.config

  (** [read_kernel kernel] returns a tuple consisting of the kernel code
      [content] and a boolean [is_binary], where [is_binary] is [true] if
      [content] is a WASM blob, and [false] if it is a wat file (WebAssembly text
      format). *)
  val read_kernel : Pvm_types.kernel -> (string * bool) tzresult Lwt.t

  val check_kernel :
    binary:bool ->
    name:string ->
    Tezos_scoru_wasm.Wasm_pvm_state.version ->
    string ->
    unit tzresult Lwt.t

  val set_durable_value :
    ?edit_readonly:bool -> State.t -> string -> string -> State.t Lwt.t

  val start :
    tree:State.t ->
    Tezos_scoru_wasm.Wasm_pvm_state.version ->
    Pvm_types.kernel ->
    State.t tzresult Lwt.t

  val find_key_in_durable :
    State.t ->
    Tezos_scoru_wasm.Durable.key ->
    Tezos_lazy_containers.Chunked_byte_vector.t option Lwt.t

  val wrap_as_durable_storage :
    State.t -> Tezos_webassembly_interpreter.Durable_storage.t Lwt.t

  val eval :
    ?hooks:Tezos_scoru_wasm.Hooks.t ->
    ?migrate_to:Tezos_scoru_wasm.Pvm_input_kind.protocol ->
    write_debug:Tezos_scoru_wasm.Builtins.write_debug ->
    wasm_entrypoint:string ->
    int32 ->
    string trace Seq.t ->
    Pvm_types.config ->
    Octez_smart_rollup_wasm_debugger_lib.Commands.eval_step ->
    State.t ->
    (State.t * int64 * string trace Seq.t * int32) tzresult Lwt.t

  val profile :
    ?migrate_to:Tezos_scoru_wasm.Pvm_input_kind.protocol ->
    ?hooks:Tezos_scoru_wasm.Hooks.t ->
    collapse:bool ->
    with_time:bool ->
    no_reboot:bool ->
    int32 ->
    string trace Seq.t ->
    Pvm_types.config ->
    string Octez_smart_rollup_wasm_debugger_lib.Custom_section.FuncMap.t ->
    State.t ->
    (State.t * string trace Seq.t * int32) tzresult Lwt.t

  val encode :
    Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state ->
    State.t ->
    State.t Lwt.t

  val decode :
    State.t -> Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state Lwt.t

  val get_wasm_version :
    State.t -> Tezos_scoru_wasm.Wasm_pvm_state.version Lwt.t

  val get_function_symbols :
    State.t ->
    string Octez_smart_rollup_wasm_debugger_lib.Custom_section.FuncMap.t
    tzresult
    Lwt.t
end
