(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  type repo

  type tree

  type 'a index = ('a, repo) Context_sigs.index

  (** Read/write {!type:index}. *)
  type rw_index = [`Read | `Write] index

  (** Read only {!type:index}. *)
  type ro_index = [`Read] index

  type 'a t = {index : 'a index; tree : tree}

  (** Read/write context {!t}. *)
  type rw = [`Read | `Write] t

  (** A context hash is the hash produced when the data of the context is
      committed to disk, i.e. the {!type:commit} hash. *)
  type hash

  val equality_witness :
    repo Context_sigs.Equality_witness.t * tree Context_sigs.Equality_witness.t

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

module Context = struct
  type ('repo, 'tree) impl =
    (module S with type repo = 'repo and type tree = 'tree)

  type tree = Tree : {tree : 'tree; impl : ('repo, 'tree) impl} -> tree

  type 'a index =
    | Index : {
        index : ('a, 'repo) Context_sigs.index;
        impl : ('repo, 'tree) impl;
      }
        -> 'a index

  type rw_index = [`Read | `Write] index

  type ro_index = [`Read] index

  type 'a t =
    | Context : {
        index : ('a, 'repo) Context_sigs.index;
        tree : 'tree;
        impl : ('repo, 'tree) impl;
      }
        -> 'a t

  type rw = [`Read | `Write] t

  type hash = Smart_rollup_context_hash.t

  let make_index ~index ~impl = Index {index; impl}

  let make_ctxt ~index ~tree ~impl = Context {index; tree; impl}

  let make_tree ~tree ~impl = Tree {tree; impl}

  let equiv (a, b) (c, d) =
    (Context_sigs.Equality_witness.eq a c, Context_sigs.Equality_witness.eq b d)

  let load : type tree repo.
      (repo, tree) impl ->
      cache_size:int ->
      ?async_domain:bool ->
      'a Access_mode.t ->
      string ->
      'a index tzresult Lwt.t =
   fun (module Impl) ~cache_size ?async_domain mode path ->
    let open Lwt_result_syntax in
    let* index = Impl.load ~cache_size ?async_domain mode path in
    return @@ make_index ~index ~impl:(module Impl)

  let reload (Index {index; impl = (module Impl)} : [< `Read] index) =
    Impl.reload index

  let commit : type a. ?message:string -> a t -> hash Lwt.t =
   fun ?message (Context {index; tree; impl = (module Impl)}) ->
    let open Lwt_syntax in
    let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
    let* hash = Impl.commit ?message {index; tree} in
    let hash = Impl.context_hash_of_hash hash in
    return hash

  let checkout_exn : type a. a index -> hash -> a t Lwt.t =
   fun (Index {index; impl = (module Impl)}) hash ->
    let open Lwt_syntax in
    let hash = Impl.hash_of_context_hash hash in
    let* {index; tree} = Impl.checkout_exn index hash in
    return @@ make_ctxt ~index ~tree ~impl:(module Impl)

  let empty : type a. a index -> a t =
   fun (Index {index; impl = (module Impl)}) ->
    let empty = Impl.empty index in
    make_ctxt ~index:empty.index ~tree:empty.tree ~impl:(module Impl)

  let split : type a. a index -> unit =
   fun (Index {index; impl = (module Impl)}) -> Impl.split index

  let gc : type a.
      a index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t =
   fun (Index {index; impl = (module Impl)}) ?callback hash ->
    let hash = Impl.hash_of_context_hash hash in
    let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
    Impl.gc index ?callback hash

  let wait_gc_completion : type a. a index -> unit Lwt.t =
   fun (Index {index; impl = (module Impl)}) ->
    let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
    Impl.wait_gc_completion index
end

module State = struct
  type t = Context.tree

  let empty : type tree repo. (repo, tree) Context.impl -> unit -> t =
   fun (module Impl) () ->
    let tree = Impl.PVMState.empty () in
    Context.make_tree ~tree ~impl:(module Impl)

  let get : type a. a Context.t -> t Lwt.t =
   fun (Context {index; tree; impl = (module Impl)}) ->
    let open Lwt_syntax in
    let* tree = Impl.PVMState.get {index; tree} in
    return @@ Context.make_tree ~tree ~impl:(module Impl)

  let set : type a. a Context.t -> t -> a Context.t Lwt.t =
   fun (Context {index; tree; impl = (module Impl)})
       (Tree ({impl = (module ImplT); _} as t)) ->
    let open Lwt_syntax in
    match Context.equiv Impl.equality_witness ImplT.equality_witness with
    | Some Refl, Some Refl ->
        let* ctxt = Impl.PVMState.set {index; tree} t.tree in
        return
        @@ Context.make_ctxt
             ~index:ctxt.index
             ~tree:ctxt.tree
             ~impl:(module Impl)
    | _ -> raise (Invalid_argument "Implementation mismatch")
end

module Irmin_context :
  S
    with type repo = Irmin_context.repo
     and type tree = Irmin_context.tree
     and type 'a index = 'a Irmin_context.index = struct
  include Irmin_context

  type 'a t = {index : 'a index; tree : tree}

  type rw = [`Read | `Write] t

  let equality_witness =
    let repo_eq_w, tree_eq_w, _ = Irmin_context.equality_witness in
    (repo_eq_w, tree_eq_w)

  let commit ?message {index; tree} =
    Irmin_context.commit ?message {index; state = ref tree}

  let checkout_exn index hash =
    let open Lwt_syntax in
    let+ {index; state} = Irmin_context.checkout_exn index hash in
    {index; tree = !state}

  let empty index =
    let {Context_sigs.index; state} = Irmin_context.empty index in
    {index; tree = !state}

  module PVMState = struct
    type value = Irmin_context.tree

    let empty () = !(Irmin_context.PVMState.empty ())

    let get {index; tree} =
      let open Lwt_syntax in
      let+ state = Irmin_context.PVMState.get {index; state = ref tree} in
      !state

    let set {index; tree} pvmstate =
      let open Lwt_syntax in
      let state = ref tree in
      let+ () = Irmin_context.PVMState.set {index; state} (ref pvmstate) in
      {index; tree = !state}
  end
end

module Wasm_internal = struct
  let to_irmin_exn : Context.tree -> Irmin_context.tree =
   fun (Tree {tree; impl = (module Impl)}) ->
    match
      Context.equiv Irmin_context.equality_witness Impl.equality_witness
    with
    | Some Refl, Some Refl -> tree
    | _ -> raise (Invalid_argument "Not an Irmin tree")

  let of_irmin : Irmin_context.tree -> Context.tree =
   fun tree -> Context.make_tree ~tree ~impl:(module Irmin_context)
end

module Kernel = struct
  let config = Wasm_debugger.config

  let read_kernel = Wasm_debugger.read_kernel

  let check_kernel = Wasm_debugger.check_kernel

  let set_durable_value ?edit_readonly tree key value =
    let open Lwt_syntax in
    let tree = Wasm_internal.to_irmin_exn tree in
    let* tree = Wasm_debugger.set_durable_value ?edit_readonly tree key value in
    return (Wasm_internal.of_irmin tree)

  let start ~tree version kernel =
    let open Lwt_result_syntax in
    let tree = Wasm_internal.to_irmin_exn tree in
    let* tree = Wasm_debugger.start ~state:tree version kernel in
    return (Wasm_internal.of_irmin tree)

  let find_key_in_durable tree key =
    Wasm_debugger.find_key_in_durable (Wasm_internal.to_irmin_exn tree) key

  let wrap_as_durable_storage tree =
    Wasm_debugger.wrap_as_durable_storage (Wasm_internal.to_irmin_exn tree)

  let eval ?hooks ?migrate_to ~write_debug ~wasm_entrypoint level inboxes config
      step tree =
    let open Lwt_result_syntax in
    let tree = Wasm_internal.to_irmin_exn tree in
    let* tree, ticks, inboxes, level =
      Wasm_debugger.eval
        ?hooks
        ?migrate_to
        ~write_debug
        ~wasm_entrypoint
        level
        inboxes
        config
        step
        tree
    in
    let tree = Wasm_internal.of_irmin tree in
    return (tree, ticks, inboxes, level)

  let profile ?migrate_to ?hooks ~collapse ~with_time ~no_reboot level inboxes
      config function_symbols tree =
    let open Lwt_result_syntax in
    let tree = Wasm_internal.to_irmin_exn tree in
    let* tree, inboxes, level =
      Wasm_debugger.profile
        ?migrate_to
        ?hooks
        ~collapse
        ~with_time
        ~no_reboot
        level
        inboxes
        config
        function_symbols
        tree
    in
    let tree = Wasm_internal.of_irmin tree in
    return (tree, inboxes, level)

  let encode value tree =
    let open Lwt_syntax in
    let tree = Wasm_internal.to_irmin_exn tree in
    let* tree = Wasm_debugger.encode value tree in
    return (Wasm_internal.of_irmin tree)

  let decode tree =
    let tree = Wasm_internal.to_irmin_exn tree in
    Wasm_debugger.decode tree

  let get_wasm_version tree =
    Wasm_debugger.get_wasm_version (Wasm_internal.to_irmin_exn tree)

  let get_function_symbols tree =
    Wasm_debugger.get_function_symbols (Wasm_internal.to_irmin_exn tree)
end
