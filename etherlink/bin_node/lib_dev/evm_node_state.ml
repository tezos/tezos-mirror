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

type ('repo, 'tree) context_impl =
  (module S with type repo = 'repo and type tree = 'tree)

type tree =
  | Tree : {tree : 'tree; context_impl : ('repo, 'tree) context_impl} -> tree

type 'a index =
  | Index : {
      index : ('a, 'repo) Context_sigs.index;
      context_impl : ('repo, 'tree) context_impl;
    }
      -> 'a index

type rw_index = [`Read | `Write] index

type ro_index = [`Read] index

type 'a t =
  | Context : {
      index : ('a, 'repo) Context_sigs.index;
      tree : 'tree;
      context_impl : ('repo, 'tree) context_impl;
    }
      -> 'a t

type rw = [`Read | `Write] t

(* TODO TZK-27: Make hash generic wrt storage context *)
type hash = Irmin_context.hash

let make_index ~index ~context_impl = Index {index; context_impl}

let make_ctxt ~index ~tree ~context_impl = Context {index; tree; context_impl}

let make_tree ~tree ~context_impl = Tree {tree; context_impl}

let load : type tree repo.
    (repo, tree) context_impl ->
    cache_size:int ->
    ?async_domain:bool ->
    'a Access_mode.t ->
    string ->
    'a index tzresult Lwt.t =
 fun (module Impl) ~cache_size ?async_domain mode path ->
  let open Lwt_result_syntax in
  let* index = Impl.load ~cache_size ?async_domain mode path in
  return @@ make_index ~index ~context_impl:(module Impl)

let reload (Index {index; context_impl = (module Impl)} : [< `Read] index) =
  Impl.reload index

let commit : type a. ?message:string -> a t -> hash Lwt.t =
 fun ?message (Context {index; tree; context_impl = (module Impl)}) ->
  let open Lwt_syntax in
  let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
  let* hash = Impl.commit ?message {index; tree} in

  (* TODO TZK-27: Make hash generic wrt storage context *)
  let hash = Impl.context_hash_of_hash hash in
  let hash = Irmin_context.hash_of_context_hash hash in

  return hash

let checkout_exn : type a. a index -> hash -> a t Lwt.t =
 fun (Index {index; context_impl = (module Impl)}) hash ->
  let open Lwt_syntax in
  (* TODO TZK-27: Make hash generic wrt storage context *)
  let hash = Irmin_context.context_hash_of_hash hash in
  let hash = Impl.hash_of_context_hash hash in

  let* {index; tree} = Impl.checkout_exn index hash in
  return @@ make_ctxt ~index ~tree ~context_impl:(module Impl)

let empty : type a. a index -> a t =
 fun (Index {index; context_impl = (module Impl)}) ->
  let empty = Impl.empty index in
  make_ctxt ~index:empty.index ~tree:empty.tree ~context_impl:(module Impl)

let split : type a. a index -> unit =
 fun (Index {index; context_impl = (module Impl)}) -> Impl.split index

let gc : type a. a index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t
    =
 fun (Index {index; context_impl = (module Impl)}) ?callback hash ->
  (* TODO TZK-27: Make hash generic wrt storage context *)
  let hash = Irmin_context.context_hash_of_hash hash in
  let hash = Impl.hash_of_context_hash hash in

  let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
  Impl.gc index ?callback hash

let wait_gc_completion : type a. a index -> unit Lwt.t =
 fun (Index {index; context_impl = (module Impl)}) ->
  let index = (index : _ Impl.index :> [`Read | `Write] Impl.index) in
  Impl.wait_gc_completion index

let context_hash_of_hash h = Irmin_context.context_hash_of_hash h

let hash_of_context_hash h = Irmin_context.hash_of_context_hash h

module PVMState = struct
  type value = tree

  let empty : type tree repo. (repo, tree) context_impl -> unit -> value =
   fun (module Impl) () ->
    let tree = Impl.PVMState.empty () in
    make_tree ~tree ~context_impl:(module Impl)

  let get : type a. a t -> value Lwt.t =
   fun (Context {index; tree; context_impl = (module Impl)}) ->
    let open Lwt_syntax in
    let* tree = Impl.PVMState.get {index; tree} in
    return @@ make_tree ~tree ~context_impl:(module Impl)

  let set : type a. a t -> value -> a t Lwt.t =
   fun (Context {index; tree; context_impl = (module Impl)})
       (Tree ({context_impl = (module ImplT); _} as t)) ->
    let open Lwt_syntax in
    match Context.equiv Impl.equality_witness ImplT.equality_witness with
    | Some Refl, Some Refl ->
        let* ctxt = Impl.PVMState.set {index; tree} t.tree in
        return
        @@ make_ctxt
             ~index:ctxt.index
             ~tree:ctxt.tree
             ~context_impl:(module Impl)
    | _ -> raise (Invalid_argument "Implementation mismatch")
end

(* TODO TZX-24: Only make `Wasm_internal` available when node is instantiated
 * with the Wasm PVM *)
module Wasm_internal = struct
  let to_irmin_exn : tree -> Irmin_context.tree =
   fun (Tree {tree; context_impl = (module Impl)}) ->
    match
      Context.equiv Irmin_context.equality_witness Impl.equality_witness
    with
    | Some Refl, Some Refl -> tree
    | _ -> raise (Invalid_argument "Not an Irmin tree")

  let of_irmin : Irmin_context.tree -> tree =
   fun tree -> make_tree ~tree ~context_impl:(module Irmin_context)
end
