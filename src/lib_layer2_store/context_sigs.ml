(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Store_sigs

module Equality_witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option
end = struct
  type (_, _) eq = Refl : ('a, 'a) eq

  type _ equality = ..

  module type Inst = sig
    type t

    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a

      type _ equality += Eq : t equality
    end in
    (module Inst)

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

type ('a, 'b) equality_witness = 'a Equality_witness.t * 'b Equality_witness.t

type ('a, 'repo) raw_index = {path : string; repo : 'repo}

type ('a, 'repo) index = ('a, 'repo) raw_index
  constraint 'a = [< `Read | `Write > `Read]

type ('a, 'repo, 'tree) t = {
  index : ('a, 'repo) index;
  tree : 'tree;
}
  constraint 'a = [< `Read | `Write > `Read]

module type S = sig
  type tree

  type repo

  type nonrec 'a index = ('a, repo) index

  val impl_name : string

  val equality_witness : (repo, tree) equality_witness

  type nonrec 'a t = ('a, repo, tree) t

  type hash = Context_hash.t

  (** [load cache_size path] initializes from disk a context from
    [path]. [cache_size] allows to change size of the Context Backend
    in use (for instance, the LRU cache size of Irmin (100_000 by
    default at irmin-pack/config.ml) *)
  val load : cache_size:int -> 'a mode -> string -> 'a index tzresult Lwt.t

  (** [index context] is the repository of the context [context]. *)
  val index : 'a t -> 'a index

  (** [close ctxt] closes the context index [ctxt]. *)
  val close : _ index -> unit Lwt.t

  (** [readonly index] returns a read-only version of the index. *)
  val readonly : [> `Read] index -> [`Read] index

  (** [checkout ctxt hash] checkouts the content that corresponds to the commit
    hash [hash] in the repository [ctxt] and returns the corresponding
    context. If there is no commit that corresponds to [hash], it returns
    [None].  *)
  val checkout : 'a index -> hash -> 'a t option Lwt.t

  (** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
  val empty : 'a index -> 'a t

  (** [commit ?message context] commits content of the context [context] on disk,
    and return the commit hash. *)
  val commit : ?message:string -> [> `Write] t -> hash Lwt.t

  (** [is_gc_finished index] returns true if a GC is finished (or idle) and false
    if a GC is running for [index]. *)
  val is_gc_finished : [> `Write] index -> bool

  (** [split ctxt] splits the current context in order to chunk the file if the
      backend supports it. This split function is expected to be called after
      committing a commit that will be a future candidate for a GC target.  *)
  val split : _ index -> unit

  (** [gc index ?callback hash] removes all data older than [hash] from disk.
    If passed, [callback] will be executed when garbage collection finishes. *)
  val gc :
    [> `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

  (** [wait_gc_completion index] will return a blocking thread if a
    GC run is currently ongoing. *)
  val wait_gc_completion : [> `Write] index -> unit Lwt.t

  (** [export_snapshot index context_hash ~path] exports the context
      corresponding to [context_hash], if found in [index], into the given
      folder path. *)
  val export_snapshot : _ index -> hash -> path:string -> unit tzresult Lwt.t

  (** State of the PVM that this rollup node deals with *)
  module PVMState : sig
    (** The value of a PVM state *)
    type value = tree

    (** [empty ()] is the empty PVM state. *)
    val empty : unit -> value

    (** [find context] returns the PVM state stored in the [context], if any. *)
    val find : _ t -> value option Lwt.t

    (** [lookup state path] returns the data stored for the path [path] in the PVM
      state [state].  *)
    val lookup : value -> string list -> bytes option Lwt.t

    (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!val:commit}. *)
    val set : 'a t -> value -> 'a t Lwt.t
  end

  module Internal_for_tests : sig
    (** [get_a_tree key] provides a value of internal type [tree] which can be
      used as a state to be set in the context directly. *)
    val get_a_tree : string -> tree Lwt.t
  end
end
