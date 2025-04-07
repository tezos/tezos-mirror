(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
open Store_properties

module type S_generic_key = sig
  (** {1 Brassaia stores}

      Brassaia stores are tree-like read-write stores with extended capabilities.
      They allow an application (or a collection of applications) to work with
      multiple local states, which can be forked and merged programmatically,
      without having to rely on a global state. In a way very similar to version
      control systems, Brassaia local states are called {i branches}.

      There are two kinds of store in Brassaia: the ones based on {{!of_branch}
      persistent} named branches and the ones based {{!of_commit} temporary}
      detached heads. These exist relative to a local, larger (and shared)
      store, and have some (shared) contents. This is exactly the same as usual
      version control systems, that the informed user can see as an implicit
      purely functional data-structure. *)

  module Schema : Schema.S

  (** The type for Brassaia repositories. *)
  type repo

  (** The type for Brassaia stores. *)
  type t

  (** The type for store contents. *)
  type contents = Schema.Contents.t [@@deriving brassaia]

  (** The type for store nodes. *)
  type node [@@deriving brassaia]

  (** The type for store trees. *)
  type tree [@@deriving brassaia]

  (** The type for object hashes. *)
  type hash = Schema.Hash.t [@@deriving brassaia]

  (** Type for [`Commit] identifiers. Similar to Git's commit SHA1s. *)
  type commit

  (** [commit_t r] is the value type for {!commit}. *)
  val commit_t : repo -> commit Type.t

  (** Type for persistent branch names. Branches usually share a common global
      namespace and it's the user's responsibility to avoid name clashes. *)
  type branch = Schema.Branch.t [@@deriving brassaia]

  (** Type for store slices. *)
  type slice [@@deriving brassaia]

  (** The type for commit info. *)
  type info = Schema.Info.t [@@deriving brassaia]

  (** The type for errors associated with functions computing least common
      ancestors *)
  type lca_error = [`Max_depth_reached | `Too_many_lcas] [@@deriving brassaia]

  (** The type for errors for {!Head.fast_forward}. *)
  type ff_error = [`No_change | `Rejected | lca_error] [@@deriving brassaia]

  module Info : sig
    (** @inline *)
    include Info.S with type t = info

    (** [pp] is a pretty-printer for info. *)
    val pp : t Fmt.t
    [@@ocaml.toplevel_printer]
  end

  type contents_key [@@deriving brassaia]

  type node_key [@@deriving brassaia]

  type commit_key [@@deriving brassaia]

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    (** The type of repository handles. *)
    type t = repo

    (** [init config] connects to a repository in a backend-specific manner. *)
    val init : Conf.t -> t

    (** [config repo] is the configuration used to create [repo] *)
    val config : t -> Conf.t

    (** @inline *)
    include Closeable with type _ t := t

    (** [heads] is {!Head.list}. *)
    val heads : t -> commit list

    (** [branches] is {!Branch.list}. *)
    val branches : t -> branch list

    (** [export t ~full ~depth ~min ~max] exports the store slice between [min]
        and [max], using at most [depth] history depth (starting from the max).

        If [max] is `Head (also the default value), use the current [heads]. If
        [min] is not specified, use an unbound past (but can still be limited by
        [depth]).

        [depth] is used to limit the depth of the commit history. [None] here
        means no limitation.

        If [full] is set (default is true), the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the commit
        history graph only. *)
    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:[`Head | `Max of commit list] ->
      t ->
      slice

    (** [import t s] imports the contents of the slice [s] in [t]. Does not
        modify branches. *)
    val import : t -> slice -> (unit, [`Msg of string]) result

    (** The type for elements iterated over by {!iter}. *)
    type elt =
      [ `Commit of commit_key
      | `Node of node_key
      | `Contents of contents_key
      | `Branch of branch ]
    [@@deriving brassaia]

    val default_pred_commit : t -> commit_key -> elt list

    val default_pred_node : t -> node_key -> elt list

    val default_pred_contents : t -> contents_key -> elt list

    (** [iter t] iterates in topological order over the closure graph of [t]. If
        [rev] is set (by default it is) the traversal is done in reverse order.

        [skip_branch], [skip_commit], [skip_node] and [skip_contents] allow the
        traversal to be stopped when the corresponding objects are traversed. By
        default no objects are skipped.

        The [branch], [commit], [node] and [contents] functions are called
        whenever the corresponding objects are traversed. By default these
        functions do nothing. These functions are not called on skipped objects.

        [pred_branch], [pred_commit], [pred_node] and [pred_contents] implicitly
        define the graph underlying the traversal. By default they exactly match
        the underlying Merkle graph of the repository [t]. These functions can
        be used to traverse a slightly modified version of that graph, for
        instance by modifying [pred_contents] to implicitly link structured
        contents with other objects in the graph.

        The traversed objects are all included between [min] (included) and
        [max] (included), following the Merkle graph order. Moreover, the [min]
        boundary is extended as follows:

        - contents and node objects in [min] stop the traversal; their
          predecessors are not traversed.
        - commit objects in [min] stop the traversal for their commit
          predecessors, but their sub-node are still traversed. This allows
          users to define an inclusive range of commit to iterate over.
        - branch objects in [min] implicitly add to [min] the commit they are
          pointing to; this allow users to define the iteration between two
          branches.

        [cache_size] is the size of the LRU used to store traversed objects. If
        an entry is evicted from the LRU, it can be traversed multiple times by
        {!Repo.iter}. When [cache_size] is [None] (the default), no entries is
        ever evicted from the cache; hence every object is only traversed once,
        at the cost of having to store all the traversed objects in memory. *)
    val iter :
      ?cache_size:int ->
      min:elt list ->
      max:elt list ->
      ?edge:(elt -> elt -> unit) ->
      ?branch:(branch -> unit) ->
      ?commit:(commit_key -> unit) ->
      ?node:(node_key -> unit) ->
      ?contents:(contents_key -> unit) ->
      ?skip_branch:(branch -> bool) ->
      ?skip_commit:(commit_key -> bool) ->
      ?skip_node:(node_key -> bool) ->
      ?skip_contents:(contents_key -> bool) ->
      ?pred_branch:(t -> branch -> elt list) ->
      ?pred_commit:(t -> commit_key -> elt list) ->
      ?pred_node:(t -> node_key -> elt list) ->
      ?pred_contents:(t -> contents_key -> elt list) ->
      ?rev:bool ->
      t ->
      unit

    val breadth_first_traversal :
      ?cache_size:int ->
      max:elt list ->
      ?branch:(branch -> unit) ->
      ?commit:(commit_key -> unit) ->
      ?node:(node_key -> unit) ->
      ?contents:(contents_key -> unit) ->
      ?pred_branch:(t -> branch -> elt list) ->
      ?pred_commit:(t -> commit_key -> elt list) ->
      ?pred_node:(t -> node_key -> elt list) ->
      ?pred_contents:(t -> contents_key -> elt list) ->
      t ->
      unit
  end

  (** [empty repo] is a temporary, empty store. Becomes a normal temporary store
      after the first update. *)
  val empty : repo -> t

  (** [main r] is a persistent store based on [r]'s main branch. This operation
      is cheap, can be repeated multiple times. *)
  val main : repo -> t

  (** [of_branch r name] is a persistent store based on the branch [name].
      Similar to {!main}, but use [name] instead of {!Brassaia.Branch.S.main}. *)
  val of_branch : repo -> branch -> t

  (** [of_commit c] is a temporary store, based on the commit [c].

      Temporary stores do not have stable names: instead they can be addressed
      using the hash of the current commit. Temporary stores are similar to
      Git's detached heads. In a temporary store, all the operations are
      performed relative to the current head and update operations can modify
      the current head: the current stores's head will automatically become the
      new head obtained after performing the update. *)
  val of_commit : commit -> t

  (** [repo t] is the repository containing [t]. *)
  val repo : t -> repo

  (** [tree t] is [t]'s current tree. Contents is not allowed at the root of the
      tree. *)
  val tree : t -> tree

  module Status : sig
    (** The type for store status. *)
    type t = [`Empty | `Branch of branch | `Commit of commit]

    (** [t] is the value type for {!type-t}. *)
    val t : repo -> t Type.t

    (** [pp] is the pretty-printer for store status. *)
    val pp : t Fmt.t
    [@@ocaml.toplevel_printer]
  end

  (** [status t] is [t]'s status. It can either be a branch, a commit or empty. *)
  val status : t -> Status.t

  (** Managing the store's heads. *)
  module Head : sig
    (** [list t] is the list of all the heads in local store. Similar to
        [git rev-list --all]. *)
    val list : repo -> commit list

    (** [find t] is the current head of the store [t]. This works for both
        persistent and temporary branches. In the case of a persistent branch,
        this involves getting the the head associated with the branch, so this
        may block. In the case of a temporary store, it simply returns the
        current head. Returns [None] if the store has no contents. Similar to
        [git rev-parse HEAD]. *)
    val find : t -> commit option

    (** Same as {!find} but raise [Invalid_argument] if the store does not have
        any contents. *)
    val get : t -> commit

    (** [set t h] updates [t]'s contents with the contents of the commit [h].
        Can cause data loss as it discards the current contents. Similar to
        [git reset --hard <hash>]. *)
    val set : t -> commit -> unit

    (** [fast_forward t h] is similar to {!set} but the [t]'s head is updated to
        [h] only if [h] is stricly in the future of [t]'s current head.
        [max_depth] or [n] are used to limit the search space of the lowest
        common ancestors (see {!lcas}).

        The result is:

        - [Ok ()] if the operation is succesfull;
        - [Error `No_change] if [h] is already [t]'s head;
        - [Error `Rejected] if [h] is not in the strict future of [t]'s head.
        - [Error e] if the history exploration has been cut before getting
          useful results. In that case. the operation can be retried using
          different parameters of [n] and [max_depth] to get better results. *)
    val fast_forward :
      t -> ?max_depth:int -> ?n:int -> commit -> (unit, ff_error) result

    (** Same as {!set} but check that the value is [test] before updating to
        [set]. Use {!set} or {!val-merge} instead if possible. *)
    val test_and_set : t -> test:commit option -> set:commit option -> bool

    (** [merge ~into:t ?max_head ?n commit] merges the contents of the commit
        associated to [commit] into [t]. [max_depth] is the maximal depth used
        for getting the lowest common ancestor. [n] is the maximum number of
        lowest common ancestors. If present, [max_depth] or [n] are used to
        limit the search space of the lowest common ancestors (see {!lcas}). *)
    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Merge.conflict) result
  end

  (** Object hashes. *)
  module Hash : Hash.S with type t = hash

  (** [Commit] defines immutable objects to describe store updates. *)
  module Commit : sig
    (** The type for store commits. *)
    type t = commit

    (** [t] is the value type for {!type-t}. *)
    val t : repo -> t Type.t

    (** [pp_hash] is a pretty-printer for a commit. Displays only the hash. *)
    val pp_hash : t Fmt.t

    (** [pp] is a full pretty-printer for a commit. Displays all information. *)
    val pp : t Fmt.t
    [@@ocaml.toplevel_printer]

    (** [init r i ~parents:p t] is the commit [c] such that:

        - [info c = i]
        - [parents c = p]
        - [tree c = t]

        When [clear] is set (the default), the tree cache is emptied upon the
        function's completion, mirroring the effect of invoking {!Tree.clear}. *)
    val init :
      ?clear:bool ->
      repo ->
      info:info ->
      parents:commit_key list ->
      tree ->
      commit

    (** [tree c] is [c]'s root tree. *)
    val tree : commit -> tree

    (** [parents c] are [c]'s parents. *)
    val parents : commit -> commit_key list

    (** [info c] is [c]'s info. *)
    val info : commit -> info

    (** [hash c] is [c]'s hash. *)
    val hash : commit -> hash

    (** {1 Import/Export} *)

    (** [key c] is [c]'s key. *)
    val key : commit -> commit_key

    (** [of_key r k] is the the commit object in [r] with key [k], or [None] if
        no such commit object exists. *)
    val of_key : repo -> commit_key -> commit option

    (** [of_hash r h] is the commit object in [r] with hash [h], or [None] if no
        such commit object is indexed in [r].

        {b Note:} in stores for which {!commit_key} = {!type-hash}, this
        function has identical behaviour to {!of_key}. *)
    val of_hash : repo -> hash -> commit option
  end

  (** [Contents] provides base functions for the store's contents. *)
  module Contents : sig
    include Contents.S with type t = contents

    (** {1 Import/Export} *)

    (** [hash c] it [c]'s hash. *)
    val hash : contents -> hash

    (** [of_key r k] is the contents object in [r] with key [k], or [None] if no
        such contents object exists. *)
    val of_key : repo -> contents_key -> contents option

    (** [of_hash r h] is the contents object in [r] with hash [h], or [None] if
        no such contents object is indexed in [r].

        {b Note:} in stores for which {!contents_key} = {!type-hash}, this
        function has identical behaviour to {!of_key}. *)
    val of_hash : repo -> hash -> contents option
  end

  (** Managing store's trees. *)
  module Tree : sig
    include
      Tree.S
        with type t := tree
         and type contents := contents
         and type contents_key := contents_key
         and type node := node
         and type hash := hash

    (** [pp] is a pretty-printer for a tree. *)
    val pp : tree Type.pp
    [@@ocaml.toplevel_printer]

    (** {1 Import/Export} *)

    (** Keys in the Brassaia store are tagged with the type of the value they
        reference (either {!contents} or {!node}). *)
    type kinded_key = [`Contents of contents_key | `Node of node_key]
    [@@deriving brassaia]

    (** [key t] is the key of tree [t] in the underlying repository, if it
        exists. Tree objects that exist entirely in memory (such as those built
        with {!of_concrete}) have no backend key until they are exported to a
        repository, and so will return [None]. *)
    val key : tree -> kinded_key option

    (** [find_key r t] is the key of a tree object with the same hash as [t] in
        [r], if such a key exists and is indexed. *)
    val find_key : Repo.t -> tree -> kinded_key option

    (** [of_key r h] is the tree object in [r] having [h] as key, or [None] if
        no such tree object exists. *)
    val of_key : Repo.t -> kinded_key -> tree option

    (** [shallow r h] is the shallow tree object with the key [h]. No check is
        performed to verify if [h] actually exists in [r]. *)
    val shallow : Repo.t -> kinded_key -> tree

    (** [hash t] is the hash of tree [t]. *)
    val hash : ?cache:bool -> tree -> hash

    (** Like {!kinded_key}, but with hashes as value references rather than
        keys. *)
    type kinded_hash = [`Contents of hash | `Node of hash]

    (** [kinded_hash t] is [c]'s kinded hash. *)
    val kinded_hash : ?cache:bool -> tree -> kinded_hash

    (** [of_hash r h] is the tree object in [r] with hash [h], or [None] if no
        such tree object is indexed in [r].

        {b Note:} in stores for which {!node_key} = {!contents_key} =
        {!type-hash}, this function has identical behaviour to {!of_key}. *)
    val of_hash : Repo.t -> kinded_hash -> tree option

    (** {1 Proofs} *)

    (** [produce r h f] runs [f] on top of a real store [r], producing a proof
        and a result using the initial root hash [h].

        The trees produced during [f]'s computation will carry the full history
        of reads. This history will be reset when [f] is complete so subtrees
        escaping the scope of [f] will not cause memory leaks.

        Calling [produce_proof] recursively has an undefined behaviour. *)
    type 'result producer :=
      repo -> kinded_key -> (tree -> tree * 'result) -> Proof.t * 'result

    (** The type for errors associated with functions that verify proofs. *)
    type verifier_error = [`Proof_mismatch of string] [@@deriving brassaia]

    (** [verify p f] runs [f] in checking mode. [f] is a function that takes a
        tree as input and returns a new version of the tree and a result. [p] is
        a proof, that is a minimal representation of the tree that contains what
        [f] should be expecting.

        Therefore, contrary to trees found in a storage, the contents of the
        trees passed to [f] may not be available. For this reason, looking up a
        value at some [path] can now produce three distinct outcomes:

        - A value [v] is present in the proof [p] and returned :
          [find tree path] is a promise returning [Some v];
        - [path] is known to have no value in [tree] : [find tree path] is a
          promise returning [None]; and
        - [path] is known to have a value in [tree] but [p] does not provide it
          because [f] should not need it: [verify] returns an error classifying
          [path] as an invalid path (see below).

        The same semantics apply to all operations on the tree [t] passed to [f]
        and on all operations on the trees built from [f].

        The generated tree is the tree after [f] has completed. That tree is
        disconnected from the backend. It is possible to run operations on it as
        long as they don't require loading shallowed subtrees, otherwise it
        would raise [Dangling_hash].

        The result is [Error _] if the proof is rejected:

        - when [p.before] is different from the hash of [p.state];
        - when [p.after] is different from the hash of [f p.state];
        - when [f p.state] tries to access paths invalid paths in [p.state]; *)
    type 'result verifier :=
      Proof.t ->
      (tree -> tree * 'result) ->
      (tree * 'result, verifier_error) result

    (** [produce_proof] is the producer of tree proofs. *)
    val produce_proof : 'a producer

    (** [verify_proof] is the verifier of tree proofs. *)
    val verify_proof : 'a verifier

    val hash_of_proof_state : Proof.tree -> kinded_hash
  end

  (** {1 Reads} *)

  (** [kind] is {!Tree.kind} applied to [t]'s root tree. *)
  val kind : t -> Path.t -> [`Contents | `Node] option

  (** [list t] is {!Tree.list} applied to [t]'s root tree. *)
  val list : t -> Path.t -> (Path.step * tree) list

  (** [mem t] is {!Tree.mem} applied to [t]'s root tree. *)
  val mem : t -> Path.t -> bool

  (** [mem_tree t] is {!Tree.mem_tree} applied to [t]'s root tree. *)
  val mem_tree : t -> Path.t -> bool

  (** [find_all t] is {!Tree.find_all} applied to [t]'s root tree. *)
  val find_all : t -> Path.t -> contents option

  (** [find t] is {!Tree.find} applied to [t]'s root tree. *)
  val find : t -> Path.t -> contents option

  (** [get_all t] is {!Tree.get_all} applied on [t]'s root tree. *)
  val get_all : t -> Path.t -> contents

  (** [get t] is {!Tree.get} applied to [t]'s root tree. *)
  val get : t -> Path.t -> contents

  (** [find_tree t] is {!Tree.find_tree} applied to [t]'s root tree. *)
  val find_tree : t -> Path.t -> tree option

  (** [get_tree t k] is {!Tree.get_tree} applied to [t]'s root tree. *)
  val get_tree : t -> Path.t -> tree

  type kinded_key := [`Contents of contents_key | `Node of node_key]

  (** [id t k] *)
  val key : t -> Path.t -> kinded_key option

  (** [hash t k] *)
  val hash : t -> Path.t -> hash option

  (** {1 Updates} *)

  (** The type for write errors.

      - Merge conflict.
      - Concurrent transactions are competing to get the current operation
        committed and too many attemps have been tried (livelock).
      - A "test and set" operation has failed and the current value is [v]
        instead of the one we were waiting for. *)
  type write_error =
    [Merge.conflict | `Too_many_retries of int | `Test_was of tree option]
  [@@deriving brassaia]

  (** [set t k ~info v] sets [k] to the value [v] in [t]. Discard any previous
      results but ensure that no operation is lost in the history.

      When [clear] is set (the default), the tree cache is emptied upon the
      function's completion, mirroring the effect of invoking {!Tree.clear}.

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)
  val set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    contents ->
    (unit, write_error) result

  (** [set_exn] is like {!set} but raise [Failure _] instead of using a result
      type. *)
  val set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    contents ->
    unit

  (** [set_tree] is like {!set} but for trees. *)
  val set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    tree ->
    (unit, write_error) result

  (** [set_tree] is like {!set_exn} but for trees. *)
  val set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    tree ->
    unit

  (** [remove t ~info k] remove any bindings to [k] in [t].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)
  val remove :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    (unit, write_error) result

  (** [remove_exn] is like {!remove} but raise [Failure _] instead of a using
      result type. *)
  val remove_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    unit

  (** [test_and_set ~test ~set] is like {!set} but it atomically checks that the
      tree is [test] before modifying it to [set].

      The result is [Error (`Test t)] if the current tree is [t] instead of
      [test].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)
  val test_and_set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    test:contents option ->
    set:contents option ->
    (unit, write_error) result

  (** [test_and_set_exn] is like {!test_and_set} but raise [Failure _] instead
      of using a result type. *)
  val test_and_set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    test:contents option ->
    set:contents option ->
    unit

  (** [test_and_set_tree] is like {!test_and_set} but for trees. *)
  val test_and_set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result

  (** [test_and_set_tree_exn] is like {!test_and_set_exn} but for trees. *)
  val test_and_set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    Path.t ->
    test:tree option ->
    set:tree option ->
    unit

  (** [test_set_and_get] is like {!test_and_set} except it also returns the
      commit associated with updating the store with the new value if the
      [test_and_set] is successful. No commit is returned if there was no update
      to the store. *)
  val test_set_and_get :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    Path.t ->
    test:contents option ->
    set:contents option ->
    (commit option, write_error) result

  (** [test_set_and_get_exn] is like {!test_set_and_get} but raises [Failure _]
      instead. *)
  val test_set_and_get_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    Path.t ->
    test:contents option ->
    set:contents option ->
    commit option

  (** [test_set_and_get_tree] is like {!test_set_and_get} but for a {!type-tree} *)
  val test_set_and_get_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    Path.t ->
    test:tree option ->
    set:tree option ->
    (commit option, write_error) result

  (** [test_set_and_get_tree_exn] is like {!test_set_and_get_tree} but raises
      [Failure _] instead. *)
  val test_set_and_get_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    Path.t ->
    test:tree option ->
    set:tree option ->
    commit option

  (** [merge ~old] is like {!set} but merge the current tree and the new tree
      using [old] as ancestor in case of conflicts.

      The result is [Error (`Conflict c)] if the merge failed with the conflict
      [c].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)
  val merge :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    Path.t ->
    contents option ->
    (unit, write_error) result

  (** [merge_exn] is like {!val-merge} but raise [Failure _] instead of using a
      result type. *)
  val merge_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    Path.t ->
    contents option ->
    unit

  (** [merge_tree] is like {!merge_tree} but for trees. *)
  val merge_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    Path.t ->
    tree option ->
    (unit, write_error) result

  (** [merge_tree] is like {!merge_tree} but for trees. *)
  val merge_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    Path.t ->
    tree option ->
    unit

  (** [with_tree t k ~info f] replaces {i atomically} the subtree [v] under [k]
      in the store [t] by the contents of the tree [f v], using the commit info
      [info ()].

      If [v = f v] and [allow_empty] is unset (default) then, the operation is a
      no-op.

      If [v != f v] and no other changes happen concurrently, [f v] becomes the
      new subtree under [k]. If other changes happen concurrently to that
      operations, the semantics depend on the value of [strategy]:

      - if [strategy = `Set], use {!set} and discard any concurrent updates to
        [k].
      - if [strategy = `Test_and_set] (default), use {!test_and_set} and ensure
        that no concurrent operations are updating [k].
      - if [strategy = `Merge], use {!val-merge} and ensure that concurrent
        updates and merged with the values present at the beginning of the
        transaction.

      {b Note:} Brassaia transactions provides
      {{:https://en.wikipedia.org/wiki/Snapshot_isolation} snapshot isolation}
      guarantees: reads and writes are isolated in every transaction, but only
      write conflicts are visible on commit. *)
  val with_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[`Set | `Test_and_set | `Merge] ->
    info:Info.f ->
    t ->
    Path.t ->
    (tree option -> tree option) ->
    (unit, write_error) result

  (** [with_tree_exn] is like {!with_tree} but raise [Failure _] instead of
      using a return type. *)
  val with_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[`Set | `Test_and_set | `Merge] ->
    info:Info.f ->
    t ->
    Path.t ->
    (tree option -> tree option) ->
    unit

  (** {1 Clones} *)

  (** [clone ~src ~dst] makes [dst] points to [Head.get src]. [dst] is created
      if needed. Remove the current contents en [dst] if [src] is {!val-empty}. *)
  val clone : src:t -> dst:branch -> t

  (** {1 Watches} *)

  (** The type for store watches. *)
  type watch

  (** [watch t f] calls [f] every time the contents of [t]'s head is updated.

      {b Note:} even if [f] might skip some head updates, it will never be
      called concurrently: all consecutive calls to [f] are done in sequence, so
      we ensure that the previous one ended before calling the next one. *)
  val watch : t -> ?init:commit -> (commit Diff.t -> unit) -> watch

  (** [watch_key t key f] calls [f] every time the [key]'s value is added,
      removed or updated. If the current branch is deleted, no signal is sent to
      the watcher. *)
  val watch_key :
    t -> Path.t -> ?init:commit -> ((commit * tree) Diff.t -> unit) -> watch

  (** [unwatch w] disable [w]. Return once the [w] is fully disabled. *)
  val unwatch : watch -> unit

  (** {1 Merges and Common Ancestors} *)

  (** The type for merge functions. *)
  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result

  (** [merge_into ~into:x ~info:i t] merges [t]'s current branch into [x]'s
      current branch using the info [i]. After that operation, the two stores
      are still independent. Similar to [git merge <branch>]. *)
  val merge_into : into:t -> t merge

  (** Same as {!val-merge} but with a branch ID. *)
  val merge_with_branch : t -> branch merge

  (** Same as {!val-merge} but with a commit_id. *)
  val merge_with_commit : t -> commit merge

  (** [lca ?max_depth ?n msg t1 t2] returns the collection of least common
      ancestors between the heads of [t1] and [t2] branches.

      - [max_depth] is the maximum depth of the exploration (default is
        [max_int]). Return [Error `Max_depth_reached] if this depth is exceeded.
      - [n] is the maximum expected number of lcas. Stop the exploration as soon
        as [n] lcas are found. Return [Error `Too_many_lcas] if more [lcas] are
        found. *)
  val lcas :
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result

  (** Same as {!lcas} but takes a branch ID as argument. *)
  val lcas_with_branch :
    t -> ?max_depth:int -> ?n:int -> branch -> (commit list, lca_error) result

  (** Same as {!lcas} but takes a commmit as argument. *)
  val lcas_with_commit :
    t -> ?max_depth:int -> ?n:int -> commit -> (commit list, lca_error) result

  (** {1 History} *)

  (** An history is a DAG of heads. *)
  module History : Graph.Sig.P with type V.t = commit

  (** [history ?depth ?min ?max t] is a view of the history of the store [t], of
      depth at most [depth], starting from the [t]'s head (or from [max] if the
      head is not set) and stopping at [min] if specified. *)
  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t

  (** [last_modified ?number c k] is the list of the last [number] commits that
      modified [path], in ascending order of date. [depth] is the maximum depth
      to be explored in the commit graph, if any. Default value for [number] is
      1. *)
  val last_modified : ?depth:int -> ?n:int -> t -> Path.t -> commit list

  (** Manipulate branches. *)
  module Branch : sig
    (** {1 Branch Store}

        Manipulate relations between {{!branch} branches} and {{!commit}
        commits}. *)

    (** [mem r b] is true iff [b] is present in [r]. *)
    val mem : repo -> branch -> bool

    (** [find r b] is [Some c] iff [c] is bound to [b] in [t]. It is [None] if
        [b] is not present in [t]. *)
    val find : repo -> branch -> commit option

    (** [get t b] is similar to {!find} but raise [Invalid_argument] if [b] is
        not present in [t]. *)
    val get : repo -> branch -> commit

    (** [set t b c] bounds [c] to [b] in [t]. *)
    val set : repo -> branch -> commit -> unit

    (** [remove t b] removes [b] from [t]. *)
    val remove : repo -> branch -> unit

    (** [list t] is the list of branches present in [t]. *)
    val list : repo -> branch list

    (** [watch t b f] calls [f] on every change in [b]. *)
    val watch :
      repo -> branch -> ?init:commit -> (commit Diff.t -> unit) -> watch

    (** [watch_all t f] calls [f] on every branch-related change in [t],
        including creation/deletion events. *)
    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit Diff.t -> unit) ->
      watch

    (** [pp] is a pretty-printer for a branch. *)
    val pp : branch Fmt.t
    [@@ocaml.toplevel_printer]

    (** Base functions for branches. *)
    include Branch.S with type t = branch
  end

  (** Backend functions, which might be used by the backends. *)
  module Backend :
    Backend.S
      with module Schema = Schema
      with type Slice.t = slice
       and type Repo.t = repo
       and module Hash = Hash
       and type Contents.key = contents_key
       and type Node.key = node_key
       and type Commit.key = commit_key

  type Remote.t +=
    | E of Backend.Remote.endpoint
          (** Extend the [remote] type with [endpoint]. *)

  (** {2 Converters to backend types} *)

  val of_backend_node : repo -> Backend.Node.value -> node

  val to_backend_node : node -> Backend.Node.value

  val to_backend_portable_node : node -> Backend.Node_portable.t

  (** [to_backend_commit c] is the backend commit object associated with the
      commit [c]. *)
  val to_backend_commit : commit -> Backend.Commit.value

  (** [of_backend_commit r k c] is the commit associated with the backend commit
      object [c] that hash key [k] in [r]. *)
  val of_backend_commit :
    repo -> Backend.Commit.Key.t -> Backend.Commit.value -> commit

  (** Save a content into the database *)
  val save_contents : [> write] Backend.Contents.t -> contents -> contents_key

  (** Save a tree into the database. Does not do any reads.

      When [clear] is set (the default), the tree cache is emptied upon the
      function's completion, mirroring the effect of invoking {!Tree.clear}. *)
  val save_tree :
    ?clear:bool ->
    repo ->
    [> write] Backend.Contents.t ->
    [> read_write] Backend.Node.t ->
    tree ->
    kinded_key

  (** {2 Deprecated} *)

  (** @deprecated Use {!main} instead *)
  val master : repo -> t
  [@@ocaml.deprecated "Use `main` instead."]
end

module type S = sig
  type hash

  (** @inline *)
  include
    S_generic_key
      with type Schema.Hash.t = hash
       and type hash := hash
       and type contents_key = hash
       and type node_key = hash
       and type commit_key = hash
end

module S_is_a_generic_keyed (X : S) : S_generic_key = X

module type Maker_generic_key = sig
  type endpoint

  include Key.Store_spec.S

  module Make (Schema : Schema.S) :
    S_generic_key
      with module Schema = Schema
       and type Backend.Remote.endpoint = endpoint
       and type contents_key = (Schema.Hash.t, Schema.Contents.t) contents_key
       and type node_key = Schema.Hash.t node_key
       and type commit_key = Schema.Hash.t commit_key
end

module type Maker =
  Maker_generic_key
    with type ('h, _) contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

module type Json_tree = functor
  (Store : S with type Schema.Contents.t = Contents.json)
  -> sig
  include Contents.S with type t = Contents.json

  val to_concrete_tree : t -> Store.Tree.concrete

  val of_concrete_tree : Store.Tree.concrete -> t

  (** Extract a [json] value from tree at the given key. *)
  val get_tree : Store.tree -> Path.t -> t

  (** Project a [json] value onto a tree at the given key. *)
  val set_tree : Store.tree -> Path.t -> t -> Store.tree

  (** Extract a [json] value from a store at the given key. *)
  val get : Store.t -> Path.t -> t

  (** Project a [json] value onto a store at the given key. *)
  val set : Store.t -> Path.t -> t -> info:(unit -> Store.info) -> unit
end

module type KV_generic_key = S_generic_key with type Schema.Branch.t = string

module type KV = S with type Schema.Branch.t = string

module type KV_maker_generic_key = sig
  type endpoint

  type hash

  type info

  include Key.Store_spec.S

  module Make (C : Contents.S) :
    KV_generic_key
      with module Schema.Contents = C
       and type Backend.Remote.endpoint = endpoint
       and type Schema.Hash.t = hash
       and type contents_key = (hash, C.t) contents_key
       and type node_key = hash node_key
       and type commit_key = hash commit_key
       and type Schema.Info.t = info
end

module type KV_maker =
  KV_maker_generic_key
    with type ('h, _) contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

module type Sigs = sig
  module type S = S

  module type Maker = Maker

  module type Json_tree = Json_tree

  module type KV = KV

  module type KV_maker = KV_maker

  module Generic_key : sig
    module type S = S_generic_key

    module type KV = KV_generic_key

    module type Maker = Maker_generic_key

    module type KV_maker = KV_maker_generic_key
  end

  type Remote.t +=
    | Store : (module Generic_key.S with type t = 'a) * 'a -> Remote.t

  module Make (B : Backend.S) :
    Generic_key.S
      with module Schema = B.Schema
       and type slice = B.Slice.t
       and type repo = B.Repo.t
       and type contents_key = B.Contents.key
       and type node_key = B.Node.key
       and type commit_key = B.Commit.key
       and module Backend = B

  (** [Json_tree] is used to project JSON values onto trees. Instead of the
      entire object being stored under one key, it is split across several keys
      starting at the specified root key. *)
  module Json_tree : Json_tree
end
