(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include Environment_context_intf
open Error_monad

let err_implementation_mismatch ~expected ~got =
  Format.kasprintf
    invalid_arg
    "Context implementation mismatch: expecting %s, got %s"
    expected
    got

module Equality_witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option

  val hash : 'a t -> int
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

  let hash : type a. a t -> int = fun (module A) -> Hashtbl.hash A.Eq
end

module Context = struct
  type key = string list

  type value = Bytes.t

  type ('ctxt, 'tree) ops = (module S with type t = 'ctxt and type tree = 'tree)

  type _ kind = ..

  type ('a, 'b) equality_witness = 'a Equality_witness.t * 'b Equality_witness.t

  let equality_witness () = (Equality_witness.make (), Equality_witness.make ())

  let equiv (a, b) (c, d) = (Equality_witness.eq a c, Equality_witness.eq b d)

  type cache_value = ..

  type delayed_value = unit -> cache_value Lwt.t

  let delay e () = Lwt.return e

  type cache = delayed_value Environment_cache.t

  type t =
    | Context : {
        kind : 'a kind;
        impl_name : string;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        equality_witness : ('a, 'b) equality_witness;
        cache : cache;
      }
        -> t

  let make ~kind ~impl_name ~ctxt ~ops ~equality_witness =
    Context
      {
        kind;
        impl_name;
        ctxt;
        ops;
        equality_witness;
        cache = Environment_cache.uninitialised;
      }

  let mem (Context {ops = (module Ops); ctxt; _}) key = Ops.mem ctxt key

  let add (Context ({ops = (module Ops); ctxt; _} as c)) key value =
    let open Lwt_syntax in
    let+ ctxt = Ops.add ctxt key value in
    Context {c with ctxt}

  let find (Context {ops = (module Ops); ctxt; _}) key = Ops.find ctxt key

  let remove (Context ({ops = (module Ops); ctxt; _} as c)) key =
    let open Lwt_syntax in
    let+ ctxt = Ops.remove ctxt key in
    Context {c with ctxt}

  (* trees *)
  type tree =
    | Tree : {
        ops : ('a, 'b) ops;
        impl_name : string;
        tree : 'b;
        equality_witness : ('a, 'b) equality_witness;
      }
        -> tree

  let mem_tree (Context {ops = (module Ops); ctxt; _}) key =
    Ops.mem_tree ctxt key

  let add_tree (Context ({ops = (module Ops); ctxt; _} as c)) key (Tree t) =
    let open Lwt_syntax in
    match equiv c.equality_witness t.equality_witness with
    | Some Refl, Some Refl ->
        let+ ctxt = Ops.add_tree ctxt key t.tree in
        Context {c with ctxt}
    | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

  let find_tree
      (Context {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _})
      key =
    let open Lwt_syntax in
    let+ t = Ops.find_tree ctxt key in
    Option.map (fun tree -> Tree {ops; tree; equality_witness; impl_name}) t

  let list
      (Context {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _})
      ?offset ?length key =
    let open Lwt_syntax in
    let+ ls = Ops.list ctxt ?offset ?length key in
    List.fold_left
      (fun acc (k, tree) ->
        let v = Tree {ops; tree; equality_witness; impl_name} in
        (k, v) :: acc)
      []
      (List.rev ls)

  let length (Context {ops = (module Ops); ctxt; _}) key = Ops.length ctxt key

  let fold ?depth
      (Context {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _})
      key ~order ~init ~f =
    Ops.fold ?depth ctxt key ~order ~init ~f:(fun k v acc ->
        let v = Tree {ops; tree = v; equality_witness; impl_name} in
        f k v acc)

  (* Tree *)
  module Tree = struct
    let pp ppf (Tree {ops = (module Ops); tree; _}) = Ops.Tree.pp ppf tree

    let hash (Tree {ops = (module Ops); tree; _}) = Ops.Tree.hash tree

    let kind (Tree {ops = (module Ops); tree; _}) = Ops.Tree.kind tree

    let to_value (Tree {ops = (module Ops); tree; _}) = Ops.Tree.to_value tree

    let of_value
        (Context
           {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) v
        =
      let open Lwt_syntax in
      let+ tree = Ops.Tree.of_value ctxt v in
      Tree {ops; tree; equality_witness; impl_name}

    let equal (Tree {ops = (module Ops); tree; equality_witness; _}) (Tree t) =
      match equiv equality_witness t.equality_witness with
      | Some Refl, Some Refl -> Ops.Tree.equal tree t.tree
      | _ -> false

    let empty
        (Context
           {ops = (module Ops) as ops; equality_witness; ctxt; impl_name; _}) =
      let empty = Ops.Tree.empty ctxt in
      Tree {ops; equality_witness; tree = empty; impl_name}

    let is_empty (Tree {ops = (module Ops); tree; _}) = Ops.Tree.is_empty tree

    let mem (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.mem tree key

    let add (Tree ({ops = (module Ops); tree; _} as c)) key value =
      let open Lwt_syntax in
      let+ tree = Ops.Tree.add tree key value in
      Tree {c with tree}

    let find (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.find tree key

    let mem_tree (Tree {ops = (module Ops); tree; _}) key =
      Ops.Tree.mem_tree tree key

    let add_tree (Tree ({ops = (module Ops); _} as c)) key (Tree t) =
      let open Lwt_syntax in
      match equiv c.equality_witness t.equality_witness with
      | Some Refl, Some Refl ->
          let+ tree = Ops.Tree.add_tree c.tree key t.tree in
          Tree {c with tree}
      | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

    let find_tree (Tree ({ops = (module Ops); tree; _} as c)) key =
      let open Lwt_syntax in
      let+ t = Ops.Tree.find_tree tree key in
      Option.map (fun tree -> Tree {c with tree}) t

    let remove (Tree ({ops = (module Ops); tree; _} as c)) key =
      let open Lwt_syntax in
      let+ tree = Ops.Tree.remove tree key in
      Tree {c with tree}

    let list
        (Tree {ops = (module Ops) as ops; tree; equality_witness; impl_name})
        ?offset ?length key =
      let open Lwt_syntax in
      let+ ls = Ops.Tree.list tree ?offset ?length key in
      List.fold_left
        (fun acc (k, tree) ->
          let v = Tree {ops; tree; equality_witness; impl_name} in
          (k, v) :: acc)
        []
        (List.rev ls)

    let length (Tree {ops = (module Ops); tree; _}) key =
      Ops.Tree.length tree key

    let fold ?depth
        (Tree {ops = (module Ops) as ops; tree = t; equality_witness; impl_name})
        key ~order ~init ~f =
      Ops.Tree.fold ?depth t key ~order ~init ~f:(fun k v acc ->
          let v = Tree {ops; tree = v; equality_witness; impl_name} in
          f k v acc)

    let clear ?depth (Tree {ops = (module Ops); tree; _}) =
      Ops.Tree.clear ?depth tree

    let config (Tree {ops = (module Ops); tree; _}) = Ops.Tree.config tree
  end

  let config (Context {ops = (module Ops); ctxt; _}) = Ops.config ctxt

  (* Proof *)
  module Proof = Tezos_context_sigs.Context.Proof_types

  (* In-memory context for proof *)
  module Proof_context = struct
    module M = struct
      include Tezos_context_memory.Context

      let set_protocol = add_protocol

      let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
    end

    let equality_witness : (M.t, M.tree) equality_witness = equality_witness ()

    let ops = (module M : S with type t = 'ctxt and type tree = 'tree)

    let impl_name = "proof"

    let inject : M.tree -> tree =
     fun tree -> Tree {ops; tree; equality_witness; impl_name}

    let project : tree -> M.tree =
     fun (Tree t) ->
      match equiv t.equality_witness equality_witness with
      | Some Refl, Some Refl -> t.tree
      | _ -> err_implementation_mismatch ~expected:impl_name ~got:t.impl_name
  end

  (* In-memory context for proof, using [Context_binary] which produces more
     compact Merkle proofs. *)
  module Proof_context_binary = struct
    module M = struct
      include Tezos_context_memory.Context_binary

      let set_protocol = add_protocol

      let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
    end

    let equality_witness : (M.t, M.tree) equality_witness = equality_witness ()

    let ops = (module M : S with type t = 'ctxt and type tree = 'tree)

    let impl_name = "proof_binary"

    let inject : M.tree -> tree =
     fun tree -> Tree {ops; tree; equality_witness; impl_name}

    let project : tree -> M.tree =
     fun (Tree t) ->
      match equiv t.equality_witness equality_witness with
      | Some Refl, Some Refl -> t.tree
      | _ -> err_implementation_mismatch ~expected:impl_name ~got:t.impl_name
  end

  module type Proof_context = sig
    module M : S

    val inject : M.tree -> tree

    val project : tree -> M.tree
  end

  type proof_version_expanded =
    Tezos_context_helpers.Context.proof_version_expanded

  let decode_proof_version = Tezos_context_helpers.Context.decode_proof_version

  let proof_context_of_proof_version_expanded :
      proof_version_expanded -> (module Proof_context) = function
    | {is_binary = true; _} -> (module Proof_context_binary)
    | {is_binary = false; _} -> (module Proof_context)

  let proof_context ~kind proof =
    match decode_proof_version proof.Proof.version with
    | Error `Invalid_proof_version ->
        Lwt.fail_with "Environment_context.verify_tree_proof: Invalid version"
    | Ok v ->
        if kind = `Tree && v.is_stream then
          Lwt.fail_with
            "Environment_context.verify_tree_proof: Received stream proof"
        else if kind = `Stream && not v.is_stream then
          Lwt.fail_with
            "Environment_context.verify_stream_proof: Received tree proof"
        else Lwt.return_ok (proof_context_of_proof_version_expanded v)

  let verify_tree_proof proof (f : tree -> (tree * 'a) Lwt.t) =
    let open Lwt_result_syntax in
    let* (module Proof_context) = proof_context ~kind:`Tree proof in
    let* tree, r =
      Proof_context.M.verify_tree_proof proof (fun tree ->
          let tree = Proof_context.inject tree in
          let*! tree, r = f tree in
          Lwt.return (Proof_context.project tree, r))
    in
    return (Proof_context.inject tree, r)

  let verify_stream_proof proof (f : tree -> (tree * 'a) Lwt.t) =
    let open Lwt_result_syntax in
    let* (module Proof_context) = proof_context ~kind:`Stream proof in
    let* tree, r =
      Proof_context.M.verify_stream_proof proof (fun tree ->
          let tree = Proof_context.inject tree in
          let*! tree, r = f tree in
          Lwt.return (Proof_context.project tree, r))
    in
    return (Proof_context.inject tree, r)

  let equal_config = Tezos_context_sigs.Config.equal

  type cache_key = Environment_cache.key

  type block_cache = {
    context_hash : Tezos_crypto.Hashed.Context_hash.t;
    cache : cache;
  }

  type source_of_cache =
    [ `Force_load
    | `Load
    | `Lazy
    | `Inherited of block_cache * Tezos_crypto.Hashed.Context_hash.t ]

  type builder = Environment_cache.key -> cache_value tzresult Lwt.t

  module Cache = struct
    type key = Environment_cache.key

    type value = cache_value = ..

    type identifier = Environment_cache.identifier

    type size = Environment_cache.size

    type index = Environment_cache.index

    module Events = struct
      open Internal_event.Simple

      let section = ["protocol_cache"]

      let start_loading_cache =
        declare_0
          ~section
          ~level:Info
          ~name:"start_loading_cache"
          ~msg:"start loading cache now"
          ()

      let stop_loading_cache =
        declare_0
          ~section
          ~level:Info
          ~name:"stop_loading_cache"
          ~msg:"stop loading cache now"
          ()

      let start_loading_cache_lazily =
        declare_0
          ~section
          ~level:Debug
          ~name:"start_loading_cache_lazily"
          ~msg:"start loading cache lazily"
          ()

      let stop_loading_cache_lazily =
        declare_0
          ~section
          ~level:Debug
          ~name:"stop_loading_cache_lazily"
          ~msg:"stop loading cache lazily"
          ()

      let emit = Internal_event.Simple.emit

      let observe start_event stop_event f =
        let open Lwt_result_syntax in
        let*! () = emit start_event () in
        let* ret = f () in
        let*! () = emit stop_event () in
        return ret
    end

    let key_of_identifier = Environment_cache.key_of_identifier

    let identifier_of_key = Environment_cache.identifier_of_key

    let pp fmt (Context {cache; _}) = Environment_cache.pp fmt cache

    let cache_number_path = ["number_of_caches"]

    let cache_path cache_index = ["cache"; string_of_int cache_index]

    let cache_limit_path cache = cache_path cache @ ["limit"]

    let get_cache_number ctxt =
      let open Lwt_syntax in
      let+ cn = find ctxt cache_number_path in
      match cn with
      | None -> 0
      | Some v -> Data_encoding.(Binary.of_bytes_exn int31 v)

    let set_cache_number ctxt cache_number =
      if cache_number = 0 then Lwt.return ctxt
      else
        let bytes = Data_encoding.(Binary.to_bytes_exn int31) cache_number in
        add ctxt cache_number_path bytes

    let get_cache_limit ctxt cache_handle =
      let open Lwt_syntax in
      let+ c = find ctxt (cache_limit_path cache_handle) in
      Option.map Data_encoding.(Binary.of_bytes_exn int31) c

    let set_cache_limit ctxt cache_handle limit =
      let path = cache_limit_path cache_handle in
      let bytes = Data_encoding.(Binary.to_bytes_exn int31) limit in
      add ctxt path bytes

    let set_cache_layout (Context ctxt) layout =
      let open Lwt_syntax in
      let cache = Environment_cache.from_layout layout in
      let ctxt = Context {ctxt with cache} in
      let cache_number = List.length layout in
      let* ctxt = set_cache_number ctxt cache_number in
      List.fold_left_i_s
        (fun i ctxt limit -> set_cache_limit ctxt i limit)
        ctxt
        layout

    let get_cache_layout ctxt =
      let open Lwt_syntax in
      let* n = get_cache_number ctxt in
      List.map_s
        (fun index ->
          let* o = get_cache_limit ctxt index in
          match o with
          | None ->
              (*

                 [set_cache_layout] must be called at the beginning of
                 each protocol activation so that the storage contains
                 a consistent description of the layout.  If this
                 invariant holds, then there always is a limit in the
                 context.

              *)
              assert false
          | Some limit -> Lwt.return limit)
        (0 -- (n - 1))

    let update (Context ctxt) key value =
      let delayed_value =
        Option.map (fun (value, index) -> (delay value, index)) value
      in
      let cache = Environment_cache.update ctxt.cache key delayed_value in
      Context {ctxt with cache}

    let cache_domain_path = ["domain"]

    let sync (Context ctxt) ~cache_nonce =
      let open Environment_cache in
      let open Data_encoding in
      let cache, domain = sync ctxt.cache ~cache_nonce in
      let bytes = Binary.to_bytes_exn domain_encoding domain in
      let ctxt = Context {ctxt with cache} in
      add ctxt cache_domain_path bytes

    let clear (Context ctxt) =
      Context {ctxt with cache = Environment_cache.clear ctxt.cache}

    let list_keys (Context {cache; _}) = Environment_cache.list_keys cache

    let future_cache_expectation (Context ctxt) ~time_in_blocks =
      let open Environment_cache in
      let cache = future_cache_expectation ctxt.cache ~time_in_blocks in
      Context {ctxt with cache}

    let find_domain ctxt =
      let open Lwt_syntax in
      let+ v = find ctxt cache_domain_path in
      Option.map
        (Data_encoding.Binary.of_bytes_exn Environment_cache.domain_encoding)
        v

    let find (Context {cache; _}) key =
      Option.map_s (fun value -> value ()) (Environment_cache.find cache key)

    let load ctxt inherited ~value_of_key =
      let open Lwt_syntax in
      let open Environment_cache in
      let* o = find_domain ctxt in
      match o with
      | None ->
          (*

               This case can happen if a reorganization occurs on the
               very first block of the protocol that introduces the
               cache.

               Indeed, in the first block, the predecessor block had no
               cache so no domain can be found in the storage. However,
               a cache can be inherited from a block in a canceled
               chain.

            *)
          return_ok @@ clear inherited
      | Some domain -> from_cache inherited domain ~value_of_key

    let load_now ctxt cache builder =
      let open Lwt_result_syntax in
      load ctxt cache ~value_of_key:(fun key ->
          let* value = builder key in
          return (delay value))

    let load_on_demand ctxt cache builder =
      let open Lwt_syntax in
      let builder key =
        let* r = builder key in
        match r with
        | Error _ ->
            (*

               This error is critical as it means that there have been a
               cached [value] for [key] in the past but that [builder] is
               unable to build it again. We stop everything at this point
               because a node cannot run if it does not have the same
               cache as other nodes in the chain.

            *)
            Lwt.fail_with
              "Environment_context.load_on_demand: Unable to load value"
        | Ok value -> Lwt.return value
      in
      load ctxt cache ~value_of_key:(fun key ->
          let lazy_value =
            let cache = ref None in
            fun () ->
              match !cache with
              | Some value -> return value
              | None ->
                  let+ r = builder key in
                  cache := Some r ;
                  r
          in
          return_ok lazy_value)

    let load_cache ctxt cache mode builder =
      Events.(
        match mode with
        | `Load ->
            observe start_loading_cache stop_loading_cache @@ fun () ->
            load_now ctxt cache builder
        | `Lazy ->
            observe start_loading_cache_lazily stop_loading_cache_lazily
            @@ fun () -> load_on_demand ctxt cache builder)

    let ensure_valid_recycling (Context ctxt) cache =
      let open Lwt_syntax in
      let* layout = get_cache_layout (Context ctxt) in
      if Environment_cache.compatible_layout cache layout then Lwt.return cache
      else Lwt.return (Environment_cache.from_layout layout)

    let key_rank (Context ctxt) key = Environment_cache.key_rank ctxt.cache key

    let cache_size (Context ctxt) ~cache_index =
      Environment_cache.cache_size ctxt.cache ~cache_index

    let cache_size_limit (Context ctxt) ~cache_index =
      Environment_cache.cache_size_limit ctxt.cache ~cache_index

    module Internal_for_tests = struct
      let same_cache_domains ctxt ctxt' =
        let open Lwt_syntax in
        let* domain = find_domain ctxt in
        let* domain' = find_domain ctxt' in
        return_ok
        @@ Option.equal
             Environment_cache.Internal_for_tests.equal_domain
             domain
             domain'
    end
  end

  let load_cache (Context ctxt) mode builder =
    let open Lwt_syntax in
    match mode with
    | `Inherited ({context_hash; cache}, predecessor_context_hash) ->
        if
          Tezos_crypto.Hashed.Context_hash.equal
            context_hash
            predecessor_context_hash
        then
          (*

             We can safely reuse the cache of the predecessor block.

          *)
          return_ok cache
        else
          (*

             The client of [load_cache] has provided a cache that is not
             the cache of the predecessor but the predecessor and the
             block have a common ancestor. Therefore, the inherited
             cache is supposed to contain many entries that can be
             recycled to build the new cache.

          *)
          let* cache = Cache.ensure_valid_recycling (Context ctxt) cache in
          Cache.load_cache (Context ctxt) cache `Load builder
    | (`Load | `Lazy) as mode ->
        let* layout = Cache.get_cache_layout (Context ctxt) in
        let cache = Environment_cache.from_layout layout in
        Cache.load_cache (Context ctxt) cache mode builder

  (**

     The following cache is for the cache to avoid reloading the cache from the
     context when it has been used in the last cache-related operations.

     The cache is indexed by the block hash that has produced it.

     Notice that there is no guarantee that, after a call to [load_cache b], the
     [cache_cache] holds the cache of the block [b]. Indeed, a subsequent call
     to [load_cache bb] will take precedence. This is true even if the promise
     for [b] has not resolved yet. Either way, whatever the pattern of
     concurrent calls, the cache is safe in that:

     - The cache that is returned by [load_cache b] is always the cache for the
       block [b].
     - If an error occurs during the loading of a cache, then the cache-cache
       simply becomes empty.

  *)
  module Cache_cache =
    Aches_lwt.Lache.Make_result (Aches.Rache.SingletonTransferMap (Block_hash))

  let cache_cache : (cache, error trace) Cache_cache.t =
    (* The cache is a singleton cache, this is set during the instantiation of
       the module in the functor application above. This is why [-1] is an
       acceptable value for the size limit: it is ignored and the functor's
       value is used instead. *)
    Cache_cache.create (-1)

  let load_cache block_hash (Context ctxt) mode builder =
    let open Lwt_result_syntax in
    let* cache =
      match mode with
      | `Force_load ->
          let p = load_cache (Context ctxt) `Load builder in
          Cache_cache.put cache_cache block_hash p ;
          p
      | (`Load | `Lazy | `Inherited _) as mode ->
          Cache_cache.bind_or_put
            cache_cache
            block_hash
            (fun _block_hash -> load_cache (Context ctxt) mode builder)
            (fun p -> Lwt.return p)
    in
    return (Context {ctxt with cache})

  (* misc *)

  let set_protocol (Context ({ops = (module Ops); ctxt; _} as c)) protocol_hash
      =
    let open Lwt_syntax in
    let+ ctxt = Ops.set_protocol ctxt protocol_hash in
    Context {c with ctxt}

  let get_protocol (Context {ops = (module Ops); ctxt; _}) =
    Ops.get_protocol ctxt

  let fork_test_chain (Context ({ops = (module Ops); ctxt; _} as c)) ~protocol
      ~expiration =
    let open Lwt_syntax in
    let+ ctxt = Ops.fork_test_chain ctxt ~protocol ~expiration in
    Context {c with ctxt}

  let get_hash_version (Context {ops = (module Ops); ctxt; _}) =
    Ops.get_hash_version ctxt

  let set_hash_version (Context ({ops = (module Ops); ctxt; _} as c)) v =
    let open Lwt_result_syntax in
    let+ ctxt = Ops.set_hash_version ctxt v in
    Context {c with ctxt}
end

module Register (C : S) = struct
  type _ Context.kind += Context : C.t Context.kind

  let equality_witness : (C.t, C.tree) Context.equality_witness =
    Context.equality_witness ()

  let ops = (module C : S with type t = 'ctxt and type tree = 'tree)
end

type legacy_validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_finalized_block_level : Int32.t;
  last_preserved_block_level : Int32.t;
}

let lift_legacy_validation_result
    (legacy_validation_result : legacy_validation_result) : validation_result =
  {
    context = legacy_validation_result.context;
    fitness = legacy_validation_result.fitness;
    message = legacy_validation_result.message;
    max_operations_ttl = legacy_validation_result.max_operations_ttl;
    last_finalized_block_level =
      legacy_validation_result.last_allowed_fork_level;
    last_preserved_block_level =
      legacy_validation_result.last_allowed_fork_level;
  }

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Tezos_crypto.Hashed.Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

type header_context_hash_semantics =
  | Resulting_context
  | Predecessor_resulting_context
