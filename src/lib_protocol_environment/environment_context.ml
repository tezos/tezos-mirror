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

open Error_monad

let err_implementation_mismatch ~expected ~got =
  Format.kasprintf
    invalid_arg
    "Context implementation mismatch: expecting %s, got %s"
    expected
    got

module type CONTEXT = Environment_context_intf.S

module type VIEW = Environment_context_intf.VIEW

module type TREE = Environment_context_intf.TREE

module type CACHE = Environment_context_intf.CACHE

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

module Int64Map = Map.Make (Int64)

module Context = struct
  type key = string list

  type value = Bytes.t

  type ('ctxt, 'tree) ops =
    (module CONTEXT with type t = 'ctxt and type tree = 'tree)

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
    Ops.add ctxt key value >|= fun ctxt -> Context {c with ctxt}

  let find (Context {ops = (module Ops); ctxt; _}) key = Ops.find ctxt key

  let remove (Context ({ops = (module Ops); ctxt; _} as c)) key =
    Ops.remove ctxt key >|= fun ctxt -> Context {c with ctxt}

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
    match equiv c.equality_witness t.equality_witness with
    | (Some Refl, Some Refl) ->
        Ops.add_tree ctxt key t.tree >|= fun ctxt -> Context {c with ctxt}
    | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

  let find_tree
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) key =
    Ops.find_tree ctxt key
    >|= Option.map (fun tree -> Tree {ops; tree; equality_witness; impl_name})

  let list
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _})
      ?offset ?length key =
    Ops.list ctxt ?offset ?length key >|= fun ls ->
    List.fold_left
      (fun acc (k, tree) ->
        let v = Tree {ops; tree; equality_witness; impl_name} in
        (k, v) :: acc)
      []
      (List.rev ls)

  let fold ?depth
      (Context
        {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) key
      ~order ~init ~f =
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
          {ops = (module Ops) as ops; ctxt; equality_witness; impl_name; _}) v =
      Ops.Tree.of_value ctxt v >|= fun tree ->
      Tree {ops; tree; equality_witness; impl_name}

    let equal (Tree {ops = (module Ops); tree; equality_witness; _}) (Tree t) =
      match equiv equality_witness t.equality_witness with
      | (Some Refl, Some Refl) -> Ops.Tree.equal tree t.tree
      | _ -> false

    let empty
        (Context
          {ops = (module Ops) as ops; equality_witness; ctxt; impl_name; _}) =
      let empty = Ops.Tree.empty ctxt in
      Tree {ops; equality_witness; tree = empty; impl_name}

    let is_empty (Tree {ops = (module Ops); tree; _}) = Ops.Tree.is_empty tree

    let mem (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.mem tree key

    let add (Tree ({ops = (module Ops); tree; _} as c)) key value =
      Ops.Tree.add tree key value >|= fun tree -> Tree {c with tree}

    let find (Tree {ops = (module Ops); tree; _}) key = Ops.Tree.find tree key

    let mem_tree (Tree {ops = (module Ops); tree; _}) key =
      Ops.Tree.mem_tree tree key

    let add_tree (Tree ({ops = (module Ops); _} as c)) key (Tree t) =
      match equiv c.equality_witness t.equality_witness with
      | (Some Refl, Some Refl) ->
          Ops.Tree.add_tree c.tree key t.tree >|= fun tree -> Tree {c with tree}
      | _ -> err_implementation_mismatch ~expected:c.impl_name ~got:t.impl_name

    let find_tree (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.find_tree tree key
      >|= Option.map (fun tree -> Tree {c with tree})

    let remove (Tree ({ops = (module Ops); tree; _} as c)) key =
      Ops.Tree.remove tree key >|= fun tree -> Tree {c with tree}

    let list
        (Tree {ops = (module Ops) as ops; tree; equality_witness; impl_name})
        ?offset ?length key =
      Ops.Tree.list tree ?offset ?length key >|= fun ls ->
      List.fold_left
        (fun acc (k, tree) ->
          let v = Tree {ops; tree; equality_witness; impl_name} in
          (k, v) :: acc)
        []
        (List.rev ls)

    let fold ?depth
        (Tree
          {ops = (module Ops) as ops; tree = t; equality_witness; impl_name})
        key ~order ~init ~f =
      Ops.Tree.fold ?depth t key ~order ~init ~f:(fun k v acc ->
          let v = Tree {ops; tree = v; equality_witness; impl_name} in
          f k v acc)

    let clear ?depth (Tree {ops = (module Ops); tree; _}) =
      Ops.Tree.clear ?depth tree
  end

  type cache_key = Environment_cache.key

  type block_cache = {context_hash : Context_hash.t; cache : cache}

  type source_of_cache =
    [`Force_load | `Load | `Lazy | `Inherited of block_cache * Context_hash.t]

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
        emit start_event () >>= fun () ->
        f () >>=? fun ret ->
        emit stop_event () >>= fun () -> return ret
    end

    let key_of_identifier = Environment_cache.key_of_identifier

    let identifier_of_key = Environment_cache.identifier_of_key

    let pp fmt (Context {cache; _}) = Environment_cache.pp fmt cache

    let cache_number_path = ["number_of_caches"]

    let cache_path cache_index = ["cache"; string_of_int cache_index]

    let cache_limit_path cache = cache_path cache @ ["limit"]

    let get_cache_number ctxt =
      let open Data_encoding in
      find ctxt cache_number_path
      >>= Option.fold_s ~none:0 ~some:(fun v ->
              Lwt.return (Binary.of_bytes_exn int31 v))

    let set_cache_number ctxt cache_number =
      let open Data_encoding in
      if cache_number = 0 then Lwt.return ctxt
      else
        let bytes = (Binary.to_bytes_exn int31) cache_number in
        add ctxt cache_number_path bytes

    let get_cache_limit ctxt cache_handle =
      let open Data_encoding in
      find ctxt (cache_limit_path cache_handle)
      >>= Option.map_s @@ fun v -> Lwt.return ((Binary.of_bytes_exn int31) v)

    let set_cache_limit ctxt cache_handle limit =
      let path = cache_limit_path cache_handle in
      let bytes = Data_encoding.(Binary.to_bytes_exn int31) limit in
      add ctxt path bytes

    let set_cache_layout (Context ctxt) layout =
      let cache = Environment_cache.from_layout layout in
      let ctxt = Context {ctxt with cache} in
      let cache_number = List.length layout in
      set_cache_number ctxt cache_number >>= fun ctxt ->
      List.fold_left_i_s
        (fun i ctxt limit -> set_cache_limit ctxt i limit)
        ctxt
        layout

    let get_cache_layout ctxt =
      get_cache_number ctxt >>= fun n ->
      List.map_s
        (fun index ->
          get_cache_limit ctxt index >>= function
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
      let (cache, domain) = sync ctxt.cache ~cache_nonce in
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
      Data_encoding.(
        find ctxt cache_domain_path
        >>= Option.map_s @@ fun v ->
            Lwt.return
            @@ (Binary.of_bytes_exn Environment_cache.domain_encoding) v)

    let find (Context {cache; _}) key =
      match Environment_cache.find cache key with
      | None -> Lwt.return_none
      | Some value -> value () >|= Option.some

    let load ctxt inherited ~value_of_key =
      let open Environment_cache in
      find_domain ctxt >>= function
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
          return @@ clear inherited
      | Some domain -> from_cache inherited domain ~value_of_key

    let load_now ctxt cache builder =
      load ctxt cache ~value_of_key:(fun key ->
          builder key >>=? fun value -> return (delay value))

    let load_on_demand ctxt cache builder =
      let builder key =
        builder key >>= function
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
              | Some value -> Lwt.return value
              | None ->
                  builder key >>= fun r ->
                  cache := Some r ;
                  Lwt.return r
          in
          return lazy_value)

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
      get_cache_layout (Context ctxt) >>= fun layout ->
      if Environment_cache.compatible_layout cache layout then Lwt.return cache
      else Lwt.return (Environment_cache.from_layout layout)

    let key_rank (Context ctxt) key = Environment_cache.key_rank ctxt.cache key

    let cache_size (Context ctxt) ~cache_index =
      Environment_cache.cache_size ctxt.cache ~cache_index

    let cache_size_limit (Context ctxt) ~cache_index =
      Environment_cache.cache_size_limit ctxt.cache ~cache_index
  end

  let load_cache (Context ctxt) mode builder =
    match mode with
    | `Inherited ({context_hash; cache}, predecessor_context_hash) ->
        if Context_hash.equal context_hash predecessor_context_hash then
          (*

             We can safely reuse the cache of the predecessor block.

          *)
          return cache
        else
          (*

             The client of [load_cache] has provided a cache that is not
             the cache of the predecessor but the predecessor and the
             block have a common ancestor. Therefore, the inherited
             cache is supposed to contain many entries that can be
             recycled to build the new cache.

          *)
          Cache.ensure_valid_recycling (Context ctxt) cache >>= fun cache ->
          Cache.load_cache (Context ctxt) cache `Load builder
    | (`Load | `Lazy) as mode ->
        Cache.get_cache_layout (Context ctxt) >>= fun layout ->
        let cache = Environment_cache.from_layout layout in
        Cache.load_cache (Context ctxt) cache mode builder

  (**

     The following reference contains a cache for the cache to avoid
     reloading the cache from the context when it has been used in the
     last cache-related operations.

     The cache is indexed by the block hash that has produced it.

     Notice that there is no guarantee that, after the execution of
     [load_cache b], [cache_cache] contains the cache of the block
     [b]. Indeed, a concurrent evaluation of [load_cache] may assign
     [cache_cache] before we reach the [return] instruction. This
     cannot endanger safety since the assignment to [cache_cache] is
     atomic and maintains the invariant that the pair [(block_hash,
     cache)] is consistent.

  *)
  let cache_cache : (Block_hash.t * cache) option ref = ref None

  let load_cache block_hash (Context ctxt) mode builder =
    match mode with
    | `Force_load ->
        cache_cache := None ;
        load_cache (Context ctxt) `Load builder >>=? fun cache ->
        cache_cache := Some (block_hash, cache) ;
        return (Context {ctxt with cache})
    | (`Load | `Lazy | `Inherited _) as mode ->
        (match !cache_cache with
        | Some (block_hash', cached_cache)
          when Block_hash.equal block_hash block_hash' ->
            return cached_cache
        | _ ->
            cache_cache := None ;
            load_cache (Context ctxt) mode builder >>=? fun cache ->
            cache_cache := Some (block_hash, cache) ;
            return cache)
        >>=? fun cache -> return (Context {ctxt with cache})

  (* misc *)

  let set_protocol (Context ({ops = (module Ops); ctxt; _} as c)) protocol_hash
      =
    Ops.set_protocol ctxt protocol_hash >|= fun ctxt -> Context {c with ctxt}

  let get_protocol (Context {ops = (module Ops); ctxt; _}) =
    Ops.get_protocol ctxt

  let fork_test_chain (Context ({ops = (module Ops); ctxt; _} as c)) ~protocol
      ~expiration =
    Ops.fork_test_chain ctxt ~protocol ~expiration >|= fun ctxt ->
    Context {c with ctxt}

  let get_hash_version (Context {ops = (module Ops); ctxt; _}) =
    Ops.get_hash_version ctxt

  let set_hash_version (Context ({ops = (module Ops); ctxt; _} as c)) v =
    Ops.set_hash_version ctxt v >|=? fun ctxt -> Context {c with ctxt}
end

module Register (C : CONTEXT) = struct
  type _ Context.kind += Context : C.t Context.kind

  let equality_witness : (C.t, C.tree) Context.equality_witness =
    Context.equality_witness ()

  let ops = (module C : CONTEXT with type t = 'ctxt and type tree = 'tree)
end

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}
