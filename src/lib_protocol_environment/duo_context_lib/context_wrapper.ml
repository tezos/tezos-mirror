(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type IRMIN_CONTEXT =
  Tezos_context_sigs.Context.TEZOS_CONTEXT
    with type memory_context_tree := Tezos_context_memory.Context.tree

module type BRASSAIA_CONTEXT =
  Tezos_context_sigs.Context.TEZOS_CONTEXT
    with type memory_context_tree :=
      Tezos_context_brassaia_memory.Tezos_context_memory.Context.tree

module Make
    (Irmin_Context : IRMIN_CONTEXT)
    (Brassaia_Context : BRASSAIA_CONTEXT) =
struct
  open Lwt_syntax

  type index = {
    irmin_index : Irmin_Context.index;
    brassaia_index : Brassaia_Context.index;
  }

  type t = {
    irmin_context : Irmin_Context.t;
    brassaia_context : Brassaia_Context.t;
  }

  type key = Irmin_Context.key

  type value = Irmin_Context.value

  type tree = {
    irmin_tree : Irmin_Context.tree;
    brassaia_tree : Brassaia_Context.tree;
  }

  module Tree = struct
    module Irmin_Tree = Irmin_Context.Tree
    module Brassaia_Tree = Brassaia_Context.Tree

    let mem : tree -> key -> bool Lwt.t =
     fun t key ->
      let* b1 = Irmin_Tree.mem t.irmin_tree key in
      let+ b2 = Brassaia_Tree.mem t.brassaia_tree key in
      assert (b1 = b2) ;
      b1

    let mem_tree : tree -> key -> bool Lwt.t =
     fun t key ->
      let* b1 = Irmin_Tree.mem_tree t.irmin_tree key in
      let+ b2 = Brassaia_Tree.mem_tree t.brassaia_tree key in
      assert (b1 = b2) ;
      b1

    let find : tree -> key -> value option Lwt.t =
     fun t key ->
      let* v1 = Irmin_Tree.find t.irmin_tree key in
      let+ v2 = Brassaia_Tree.find t.brassaia_tree key in
      assert (v1 = v2) ;
      v1

    let find_tree : tree -> key -> tree option Lwt.t =
     fun t key ->
      let* irmin_tree = Irmin_Tree.find_tree t.irmin_tree key in
      let+ brassaia_tree = Brassaia_Tree.find_tree t.brassaia_tree key in
      match (irmin_tree, brassaia_tree) with
      | Some irmin_tree, Some brassaia_tree -> Some {irmin_tree; brassaia_tree}
      | None, None -> None
      | _ -> Fmt.failwith "Received Some tree and None"

    let list :
        tree -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t
        =
     fun t ?offset ?length key ->
      let* irmin_list = Irmin_Tree.list t.irmin_tree ?offset ?length key in
      let+ brassaia_list =
        Brassaia_Tree.list t.brassaia_tree ?offset ?length key
      in
      assert (List.compare_lengths irmin_list brassaia_list = 0) ;
      List.map
        (fun (key, irmin_tree) ->
          let brassaia_tree =
            List.find (fun (key2, _) -> String.equal key key2) brassaia_list
            |> Option.value
                 ~default:(Fmt.failwith "No value associated to %s" key)
            |> snd
          in
          (key, {irmin_tree; brassaia_tree}))
        irmin_list

    let length : tree -> key -> int Lwt.t =
     fun t key ->
      let* length1 = Irmin_Tree.length t.irmin_tree key in
      let+ length2 = Brassaia_Tree.length t.brassaia_tree key in
      assert (length1 = length2) ;
      length1

    let add : tree -> key -> value -> tree Lwt.t =
     fun t key value ->
      let* irmin_tree = Irmin_Tree.add t.irmin_tree key value in
      let+ brassaia_tree = Brassaia_Tree.add t.brassaia_tree key value in
      {irmin_tree; brassaia_tree}

    let add_tree : tree -> key -> tree -> tree Lwt.t =
     fun t key tree ->
      let* irmin_tree = Irmin_Tree.add_tree t.irmin_tree key tree.irmin_tree in
      let+ brassaia_tree =
        Brassaia_Tree.add_tree t.brassaia_tree key tree.brassaia_tree
      in
      {irmin_tree; brassaia_tree}

    let remove : tree -> key -> tree Lwt.t =
     fun t key ->
      let* irmin_tree = Irmin_Tree.remove t.irmin_tree key in
      let+ brassaia_tree = Brassaia_Tree.remove t.brassaia_tree key in
      {irmin_tree; brassaia_tree}

    let fold :
        ?depth:Tezos_context_sigs.Context.depth ->
        tree ->
        key ->
        order:[`Sorted | `Undefined] ->
        init:'a ->
        f:(key -> tree -> 'a -> 'a Lwt.t) ->
        'a Lwt.t =
     fun ?depth t key ~order ~init ~f ->
      let f_irmin key irmin_tree acc = f key {t with irmin_tree} acc in
      let res1 =
        Irmin_Tree.fold ?depth t.irmin_tree key ~order ~init ~f:f_irmin
      in
      let f_brassaia key brassaia_tree acc = f key {t with brassaia_tree} acc in
      let _res2 =
        Brassaia_Tree.fold ?depth t.brassaia_tree key ~order ~init ~f:f_brassaia
      in
      (* assert (res1 = res2) ; *)
      res1

    let config : tree -> Tezos_context_sigs.Config.t =
     fun t ->
      let config1 = Irmin_Tree.config t.irmin_tree in
      let config2 = Brassaia_Tree.config t.brassaia_tree in
      assert (Tezos_context_sigs.Config.equal config1 config2) ;
      config1

    let empty : t -> tree =
     fun t ->
      let irmin_tree = Irmin_Tree.empty t.irmin_context in
      let brassaia_tree = Brassaia_Tree.empty t.brassaia_context in
      {irmin_tree; brassaia_tree}

    let irmin_tree : Irmin_Context.tree -> tree =
     fun irmin_tree -> {irmin_tree; brassaia_tree = Obj.magic irmin_tree}

    let brassaia_tree : Brassaia_Context.tree -> tree =
     fun brassaia_tree -> {brassaia_tree; irmin_tree = Obj.magic brassaia_tree}

    let is_empty : tree -> bool =
     fun t ->
      let bool1 = Irmin_Tree.is_empty t.irmin_tree in
      let bool2 = Brassaia_Tree.is_empty t.brassaia_tree in
      assert (bool1 = bool2) ;
      bool1

    let kind : tree -> Tezos_context_sigs.Context.Kind.t =
     fun t ->
      let kind1 = Irmin_Tree.kind t.irmin_tree in
      let kind2 = Brassaia_Tree.kind t.brassaia_tree in
      assert (kind1 = kind2) ;
      kind1

    let to_value : tree -> value option Lwt.t =
     fun t ->
      let* value1 = Irmin_Tree.to_value t.irmin_tree in
      let+ value2 = Brassaia_Tree.to_value t.brassaia_tree in
      match (value1, value2) with
      | Some value1, Some value2 ->
          assert (value1 = value2) ;
          Some value1
      | None, None -> None
      | _ -> Fmt.failwith "Received Some value and None"

    let of_value : t -> value -> tree Lwt.t =
     fun t value ->
      let* irmin_tree = Irmin_Tree.of_value t.irmin_context value in
      let+ brassaia_tree = Brassaia_Tree.of_value t.brassaia_context value in
      {irmin_tree; brassaia_tree}

    let hash : tree -> Context_hash.t =
     fun t ->
      let context_hash1 = Irmin_Tree.hash t.irmin_tree in
      let context_hash2 = Brassaia_Tree.hash t.brassaia_tree in
      assert (context_hash1 = context_hash2) ;
      context_hash1

    let equal : tree -> tree -> bool =
     fun t1 t2 ->
      let bool1 = Irmin_Tree.equal t1.irmin_tree t2.irmin_tree in
      let bool2 = Brassaia_Tree.equal t1.brassaia_tree t2.brassaia_tree in
      assert (bool1 = bool2) ;
      bool1

    let clear : ?depth:int -> tree -> unit =
     fun ?depth t ->
      let () = Irmin_Tree.clear ?depth t.irmin_tree in
      Brassaia_Tree.clear ?depth t.brassaia_tree

    let pp : Format.formatter -> tree -> unit =
     fun ppf t ->
      Irmin_Tree.pp ppf t.irmin_tree ;
      Brassaia_Tree.pp ppf t.brassaia_tree

    type raw = Irmin_Tree.raw
    (*   [`Tree of raw Tezos_base.TzPervasives.String.Map.t | `Value of value] *)

    let raw_encoding : raw Tezos_base.TzPervasives.Data_encoding.t =
      Irmin_Tree.raw_encoding

    let to_raw : tree -> raw Lwt.t =
     fun t ->
      let* raw1 = Irmin_Tree.to_raw t.irmin_tree in
      let+ raw2 = Brassaia_Tree.to_raw t.brassaia_tree in
      assert (raw1 = raw2) ;
      raw1

    let of_raw : raw -> tree =
     fun raw ->
      let irmin_tree = Irmin_Tree.of_raw raw in
      let brassaia_tree = Brassaia_Tree.of_raw raw in
      {irmin_tree; brassaia_tree}

    let unshallow : tree -> tree Lwt.t =
     fun t ->
      let* irmin_tree = Irmin_Tree.unshallow t.irmin_tree in
      let+ brassaia_tree = Brassaia_Tree.unshallow t.brassaia_tree in
      {irmin_tree; brassaia_tree}

    type repo = Irmin_Tree.repo

    let make_repo : unit -> repo Lwt.t = Irmin_Tree.make_repo

    let is_shallow : tree -> bool =
     fun t ->
      let bool1 = Irmin_Tree.is_shallow t.irmin_tree in
      let bool2 = Brassaia_Tree.is_shallow t.brassaia_tree in
      assert (bool1 = bool2) ;
      bool1

    let kinded_key : tree -> Irmin_Context.kinded_key option =
     fun t ->
      let kinded_key1 = Irmin_Tree.kinded_key t.irmin_tree in
      let kinded_key2 = Brassaia_Tree.kinded_key t.brassaia_tree in
      assert (kinded_key1 = Obj.magic kinded_key2) ;
      kinded_key1
  end

  module Proof = Irmin_Context.Proof

  let add_protocol : t -> Protocol_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context = Irmin_Context.add_protocol t.irmin_context hash in
    let+ brassaia_context =
      Brassaia_Context.add_protocol t.brassaia_context hash
    in
    {irmin_context; brassaia_context}

  let equal_config config1 config2 =
    let bool1 = Irmin_Context.equal_config config1 config2 in
    let bool2 = Brassaia_Context.equal_config config1 config2 in
    assert (bool1 = bool2) ;
    bool1

  let mem : t -> key -> bool Lwt.t =
   fun t key ->
    let* bool1 = Irmin_Context.mem t.irmin_context key in
    let+ bool2 = Brassaia_Context.mem t.brassaia_context (Obj.magic key) in
    assert (bool1 = bool2) ;
    bool1

  let mem_tree : t -> key -> bool Lwt.t =
   fun t key ->
    let* bool1 = Irmin_Context.mem_tree t.irmin_context key in
    let+ bool2 = Brassaia_Context.mem_tree t.brassaia_context key in
    assert (bool1 = bool2) ;
    bool1

  let find : t -> key -> value option Lwt.t =
   fun t key ->
    let* value1 = Irmin_Context.find t.irmin_context key in
    let+ value2 = Brassaia_Context.find t.brassaia_context key in
    assert (value1 = value2) ;
    value1

  let find_tree : t -> key -> tree option Lwt.t =
   fun t key ->
    let* irmin_tree = Irmin_Context.find_tree t.irmin_context key in
    let+ brassaia_tree = Brassaia_Context.find_tree t.brassaia_context key in
    match (irmin_tree, brassaia_tree) with
    | Some irmin_tree, Some brassaia_tree -> Some {irmin_tree; brassaia_tree}
    | None, None -> None
    | _ -> Fmt.failwith "Received Some tree and None"

  let list :
      t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t =
   fun t ?offset ?length key ->
    let* irmin_list = Irmin_Context.list t.irmin_context ?offset ?length key in
    let+ brassaia_list =
      Brassaia_Context.list t.brassaia_context ?offset ?length key
    in
    assert (List.compare_lengths irmin_list brassaia_list = 0) ;
    List.map
      (fun (key, irmin_tree) ->
        let brassaia_tree =
          List.find (fun (key2, _) -> String.equal key key2) brassaia_list
          |> Option.value
               ~default:(Fmt.failwith "No value associated to %s" key)
          |> snd
        in
        (key, {irmin_tree; brassaia_tree}))
      irmin_list

  let length : t -> key -> int Lwt.t =
   fun t key ->
    let* length1 = Irmin_Context.length t.irmin_context key in
    let+ length2 = Brassaia_Context.length t.brassaia_context key in
    assert (length1 = length2) ;
    length1

  let add : t -> key -> value -> t Lwt.t =
   fun t key value ->
    let* irmin_context = Irmin_Context.add t.irmin_context key value in
    let+ brassaia_context = Brassaia_Context.add t.brassaia_context key value in
    {irmin_context; brassaia_context}

  let add_tree : t -> key -> tree -> t Lwt.t =
   fun t key tree ->
    let* irmin_context =
      Irmin_Context.add_tree t.irmin_context key tree.irmin_tree
    in
    let+ brassaia_context =
      Brassaia_Context.add_tree t.brassaia_context key tree.brassaia_tree
    in
    {irmin_context; brassaia_context}

  let remove : t -> key -> t Lwt.t =
   fun t key ->
    let* irmin_context = Irmin_Context.remove t.irmin_context key in
    let+ brassaia_context = Brassaia_Context.remove t.brassaia_context key in
    {irmin_context; brassaia_context}

  let fold :
      ?depth:Tezos_context_sigs.Context.depth ->
      t ->
      key ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t =
   fun ?depth t key ~order ~init ~f ->
    let f_irmin key irmin_tree acc =
      f key {(Tree.empty t) with irmin_tree} acc
    in
    let res1 =
      Irmin_Context.fold ?depth t.irmin_context key ~order ~init ~f:f_irmin
    in
    let f_brassaia key brassaia_tree acc =
      f key {(Tree.empty t) with brassaia_tree} acc
    in
    let _res2 =
      Brassaia_Context.fold
        ?depth
        t.brassaia_context
        key
        ~order
        ~init
        ~f:f_brassaia
    in
    (* assert (res1 = res2) ; *)
    res1

  let config : t -> Tezos_context_sigs.Config.t =
   fun t ->
    let config1 = Irmin_Context.config t.irmin_context in
    let config2 = Brassaia_Context.config t.brassaia_context in
    assert (Tezos_context_sigs.Config.equal config1 config2) ;
    config1

  let get_protocol : t -> Protocol_hash.t Lwt.t =
   fun t ->
    let* hash1 = Irmin_Context.get_protocol t.irmin_context in
    let+ hash2 = Brassaia_Context.get_protocol t.brassaia_context in
    assert (Protocol_hash.equal hash1 hash2) ;
    hash1

  let fork_test_chain :
      t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t =
   fun t ~protocol ~expiration ->
    let* irmin_context =
      Irmin_Context.fork_test_chain t.irmin_context ~protocol ~expiration
    in
    let+ brassaia_context =
      Brassaia_Context.fork_test_chain t.brassaia_context ~protocol ~expiration
    in
    {irmin_context; brassaia_context}

  let set_hash_version : t -> Context_hash.version -> t tzresult Lwt.t =
   fun t version ->
    let open Lwt_result_syntax in
    let* irmin_context =
      Irmin_Context.set_hash_version t.irmin_context version
    in
    let+ brassaia_context =
      Brassaia_Context.set_hash_version t.brassaia_context version
    in
    {irmin_context; brassaia_context}

  let get_hash_version : t -> Context_hash.version =
   fun t ->
    let version1 = Irmin_Context.get_hash_version t.irmin_context in
    let version2 = Brassaia_Context.get_hash_version t.brassaia_context in
    assert (Context_hash.Version.equal version1 version2) ;
    version1

  let verify_tree_proof :
      Proof.tree Proof.t ->
      (tree -> (tree * 'a) Lwt.t) ->
      ( tree * 'a,
        [ `Proof_mismatch of string
        | `Stream_too_long of string
        | `Stream_too_short of string ] )
      result
      Lwt.t =
   fun proof f ->
    let open Lwt_result_syntax in
    let f_irmin irmin_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.irmin_tree irmin_tree) in
      (tree.irmin_tree, res)
    in
    let* irmin_tree, res1 = Irmin_Context.verify_tree_proof proof f_irmin in
    let f_brassaia brassaia_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.brassaia_tree brassaia_tree) in
      (tree.brassaia_tree, res)
    in
    let+ brassaia_tree, res2 =
      Brassaia_Context.verify_tree_proof proof f_brassaia
    in
    assert (res1 = res2) ;
    ({irmin_tree; brassaia_tree}, res1)

  let verify_stream_proof :
      Proof.stream Proof.t ->
      (tree -> (tree * 'a) Lwt.t) ->
      ( tree * 'a,
        [ `Proof_mismatch of string
        | `Stream_too_long of string
        | `Stream_too_short of string ] )
      result
      Lwt.t =
   fun proof f ->
    let open Lwt_result_syntax in
    let f_irmin irmin_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.irmin_tree irmin_tree) in
      (tree.irmin_tree, res)
    in
    let* irmin_tree, res1 = Irmin_Context.verify_stream_proof proof f_irmin in
    let f_brassaia brassaia_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.brassaia_tree brassaia_tree) in
      (tree.brassaia_tree, res)
    in
    let+ brassaia_tree, res2 =
      Brassaia_Context.verify_stream_proof proof f_brassaia
    in
    assert (res1 = res2) ;
    ({irmin_tree; brassaia_tree}, res1)

  (** Exported functions *)

  let index context =
    let irmin_index = Irmin_Context.index context.irmin_context in
    let brassaia_index = Brassaia_Context.index context.brassaia_context in
    {irmin_index; brassaia_index}

  (** GC functions *)

  let gc : index -> Context_hash.t -> unit Lwt.t =
   fun index context_hash ->
    let* () = Irmin_Context.gc index.irmin_index context_hash in
    Brassaia_Context.gc index.brassaia_index context_hash

  let wait_gc_completion : index -> unit Lwt.t =
   fun index ->
    let* () = Irmin_Context.wait_gc_completion index.irmin_index in
    Brassaia_Context.wait_gc_completion index.brassaia_index

  let is_gc_allowed : index -> bool =
   fun index ->
    let bool1 = Irmin_Context.is_gc_allowed index.irmin_index in
    let bool2 = Brassaia_Context.is_gc_allowed index.brassaia_index in
    assert (bool1 = bool2) ;
    bool1

  let split : index -> unit Lwt.t =
   fun index ->
    let* () = Irmin_Context.split index.irmin_index in
    Brassaia_Context.split index.brassaia_index

  let sync : index -> unit Lwt.t =
   fun index ->
    let* () = Irmin_Context.sync index.irmin_index in
    Brassaia_Context.sync index.brassaia_index

  let exists : index -> Context_hash.t -> bool Lwt.t =
   fun index context_hash ->
    let* bool1 = Irmin_Context.exists index.irmin_index context_hash in
    let+ bool2 = Brassaia_Context.exists index.brassaia_index context_hash in
    assert (bool1 = bool2) ;
    bool1

  let close : index -> unit Lwt.t =
   fun index ->
    let* () = Irmin_Context.close index.irmin_index in
    Brassaia_Context.close index.brassaia_index

  let compute_testchain_chain_id : Block_hash.t -> Chain_id.t =
   fun block_hash ->
    let chain_id1 = Irmin_Context.compute_testchain_chain_id block_hash in
    let chain_id2 = Brassaia_Context.compute_testchain_chain_id block_hash in
    assert (Chain_id.equal chain_id1 chain_id2) ;
    chain_id1

  (** Miscellaneous *)

  let add_predecessor_block_metadata_hash :
      t -> Block_metadata_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context =
      Irmin_Context.add_predecessor_block_metadata_hash t.irmin_context hash
    in
    let+ brassaia_context =
      Brassaia_Context.add_predecessor_block_metadata_hash
        t.brassaia_context
        hash
    in
    {irmin_context; brassaia_context}

  let add_predecessor_ops_metadata_hash :
      t -> Operation_metadata_list_list_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context =
      Irmin_Context.add_predecessor_ops_metadata_hash t.irmin_context hash
    in
    let+ brassaia_context =
      Brassaia_Context.add_predecessor_ops_metadata_hash t.brassaia_context hash
    in
    {irmin_context; brassaia_context}

  let hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t =
   fun ~time ?message t ->
    let context_hash1 = Irmin_Context.hash ~time ?message t.irmin_context in
    let context_hash2 =
      Brassaia_Context.hash ~time ?message t.brassaia_context
    in
    assert (Context_hash.equal context_hash1 context_hash2) ;
    context_hash1

  let commit_test_chain_genesis : t -> Block_header.t -> Block_header.t Lwt.t =
   fun context block_header ->
    let* block_header1 =
      Irmin_Context.commit_test_chain_genesis context.irmin_context block_header
    in
    let+ block_header2 =
      Brassaia_Context.commit_test_chain_genesis
        context.brassaia_context
        block_header
    in
    assert (Block_header.equal block_header1 block_header2) ;
    block_header1

  let get_test_chain : t -> Test_chain_status.t Lwt.t =
   fun t ->
    let* status1 = Irmin_Context.get_test_chain t.irmin_context in
    let+ status2 = Brassaia_Context.get_test_chain t.brassaia_context in
    assert (Test_chain_status.equal status1 status2) ;
    status1

  let add_test_chain : t -> Test_chain_status.t -> t Lwt.t =
   fun t status ->
    let* irmin_context = Irmin_Context.add_test_chain t.irmin_context status in
    let+ brassaia_context =
      Brassaia_Context.add_test_chain t.brassaia_context status
    in
    {irmin_context; brassaia_context}

  let commit :
      time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t =
   fun ~time ?message t ->
    let* context_hash1 = Irmin_Context.commit ~time ?message t.irmin_context in
    let+ context_hash2 =
      Brassaia_Context.commit ~time ?message t.brassaia_context
    in
    assert (Context_hash.equal context_hash1 context_hash2) ;
    context_hash1

  let commit_genesis :
      index ->
      chain_id:Chain_id.t ->
      time:Time.Protocol.t ->
      protocol:Protocol_hash.t ->
      Context_hash.t tzresult Lwt.t =
   fun index ~chain_id ~time ~protocol ->
    let open Lwt_result_syntax in
    let* context_hash1 =
      Irmin_Context.commit_genesis index.irmin_index ~chain_id ~time ~protocol
    in
    let+ context_hash2 =
      Brassaia_Context.commit_genesis
        index.brassaia_index
        ~chain_id
        ~time
        ~protocol
    in
    assert (Context_hash.equal context_hash1 context_hash2) ;
    context_hash1

  let compute_testchain_genesis : Block_hash.t -> Block_hash.t =
   fun block_hash ->
    let block_hash1 = Irmin_Context.compute_testchain_genesis block_hash in
    let block_hash2 = Brassaia_Context.compute_testchain_genesis block_hash in
    assert (Block_hash.equal block_hash1 block_hash2) ;
    block_hash1

  let merkle_tree :
      t ->
      Proof.merkle_leaf_kind ->
      key ->
      Tezos_context_sigs.Context.Proof_types.merkle_tree Lwt.t =
   fun context leaf_kind path ->
    let* proof1 =
      Irmin_Context.merkle_tree context.irmin_context leaf_kind path
    in
    let+ _proof2 =
      Brassaia_Context.merkle_tree context.brassaia_context leaf_kind path
    in
    proof1

  let merkle_tree_v2 :
      t -> Proof.merkle_leaf_kind -> key -> Proof.tree Proof.t Lwt.t =
   fun context leaf_kind path ->
    let* proof1 =
      Irmin_Context.merkle_tree_v2 context.irmin_context leaf_kind path
    in
    let+ _proof2 =
      Brassaia_Context.merkle_tree_v2 context.brassaia_context leaf_kind path
    in
    proof1
end

module Context =
  Make (Tezos_context.Context) (Tezos_context_brassaia.Tezos_context.Context)
module Context_binary =
  Make
    (Tezos_context.Context_binary)
    (Tezos_context_brassaia.Tezos_context.Context_binary)

module Memory_context =
  Make
    (Tezos_context_memory.Context)
    (Tezos_context_brassaia_memory.Tezos_context_memory.Context)

(* module Memory_context_binary = *)
(*   Make *)
(*     (Tezos_context_memory.Context_binary) *)
(*     (Tezos_context_brassaia_memory.Tezos_context_memory.Context_binary) *)
