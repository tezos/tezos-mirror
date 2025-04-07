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

module Profiler =
  Tezos_protocol_environment.Environment_profiler.Context_ops_profiler

let pp_print_option pp ppf = function
  | Some v -> Format.fprintf ppf "Some %a" pp v
  | None -> Format.fprintf ppf "None"

module Events = struct
  include Internal_event.Simple

  let section = ["node"; "duo_context"]

  let assertion_failure =
    declare_3
      ~section
      ~level:Warning
      ~name:"assertion_failure"
      ~msg:"{function} returned {res1} for Irmin and {res2} for Brassaia"
      ("res1", Data_encoding.string)
      ("res2", Data_encoding.string)
      ("function", Data_encoding.string)
end

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

  let assert_and_return_result_lwt res1 res2 equal pp1 pp2 function_name
      final_res =
    if equal res1 res2 then Lwt.return final_res
    else
      let+ () =
        Events.(
          emit
            assertion_failure
            ( Format.asprintf "%a" pp1 res1,
              Format.asprintf "%a" pp2 res2,
              function_name ))
      in
      final_res

  let assert_and_return_result res1 res2 equal pp1 pp2 function_name final_res =
    if equal res1 res2 then final_res
    else (
      Events.(
        emit__dont_wait__use_with_care
          assertion_failure
          ( Format.asprintf "%a" pp1 res1,
            Format.asprintf "%a" pp2 res2,
            function_name )) ;
      final_res)

  let option_equal_no_traversal o1 o2 =
    match (o1, o2) with Some _, Some _ | None, None -> true | _ -> false

  let pp_option_no_traversal ppf = function
    | Some _ -> Format.fprintf ppf "Some _"
    | None -> Format.fprintf ppf "None"

  module Tree = struct
    module Irmin_Tree = Irmin_Context.Tree
    module Brassaia_Tree = Brassaia_Context.Tree

    let mem : tree -> key -> bool Lwt.t =
     fun t key ->
      let* bool1 =
        (Irmin_Tree.mem
           t.irmin_tree
           key [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "mem"]])
      in
      let* bool2 =
        (Brassaia_Tree.mem
           t.brassaia_tree
           key
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "mem"]])
      in
      assert_and_return_result_lwt
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.mem"
        bool1

    let mem_tree : tree -> key -> bool Lwt.t =
     fun t key ->
      let* bool1 =
        (Irmin_Tree.mem_tree
           t.irmin_tree
           key
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "mem tree"]])
      in
      let* bool2 =
        (Brassaia_Tree.mem_tree
           t.brassaia_tree
           key
         [@profiler.span_s
           {verbosity = Notice} ["brassaia"; "tree"; "mem tree"]])
      in
      assert_and_return_result_lwt
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.mem_tree"
        bool1

    let find : tree -> key -> value option Lwt.t =
     fun t key ->
      let* value1 =
        (Irmin_Tree.find
           t.irmin_tree
           key [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "find"]])
      in
      let* value2 =
        (Brassaia_Tree.find
           t.brassaia_tree
           key
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "find"]])
      in
      assert_and_return_result_lwt
        value1
        value2
        (Option.equal Bytes.equal)
        (pp_print_option Format.pp_print_bytes)
        (pp_print_option Format.pp_print_bytes)
        "Tree.find"
        value1

    let find_tree : tree -> key -> tree option Lwt.t =
     fun t key ->
      let* irmin_tree =
        (Irmin_Tree.find_tree
           t.irmin_tree
           key
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "find_tree"]])
      in
      let* brassaia_tree =
        (Brassaia_Tree.find_tree
           t.brassaia_tree
           key
         [@profiler.span_s
           {verbosity = Notice} ["brassaia"; "tree"; "find_tree"]])
      in
      let final_res =
        match (irmin_tree, brassaia_tree) with
        | Some irmin_tree, Some brassaia_tree ->
            Some {irmin_tree; brassaia_tree}
        | _ -> None
      in
      assert_and_return_result_lwt
        irmin_tree
        brassaia_tree
        option_equal_no_traversal
        pp_option_no_traversal
        pp_option_no_traversal
        "Tree.find_tree"
        final_res

    let list :
        tree -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t
        =
     fun t ?offset ?length key ->
      let* irmin_list =
        (Irmin_Tree.list
           t.irmin_tree
           ?offset
           ?length
           key [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "list"]])
      in
      let* brassaia_list =
        (Brassaia_Tree.list
           t.brassaia_tree
           ?offset
           ?length
           key
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "list"]])
      in
      let final_res =
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
      in
      let pp_list ppf l =
        Format.pp_print_list (fun ppf (s, _) -> Format.fprintf ppf "%s" s) ppf l
      in
      assert_and_return_result_lwt
        irmin_list
        brassaia_list
        (fun l1 l2 -> List.compare_lengths l1 l2 = 0)
        pp_list
        pp_list
        "Tree.list"
        final_res

    let length : tree -> key -> int Lwt.t =
     fun t key ->
      let* length1 =
        (Irmin_Tree.length
           t.irmin_tree
           key
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "length"]])
      in
      let* length2 =
        (Brassaia_Tree.length
           t.brassaia_tree
           key
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "length"]])
      in
      assert_and_return_result_lwt
        length1
        length2
        Int.equal
        Format.pp_print_int
        Format.pp_print_int
        "Tree.length"
        length1

    let add : tree -> key -> value -> tree Lwt.t =
     fun t key value ->
      let* irmin_tree =
        (Irmin_Tree.add
           t.irmin_tree
           key
           value
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "add"]])
      in
      let+ brassaia_tree =
        (Brassaia_Tree.add
           t.brassaia_tree
           key
           value
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "add"]])
      in
      {irmin_tree; brassaia_tree}

    let add_tree : tree -> key -> tree -> tree Lwt.t =
     fun t key tree ->
      let* irmin_tree =
        (Irmin_Tree.add_tree
           t.irmin_tree
           key
           tree.irmin_tree
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "add_tree"]])
      in
      let+ brassaia_tree =
        (Brassaia_Tree.add_tree
           t.brassaia_tree
           key
           tree.brassaia_tree
         [@profiler.span_s
           {verbosity = Notice} ["brassaia"; "tree"; "add_tree"]])
      in
      {irmin_tree; brassaia_tree}

    let remove : tree -> key -> tree Lwt.t =
     fun t key ->
      let* irmin_tree =
        (Irmin_Tree.remove
           t.irmin_tree
           key
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "remove"]])
      in
      let+ brassaia_tree =
        (Brassaia_Tree.remove
           t.brassaia_tree
           key
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "remove"]])
      in
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
        (Irmin_Tree.fold
           ?depth
           t.irmin_tree
           key
           ~order
           ~init
           ~f:f_irmin
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "fold"]])
      in
      let f_brassaia key brassaia_tree acc =
        (f
           key
           {t with brassaia_tree}
           acc
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "fold"]])
      in
      let _res2 =
        Brassaia_Tree.fold ?depth t.brassaia_tree key ~order ~init ~f:f_brassaia
      in
      (* assert (res1 = res2) ; *)
      res1

    let config : tree -> Tezos_context_sigs.Config.t =
     fun t ->
      let config1 =
        (Irmin_Tree.config
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "config"]])
      in
      let config2 =
        (Brassaia_Tree.config
           t.brassaia_tree
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "config"]])
      in
      assert_and_return_result
        config1
        config2
        Tezos_context_sigs.Config.equal
        Tezos_context_sigs.Config.pp
        Tezos_context_sigs.Config.pp
        "Tree.config"
        config1

    let empty : t -> tree =
     fun t ->
      let irmin_tree =
        (Irmin_Tree.empty
           t.irmin_context
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "empty"]])
      in
      let brassaia_tree =
        (Brassaia_Tree.empty
           t.brassaia_context
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "empty"]])
      in
      {irmin_tree; brassaia_tree}

    let irmin_tree : Irmin_Context.tree -> tree =
     fun irmin_tree -> {irmin_tree; brassaia_tree = Obj.magic irmin_tree}

    let brassaia_tree : Brassaia_Context.tree -> tree =
     fun brassaia_tree -> {brassaia_tree; irmin_tree = Obj.magic brassaia_tree}

    let is_empty : tree -> bool =
     fun t ->
      let bool1 =
        (Irmin_Tree.is_empty
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "is_empty"]])
      in
      let bool2 =
        (Brassaia_Tree.is_empty
           t.brassaia_tree
         [@profiler.span_f
           {verbosity = Notice} ["brassaia"; "tree"; "is_empty"]])
      in
      assert_and_return_result
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.is_empty"
        bool1

    let kind : tree -> Tezos_context_sigs.Context.Kind.t =
     fun t ->
      let kind1 =
        (Irmin_Tree.kind
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "kind"]])
      in
      let kind2 =
        (Brassaia_Tree.kind
           t.brassaia_tree
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "kind"]])
      in
      let pp_kind ppf = function
        | `Value -> Format.fprintf ppf "value"
        | `Tree -> Format.fprintf ppf "tree"
      in
      assert_and_return_result
        kind1
        kind2
        ( = )
        pp_kind
        pp_kind
        "Tree.kind"
        kind1

    let to_value : tree -> value option Lwt.t =
     fun t ->
      let* value1 =
        (Irmin_Tree.to_value
           t.irmin_tree
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "to_value"]])
      in
      let* value2 =
        (Brassaia_Tree.to_value
           t.brassaia_tree
         [@profiler.span_s
           {verbosity = Notice} ["brassaia"; "tree"; "to_value"]])
      in
      assert_and_return_result_lwt
        value1
        value2
        Option.(equal Bytes.equal)
        (pp_print_option Format.pp_print_bytes)
        (pp_print_option Format.pp_print_bytes)
        "Tree.to_value"
        value1

    let of_value : t -> value -> tree Lwt.t =
     fun t value ->
      let* irmin_tree =
        (Irmin_Tree.of_value
           t.irmin_context
           value
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "of_value"]])
      in
      let+ brassaia_tree =
        (Brassaia_Tree.of_value
           t.brassaia_context
           value
         [@profiler.span_s
           {verbosity = Notice} ["brassaia"; "tree"; "of_value"]])
      in
      {irmin_tree; brassaia_tree}

    let hash : tree -> Context_hash.t =
     fun t ->
      let context_hash1 =
        (Irmin_Tree.hash
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "hash"]])
      in
      let context_hash2 =
        (Brassaia_Tree.hash
           t.brassaia_tree
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "hash"]])
      in
      assert_and_return_result
        context_hash1
        context_hash2
        Context_hash.equal
        Context_hash.pp
        Context_hash.pp
        "Tree.hash"
        context_hash1

    let equal : tree -> tree -> bool =
     fun t1 t2 ->
      let bool1 =
        (Irmin_Tree.equal
           t1.irmin_tree
           t2.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "equal"]])
      in
      let bool2 =
        (Brassaia_Tree.equal
           t1.brassaia_tree
           t2.brassaia_tree
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "equal"]])
      in
      assert_and_return_result
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.equal"
        bool1

    let clear : ?depth:int -> tree -> unit =
     fun ?depth t ->
      let () =
        (Irmin_Tree.clear
           ?depth
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "clear"]])
      in
      (Brassaia_Tree.clear
         ?depth
         t.brassaia_tree
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "clear"]])

    let pp : Format.formatter -> tree -> unit =
     fun ppf t ->
      Irmin_Tree.pp
        ppf
        t.irmin_tree
      [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "pp"]] ;
      Brassaia_Tree.pp
        ppf
        t.brassaia_tree
      [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "pp"]]

    type raw = Irmin_Tree.raw

    let raw_encoding : raw Tezos_base.TzPervasives.Data_encoding.t =
      Irmin_Tree.raw_encoding

    let to_raw : tree -> raw Lwt.t =
     fun t ->
      let* raw1 =
        (Irmin_Tree.to_raw
           t.irmin_tree
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "to_raw"]])
      in
      let+ raw2 =
        (Brassaia_Tree.to_raw
           t.brassaia_tree
         [@profiler.span_s {verbosity = Notice} ["brassaia"; "tree"; "to_raw"]])
      in
      let equal raw1 raw2 =
        match (raw1, raw2) with
        | `Value b1, `Value b2 -> Bytes.equal b1 b2
        | `Tree _, `Tree _ -> true
        | _ -> false
      in
      let pp ppf = function
        | `Value b -> Format.fprintf ppf "Value %s" (Bytes.to_string b)
        | `Tree _ -> Format.fprintf ppf "Tree"
      in
      assert_and_return_result raw1 raw2 equal pp pp "Tree.to_raw" raw1

    let of_raw : raw -> tree =
     fun raw ->
      let irmin_tree =
        (Irmin_Tree.of_raw
           raw
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "of_raw"]])
      in
      let brassaia_tree =
        (Brassaia_Tree.of_raw
           raw
         [@profiler.span_f {verbosity = Notice} ["brassaia"; "tree"; "of_raw"]])
      in
      {irmin_tree; brassaia_tree}

    let unshallow : tree -> tree Lwt.t =
     fun t ->
      let* irmin_tree =
        (Irmin_Tree.unshallow
           t.irmin_tree
         [@profiler.span_s {verbosity = Notice} ["irmin"; "tree"; "unshallow"]])
      in
      let+ brassaia_tree =
        (Brassaia_Tree.unshallow
           t.brassaia_tree
         [@profiler.span_s
           {verbosity = Notice} ["brasssaia"; "tree"; "unshallow"]])
      in
      {irmin_tree; brassaia_tree}

    type repo = Irmin_Tree.repo

    let make_repo : unit -> repo Lwt.t = Irmin_Tree.make_repo

    let is_shallow : tree -> bool =
     fun t ->
      let bool1 =
        (Irmin_Tree.is_shallow
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "is_shallow"]])
      in
      let bool2 =
        (Brassaia_Tree.is_shallow
           t.brassaia_tree
         [@profiler.span_f
           {verbosity = Notice} ["brassaia"; "tree"; "is_shallow"]])
      in
      assert_and_return_result
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.is_shallow"
        bool1

    let kinded_key : tree -> Irmin_Context.kinded_key option =
     fun t ->
      let kinded_key1 =
        (Irmin_Tree.kinded_key
           t.irmin_tree
         [@profiler.span_f {verbosity = Notice} ["irmin"; "tree"; "kinded_key"]])
      in
      let kinded_key2 =
        (Brassaia_Tree.kinded_key
           t.brassaia_tree
         [@profiler.span_f
           {verbosity = Notice} ["brassaia"; "tree"; "kinded_key"]])
      in
      let pp ppf = function
        | `Node _node_key -> Format.fprintf ppf "Node"
        | `Value _value_key -> Format.fprintf ppf "Value"
      in
      assert_and_return_result
        kinded_key1
        kinded_key2
        (fun k1 k2 -> k1 = Obj.magic k2)
        (pp_print_option pp)
        (pp_print_option pp)
        "Tree.kinded_key"
        kinded_key1
  end

  module Proof = Irmin_Context.Proof

  let add_protocol : t -> Protocol_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context =
      (Irmin_Context.add_protocol
         t.irmin_context
         hash [@profiler.span_s {verbosity = Notice} ["irmin"; "add_protocol"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add_protocol
         t.brassaia_context
         hash
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "add_protocol"]])
    in
    {irmin_context; brassaia_context}

  let equal_config config1 config2 =
    let bool1 =
      (Irmin_Context.equal_config
         config1
         config2
       [@profiler.span_f {verbosity = Notice} ["irmin"; "equal_config"]])
    in
    let bool2 =
      (Brassaia_Context.equal_config
         config1
         config2
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "equal_config"]])
    in
    assert_and_return_result
      bool1
      bool2
      Bool.equal
      Format.pp_print_bool
      Format.pp_print_bool
      "equal_config"
      bool1

  let mem : t -> key -> bool Lwt.t =
   fun t key ->
    let* bool1 =
      (Irmin_Context.mem
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "mem"]])
    in
    let* bool2 =
      (Brassaia_Context.mem
         t.brassaia_context
         (Obj.magic key)
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "mem"]])
    in
    assert_and_return_result_lwt
      bool1
      bool2
      Bool.equal
      Format.pp_print_bool
      Format.pp_print_bool
      "mem"
      bool1

  let mem_tree : t -> key -> bool Lwt.t =
   fun t key ->
    let* bool1 =
      (Irmin_Context.mem_tree
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "mem_tree"]])
    in
    let* bool2 =
      (Brassaia_Context.mem_tree
         t.brassaia_context
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "mem_tree"]])
    in
    assert_and_return_result_lwt
      bool1
      bool2
      Bool.equal
      Format.pp_print_bool
      Format.pp_print_bool
      "mem_tree"
      bool1

  let find : t -> key -> value option Lwt.t =
   fun t key ->
    let* value1 =
      (Irmin_Context.find
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "find"]])
    in
    let* value2 =
      (Brassaia_Context.find
         t.brassaia_context
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "find"]])
    in
    assert_and_return_result_lwt
      value1
      value2
      (Option.equal Bytes.equal)
      (pp_print_option Format.pp_print_bytes)
      (pp_print_option Format.pp_print_bytes)
      "find"
      value1

  let find_tree : t -> key -> tree option Lwt.t =
   fun t key ->
    let* irmin_tree =
      (Irmin_Context.find_tree
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "find_tree"]])
    in
    let* brassaia_tree =
      (Brassaia_Context.find_tree
         t.brassaia_context
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "find_tree"]])
    in
    let final_res =
      match (irmin_tree, brassaia_tree) with
      | Some irmin_tree, Some brassaia_tree -> Some {irmin_tree; brassaia_tree}
      | _ -> None
    in
    assert_and_return_result_lwt
      irmin_tree
      brassaia_tree
      option_equal_no_traversal
      pp_option_no_traversal
      pp_option_no_traversal
      "find_tree"
      final_res

  let list :
      t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t =
   fun t ?offset ?length key ->
    let* irmin_list =
      (Irmin_Context.list
         t.irmin_context
         ?offset
         ?length
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "list"]])
    in
    let* brassaia_list =
      (Brassaia_Context.list
         t.brassaia_context
         ?offset
         ?length
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "list"]])
    in
    let final_res =
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
    in
    let pp_list ppf l =
      Format.pp_print_list (fun ppf (s, _) -> Format.fprintf ppf "%s" s) ppf l
    in
    assert_and_return_result_lwt
      irmin_list
      brassaia_list
      (fun l1 l2 -> List.compare_lengths l1 l2 = 0)
      pp_list
      pp_list
      "list"
      final_res

  let length : t -> key -> int Lwt.t =
   fun t key ->
    let* length1 =
      (Irmin_Context.length
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "length"]])
    in
    let* length2 =
      (Brassaia_Context.length
         t.brassaia_context
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "length"]])
    in
    assert_and_return_result_lwt
      length1
      length2
      Int.equal
      Format.pp_print_int
      Format.pp_print_int
      "length"
      length1

  let add : t -> key -> value -> t Lwt.t =
   fun t key value ->
    let* irmin_context =
      (Irmin_Context.add
         t.irmin_context
         key
         value [@profiler.span_s {verbosity = Notice} ["irmin"; "add"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add
         t.brassaia_context
         key
         value [@profiler.span_s {verbosity = Notice} ["brassaia"; "add"]])
    in
    {irmin_context; brassaia_context}

  let add_tree : t -> key -> tree -> t Lwt.t =
   fun t key tree ->
    let* irmin_context =
      (Irmin_Context.add_tree
         t.irmin_context
         key
         tree.irmin_tree
       [@profiler.span_s {verbosity = Notice} ["irmin"; "add_tree"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add_tree
         t.brassaia_context
         key
         tree.brassaia_tree
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "add_tree"]])
    in
    {irmin_context; brassaia_context}

  let remove : t -> key -> t Lwt.t =
   fun t key ->
    let* irmin_context =
      (Irmin_Context.remove
         t.irmin_context
         key [@profiler.span_s {verbosity = Notice} ["irmin"; "remove"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.remove
         t.brassaia_context
         key [@profiler.span_s {verbosity = Notice} ["brassaia"; "remove"]])
    in
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
      (Irmin_Context.fold
         ?depth
         t.irmin_context
         key
         ~order
         ~init
         ~f:f_irmin [@profiler.span_f {verbosity = Notice} ["irmin"; "fold"]])
    in
    let f_brassaia key brassaia_tree acc =
      f key {(Tree.empty t) with brassaia_tree} acc
    in
    let _res2 =
      (Brassaia_Context.fold
         ?depth
         t.brassaia_context
         key
         ~order
         ~init
         ~f:f_brassaia
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "fold"]])
    in
    (* assert (res1 = res2) ; *)
    res1

  let config : t -> Tezos_context_sigs.Config.t =
   fun t ->
    let config1 =
      (Irmin_Context.config
         t.irmin_context
       [@profiler.span_f {verbosity = Notice} ["irmin"; "config"]])
    in
    let config2 =
      (Brassaia_Context.config
         t.brassaia_context
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "config"]])
    in
    assert_and_return_result
      config1
      config2
      Tezos_context_sigs.Config.equal
      Tezos_context_sigs.Config.pp
      Tezos_context_sigs.Config.pp
      "config"
      config1

  let get_protocol : t -> Protocol_hash.t Lwt.t =
   fun t ->
    let* hash1 =
      (Irmin_Context.get_protocol
         t.irmin_context
       [@profiler.span_s {verbosity = Notice} ["irmin"; "get_protocol"]])
    in
    let* hash2 =
      (Brassaia_Context.get_protocol
         t.brassaia_context
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "get_protocol"]])
    in
    assert_and_return_result_lwt
      hash1
      hash2
      Protocol_hash.equal
      Protocol_hash.pp
      Protocol_hash.pp
      "get_protocol"
      hash1

  let fork_test_chain :
      t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t =
   fun t ~protocol ~expiration ->
    let* irmin_context =
      (Irmin_Context.fork_test_chain
         t.irmin_context
         ~protocol
         ~expiration
       [@profiler.span_s {verbosity = Notice} ["irmin"; "fork_test_chain"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.fork_test_chain
         t.brassaia_context
         ~protocol
         ~expiration
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "fork_test_chain"]])
    in
    {irmin_context; brassaia_context}

  let set_hash_version : t -> Context_hash.version -> t tzresult Lwt.t =
   fun t version ->
    let open Lwt_result_syntax in
    let* irmin_context =
      (Irmin_Context.set_hash_version
         t.irmin_context
         version
       [@profiler.span_s {verbosity = Notice} ["irmin"; "set_hash_version"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.set_hash_version
         t.brassaia_context
         version
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "set_hash_version"]])
    in
    {irmin_context; brassaia_context}

  let get_hash_version : t -> Context_hash.version =
   fun t ->
    let version1 =
      (Irmin_Context.get_hash_version
         t.irmin_context
       [@profiler.span_f {verbosity = Notice} ["irmin"; "get_hash_version"]])
    in
    let version2 =
      (Brassaia_Context.get_hash_version
         t.brassaia_context
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "get_hash_version"]])
    in
    assert_and_return_result
      version1
      version2
      Context_hash.Version.equal
      Context_hash.Version.pp
      Context_hash.Version.pp
      "get_hash_version"
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
    let* irmin_tree, res1 =
      (Irmin_Context.verify_tree_proof
         proof
         f_irmin
       [@profiler.span_s {verbosity = Notice} ["irmin"; "verify_tree_proof"]])
    in
    let f_brassaia brassaia_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.brassaia_tree brassaia_tree) in
      (tree.brassaia_tree, res)
    in
    let+ brassaia_tree, res2 =
      (Brassaia_Context.verify_tree_proof
         proof
         f_brassaia
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "verify_tree_proof"]])
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
    let* irmin_tree, res1 =
      (Irmin_Context.verify_stream_proof
         proof
         f_irmin
       [@profiler.span_s {verbosity = Notice} ["irmin"; "verify_stream_proof"]])
    in
    let f_brassaia brassaia_tree =
      let open Lwt_syntax in
      let+ tree, res = f (Tree.brassaia_tree brassaia_tree) in
      (tree.brassaia_tree, res)
    in
    let+ brassaia_tree, res2 =
      (Brassaia_Context.verify_stream_proof
         proof
         f_brassaia
       [@profiler.span_s
         {verbosity = Notice} ["brassaia"; "verify_stream_proof"]])
    in
    assert (res1 = res2) ;
    ({irmin_tree; brassaia_tree}, res1)

  (** Exported functions *)

  let index context =
    let irmin_index =
      (Irmin_Context.index
         context.irmin_context
       [@profiler.span_f {verbosity = Notice} ["irmin"; "index"]])
    in
    let brassaia_index =
      (Brassaia_Context.index
         context.brassaia_context
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "index"]])
    in
    {irmin_index; brassaia_index}

  (** GC functions *)

  let gc : index -> Context_hash.t -> unit Lwt.t =
   fun index context_hash ->
    let* () =
      (Irmin_Context.gc
         index.irmin_index
         context_hash [@profiler.span_s {verbosity = Notice} ["irmin"; "gc"]])
    in
    (Brassaia_Context.gc
       index.brassaia_index
       context_hash [@profiler.span_f {verbosity = Notice} ["brassaia"; "gc"]])

  let wait_gc_completion : index -> unit Lwt.t =
   fun index ->
    let* () =
      (Irmin_Context.wait_gc_completion
         index.irmin_index
       [@profiler.span_s {verbosity = Notice} ["irmin"; "wait_gc_completion"]])
    in
    (Brassaia_Context.wait_gc_completion
       index.brassaia_index
     [@profiler.span_f {verbosity = Notice} ["brassaia"; "wait_gc_completion"]])

  let is_gc_allowed : index -> bool =
   fun index ->
    let bool1 =
      (Irmin_Context.is_gc_allowed
         index.irmin_index
       [@profiler.span_f {verbosity = Notice} ["irmin"; "is_gc_allowed"]])
    in
    let bool2 =
      (Brassaia_Context.is_gc_allowed
         index.brassaia_index
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "is_gc_allowed"]])
    in
    assert_and_return_result
      bool1
      bool2
      Bool.equal
      Format.pp_print_bool
      Format.pp_print_bool
      "is_gc_allowed"
      bool1

  let split : index -> unit Lwt.t =
   fun index ->
    let* () =
      (Irmin_Context.split
         index.irmin_index
       [@profiler.span_s {verbosity = Notice} ["irmin"; "split"]])
    in
    (Brassaia_Context.split
       index.brassaia_index
     [@profiler.span_f {verbosity = Notice} ["brassaia"; "split"]])

  let sync : index -> unit Lwt.t =
   fun index ->
    let* () =
      (Irmin_Context.sync
         index.irmin_index
       [@profiler.span_s {verbosity = Notice} ["irmin"; "sync"]])
    in
    (Brassaia_Context.sync
       index.brassaia_index
     [@profiler.span_f {verbosity = Notice} ["brassaia"; "sync"]])

  let exists : index -> Context_hash.t -> bool Lwt.t =
   fun index context_hash ->
    let* bool1 =
      (Irmin_Context.exists
         index.irmin_index
         context_hash
       [@profiler.span_s {verbosity = Notice} ["irmin"; "exists"]])
    in
    let* bool2 =
      (Brassaia_Context.exists
         index.brassaia_index
         context_hash
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "exists"]])
    in
    assert_and_return_result_lwt
      bool1
      bool2
      Bool.equal
      Format.pp_print_bool
      Format.pp_print_bool
      "exists"
      bool1

  let close : index -> unit Lwt.t =
   fun index ->
    let* () =
      (Irmin_Context.close
         index.irmin_index
       [@profiler.span_s {verbosity = Notice} ["irmin"; "close"]])
    in
    (Brassaia_Context.close
       index.brassaia_index
     [@profiler.span_f {verbosity = Notice} ["brassaia"; "close"]])

  let compute_testchain_chain_id : Block_hash.t -> Chain_id.t =
   fun block_hash ->
    let chain_id1 =
      (Irmin_Context.compute_testchain_chain_id
         block_hash
       [@profiler.span_f
         {verbosity = Notice} ["irmin"; "compute_testchain_chain_id"]])
    in
    let chain_id2 =
      (Brassaia_Context.compute_testchain_chain_id
         block_hash
       [@profiler.span_f
         {verbosity = Notice} ["brassaia"; "compute_testchain_chain_id"]])
    in
    assert_and_return_result
      chain_id1
      chain_id2
      Chain_id.equal
      Chain_id.pp
      Chain_id.pp
      "compute_testchain_chain_id"
      chain_id1

  (** Miscellaneous *)

  let add_predecessor_block_metadata_hash :
      t -> Block_metadata_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context =
      (Irmin_Context.add_predecessor_block_metadata_hash
         t.irmin_context
         hash
       [@profiler.span_s
         {verbosity = Notice} ["irmin"; "add_predecessor_block_metadata_hash"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add_predecessor_block_metadata_hash
         t.brassaia_context
         hash
       [@profiler.span_s
         {verbosity = Notice}
           ["brassaia"; "add_predecessor_block_metadata_hash"]])
    in
    {irmin_context; brassaia_context}

  let add_predecessor_ops_metadata_hash :
      t -> Operation_metadata_list_list_hash.t -> t Lwt.t =
   fun t hash ->
    let* irmin_context =
      (Irmin_Context.add_predecessor_ops_metadata_hash
         t.irmin_context
         hash
       [@profiler.span_s
         {verbosity = Notice} ["irmin"; "add_predecessor_ops_metadata_hash"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add_predecessor_ops_metadata_hash
         t.brassaia_context
         hash
       [@profiler.span_s
         {verbosity = Notice} ["brassaia"; "add_predecessor_ops_metadata_hash"]])
    in
    {irmin_context; brassaia_context}

  let hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t =
   fun ~time ?message t ->
    let context_hash1 =
      (Irmin_Context.hash
         ~time
         ?message
         t.irmin_context
       [@profiler.span_f {verbosity = Notice} ["irmin"; "hash"]])
    in
    let context_hash2 =
      (Brassaia_Context.hash
         ~time
         ?message
         t.brassaia_context
       [@profiler.span_f {verbosity = Notice} ["brassaia"; "hash"]])
    in
    assert_and_return_result
      context_hash1
      context_hash2
      Context_hash.equal
      Context_hash.pp
      Context_hash.pp
      "hash"
      context_hash1

  let commit_test_chain_genesis : t -> Block_header.t -> Block_header.t Lwt.t =
   fun context block_header ->
    let* block_header1 =
      (Irmin_Context.commit_test_chain_genesis
         context.irmin_context
         block_header
       [@profiler.span_s
         {verbosity = Notice} ["irmin"; "commit_test_chain_genesis"]])
    in
    let* block_header2 =
      (Brassaia_Context.commit_test_chain_genesis
         context.brassaia_context
         block_header
       [@profiler.span_s
         {verbosity = Notice} ["brassaia"; "commit_test_chain_genesis"]])
    in
    assert_and_return_result_lwt
      block_header1
      block_header2
      Block_header.equal
      Block_header.pp
      Block_header.pp
      "commit_test_chain_genesis"
      block_header1

  let get_test_chain : t -> Test_chain_status.t Lwt.t =
   fun t ->
    let* status1 =
      (Irmin_Context.get_test_chain
         t.irmin_context
       [@profiler.span_s {verbosity = Notice} ["irmin"; "get_test_chain"]])
    in
    let+ status2 =
      (Brassaia_Context.get_test_chain
         t.brassaia_context
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "get_test_chain"]])
    in
    assert (Test_chain_status.equal status1 status2) ;
    status1

  let add_test_chain : t -> Test_chain_status.t -> t Lwt.t =
   fun t status ->
    let* irmin_context =
      (Irmin_Context.add_test_chain
         t.irmin_context
         status
       [@profiler.span_s {verbosity = Notice} ["irmin"; "add_test_chain"]])
    in
    let+ brassaia_context =
      (Brassaia_Context.add_test_chain
         t.brassaia_context
         status
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "add_test_chain"]])
    in
    {irmin_context; brassaia_context}

  let commit :
      time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t =
   fun ~time ?message t ->
    let* context_hash1 =
      (Irmin_Context.commit
         ~time
         ?message
         t.irmin_context
       [@profiler.span_s {verbosity = Notice} ["irmin"; "commit"]])
    in
    let* context_hash2 =
      (Brassaia_Context.commit
         ~time
         ?message
         t.brassaia_context
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "commit"]])
    in
    assert_and_return_result_lwt
      context_hash1
      context_hash2
      Context_hash.equal
      Context_hash.pp
      Context_hash.pp
      "hash"
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
      (Irmin_Context.commit_genesis
         index.irmin_index
         ~chain_id
         ~time
         ~protocol
       [@profiler.span_s {verbosity = Notice} ["irmin"; "commit_genesis"]])
    in
    let+ context_hash2 =
      (Brassaia_Context.commit_genesis
         index.brassaia_index
         ~chain_id
         ~time
         ~protocol
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "commit_genesis"]])
    in
    assert (Context_hash.equal context_hash1 context_hash2) ;
    context_hash1

  let compute_testchain_genesis : Block_hash.t -> Block_hash.t =
   fun block_hash ->
    let block_hash1 =
      (Irmin_Context.compute_testchain_genesis
         block_hash
       [@profiler.span_f
         {verbosity = Notice} ["irmin"; "compute_testchain_genesis"]])
    in
    let block_hash2 =
      (Brassaia_Context.compute_testchain_genesis
         block_hash
       [@profiler.span_f
         {verbosity = Notice} ["brassaia"; "compute_testchain_genesis"]])
    in
    assert_and_return_result
      block_hash1
      block_hash2
      Block_hash.equal
      Block_hash.pp
      Block_hash.pp
      "hash"
      block_hash1

  let export_snapshot : index -> Context_hash.t -> path:string -> unit Lwt.t =
   fun index context_hash ~path ->
    let open Lwt_syntax in
    let* () =
      Irmin_Context.export_snapshot index.irmin_index context_hash ~path
    in
    Brassaia_Context.export_snapshot index.brassaia_index context_hash ~path

  let merkle_tree :
      t ->
      Proof.merkle_leaf_kind ->
      key ->
      Tezos_context_sigs.Context.Proof_types.merkle_tree Lwt.t =
   fun context leaf_kind path ->
    let* proof1 =
      (Irmin_Context.merkle_tree
         context.irmin_context
         leaf_kind
         path [@profiler.span_s {verbosity = Notice} ["irmin"; "merkle_tree"]])
    in
    let+ _proof2 =
      (Brassaia_Context.merkle_tree
         context.brassaia_context
         leaf_kind
         path
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "merkle_tree"]])
    in
    proof1

  let merkle_tree_v2 :
      t -> Proof.merkle_leaf_kind -> key -> Proof.tree Proof.t Lwt.t =
   fun context leaf_kind path ->
    let* proof1 =
      (Irmin_Context.merkle_tree_v2
         context.irmin_context
         leaf_kind
         path
       [@profiler.span_s {verbosity = Notice} ["irmin"; "merkle_tree_v2"]])
    in
    let+ _proof2 =
      (Brassaia_Context.merkle_tree_v2
         context.brassaia_context
         leaf_kind
         path
       [@profiler.span_s {verbosity = Notice} ["brassaia"; "merkle_tree_v2"]])
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
