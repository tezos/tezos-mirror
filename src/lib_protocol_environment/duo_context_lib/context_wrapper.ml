(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Profiler =
  Tezos_protocol_environment.Environment_profiler.Context_ops_profiler

let pp_print_option pp ppf = function
  | Some v -> Format.fprintf ppf "Some %a" pp v
  | None -> Format.fprintf ppf "None"

module Events = struct
  include Internal_event.Simple

  let section = ["node"; "duo_context"]

  let assertion_failure =
    declare_5
      ~section
      ~level:Warning
      ~name:"assertion_failure"
      ~msg:
        "{function} returned {res1} for {context_1} and {res2} for {context_2}"
      ("res1", Data_encoding.string)
      ("res2", Data_encoding.string)
      ("function", Data_encoding.string)
      ("context_1", Data_encoding.string)
      ("context_2", Data_encoding.string)
end

let backend_mismatch = Fmt.failwith "Duo_context.%s: backend mismatch"

open Duo_context_sig

module Utils = struct
  open Internal

  let irmin_disk_context t = Irmin_disk_context t

  let irmin_mem_context t = Irmin_mem_context t

  let brassaia_disk_context t = Brassaia_disk_context t

  let brassaia_mem_context t = Brassaia_mem_context t

  let tezedge_context t = Tezedge_context t

  let irmin_disk_context_o = Option.map irmin_disk_context

  let irmin_mem_context_o = Option.map irmin_mem_context

  let brassaia_disk_context_o = Option.map brassaia_disk_context

  let brassaia_mem_context_o = Option.map brassaia_mem_context

  let tezedge_context_o = Option.map tezedge_context

  let irmin_disk_context_r = Result.map irmin_disk_context

  let irmin_mem_context_r = Result.map irmin_mem_context

  let brassaia_disk_context_r = Result.map brassaia_disk_context

  let brassaia_mem_context_r = Result.map brassaia_mem_context

  let tezedge_context_r = Result.map tezedge_context

  let irmin_disk_tree t = Irmin_disk_tree t

  let irmin_mem_tree t = Irmin_mem_tree t

  let brassaia_disk_tree t = Brassaia_disk_tree t

  let brassaia_mem_tree t = Brassaia_mem_tree t

  let tezedge_tree t = Tezedge_tree t

  let irmin_disk_tree_o = Option.map irmin_disk_tree

  let irmin_mem_tree_o = Option.map irmin_mem_tree

  let brassaia_disk_tree_o = Option.map brassaia_disk_tree

  let brassaia_mem_tree_o = Option.map brassaia_mem_tree

  let tezedge_tree_o = Option.map tezedge_tree

  let backend_name : wrapped_backend -> string = function
    | Irmin_disk -> "Irmin_disk"
    | Irmin_mem -> "Irmin_mem"
    | Brassaia_disk -> "Brassaia_disk"
    | Brassaia_mem -> "Brassaia_mem"
    | Tezedge -> "Tezedge"
end

open Utils
open Lwt_syntax

module MakeContext (P : Duo_context_sig.CONTEXT_PARAM) : sig
  include Duo_context_sig.CONTEXT

  type index_1 = P.index_1

  type index_2 = P.index_2

  val make_index : index_1 -> index_2 -> index
end = struct
  include Internal

  type index_1 = P.index_1

  type index_2 = P.index_2

  let make_index = P.make_index

  let backend_1 = P.backend_1

  let backend_2 = P.backend_2

  let backend_1_name = backend_name backend_1

  let backend_2_name = backend_name backend_2

  let name = Printf.sprintf "duo(%s, %s)" backend_1_name backend_2_name

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
              function_name,
              backend_1_name,
              backend_2_name ))
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
            function_name,
            backend_1_name,
            backend_2_name )) ;
      final_res)

  let option_equal_no_traversal o1 o2 =
    match (o1, o2) with Some _, Some _ | None, None -> true | _ -> false

  let pp_option_no_traversal ppf = function
    | Some _ -> Format.fprintf ppf "Some _"
    | None -> Format.fprintf ppf "None"

  module Tree = struct
    let mem : tree -> key -> bool Lwt.t =
     fun t key ->
      let mem = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.mem t key
        | Irmin_mem_tree t -> Irmin_mem.Tree.mem t key
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.mem t key
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.mem t key
        | Tezedge_tree t -> Tezedge.Tree.mem t key
      in
      let mem _name t =
        (mem t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "mem"]])
      in
      let* bool1 = mem backend_1_name t.tree_1 in
      let* bool2 = mem backend_2_name t.tree_2 in
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
      let mem_tree = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.mem_tree t key
        | Irmin_mem_tree t -> Irmin_mem.Tree.mem_tree t key
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.mem_tree t key
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.mem_tree t key
        | Tezedge_tree t -> Tezedge.Tree.mem_tree t key
      in
      let mem_tree _name t =
        (mem_tree
           t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "mem_tree"]])
      in
      let* bool1 = mem_tree backend_1_name t.tree_1 in
      let* bool2 = mem_tree backend_2_name t.tree_2 in
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
      let find = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.find t key
        | Irmin_mem_tree t -> Irmin_mem.Tree.find t key
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.find t key
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.find t key
        | Tezedge_tree t -> Tezedge.Tree.find t key
      in
      let find _name t =
        (find t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "find"]])
      in
      let* value1 = find backend_1_name t.tree_1 in
      let* value2 = find backend_2_name t.tree_2 in
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
      let find_tree = function
        | Irmin_disk_tree t ->
            Lwt.map irmin_disk_tree_o (Irmin_disk.Tree.find_tree t key)
        | Irmin_mem_tree t ->
            Lwt.map irmin_mem_tree_o (Irmin_mem.Tree.find_tree t key)
        | Brassaia_disk_tree t ->
            Lwt.map brassaia_disk_tree_o (Brassaia_disk.Tree.find_tree t key)
        | Brassaia_mem_tree t ->
            Lwt.map brassaia_mem_tree_o (Brassaia_mem.Tree.find_tree t key)
        | Tezedge_tree t ->
            Lwt.map tezedge_tree_o (Tezedge.Tree.find_tree t key)
      in
      let find_tree _name t =
        (find_tree
           t
         [@profiler.span_s {verbosity = Notice} [_name; "tree"; "find_tree"]])
      in
      let* tree_1 = find_tree backend_1_name t.tree_1 in
      let* tree_2 = find_tree backend_2_name t.tree_2 in
      let final_res =
        match (tree_1, tree_2) with
        | Some tree_1, Some tree_2 -> Some {tree_1; tree_2}
        | _ -> None
      in
      assert_and_return_result_lwt
        tree_1
        tree_2
        option_equal_no_traversal
        pp_option_no_traversal
        pp_option_no_traversal
        "Tree.find_tree"
        final_res

    let list :
        tree -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t
        =
     fun t ?offset ?length key ->
      let wrap wrapper =
        Lwt.map (List.map (fun (str, t) -> (str, wrapper t)))
      in
      let list = function
        | Irmin_mem_tree t ->
            wrap irmin_mem_tree (Irmin_mem.Tree.list t ?offset ?length key)
        | Irmin_disk_tree t ->
            wrap irmin_disk_tree (Irmin_disk.Tree.list t ?offset ?length key)
        | Brassaia_disk_tree t ->
            wrap
              brassaia_disk_tree
              (Brassaia_disk.Tree.list t ?offset ?length key)
        | Brassaia_mem_tree t ->
            wrap
              brassaia_mem_tree
              (Brassaia_mem.Tree.list t ?offset ?length key)
        | Tezedge_tree t ->
            wrap tezedge_tree (Tezedge.Tree.list t ?offset ?length key)
      in
      let list _name t =
        (list t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "list"]])
      in
      let* a_list = list backend_1_name t.tree_1 in
      let* b_list = list backend_2_name t.tree_2 in
      let final_res =
        List.map
          (fun (key, tree_1) ->
            let tree_2 =
              List.find (fun (key2, _) -> String.equal key key2) b_list
              |> Option.value
                   ~default:(Fmt.failwith "No value associated to %s" key)
              |> snd
            in
            (key, {tree_1; tree_2}))
          a_list
      in
      let pp_list ppf l =
        Format.pp_print_list (fun ppf (s, _) -> Format.fprintf ppf "%s" s) ppf l
      in
      assert_and_return_result_lwt
        a_list
        b_list
        (fun l1 l2 -> List.compare_lengths l1 l2 = 0)
        pp_list
        pp_list
        "Tree.list"
        final_res

    let length : tree -> key -> int Lwt.t =
     fun t key ->
      let length = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.length t key
        | Irmin_mem_tree t -> Irmin_mem.Tree.length t key
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.length t key
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.length t key
        | Tezedge_tree t -> Tezedge.Tree.length t key
      in
      let length _name t =
        (length
           t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "length"]])
      in
      let* length1 = length backend_1_name t.tree_1 in
      let* length2 = length backend_2_name t.tree_2 in
      assert_and_return_result_lwt
        length1
        length2
        Int.equal
        Format.pp_print_int
        Format.pp_print_int
        "Tree.length"
        length1

    let add : tree -> key -> value -> tree Lwt.t =
     fun t k v ->
      let add = function
        | Irmin_disk_tree t ->
            Lwt.map irmin_disk_tree (Irmin_disk.Tree.add t k v)
        | Irmin_mem_tree t -> Lwt.map irmin_mem_tree (Irmin_mem.Tree.add t k v)
        | Brassaia_disk_tree t ->
            Lwt.map brassaia_disk_tree (Brassaia_disk.Tree.add t k v)
        | Brassaia_mem_tree t ->
            Lwt.map brassaia_mem_tree (Brassaia_mem.Tree.add t k v)
        | Tezedge_tree t -> Lwt.map tezedge_tree (Tezedge.Tree.add t k v)
      in
      let add _name t =
        (add t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "add"]])
      in
      let* tree_1 = add backend_1_name t.tree_1 in
      let* tree_2 = add backend_2_name t.tree_2 in
      Lwt.return {tree_1; tree_2}

    let add_tree : tree -> key -> tree -> tree Lwt.t =
     fun t key t' ->
      let add_tree t t' =
        match (t, t') with
        | Irmin_disk_tree t, Irmin_disk_tree t' ->
            Lwt.map irmin_disk_tree (Irmin_disk.Tree.add_tree t key t')
        | Irmin_mem_tree t, Irmin_mem_tree t' ->
            Lwt.map irmin_mem_tree (Irmin_mem.Tree.add_tree t key t')
        | Brassaia_disk_tree t, Brassaia_disk_tree t' ->
            Lwt.map brassaia_disk_tree (Brassaia_disk.Tree.add_tree t key t')
        | Brassaia_mem_tree t, Brassaia_mem_tree t' ->
            Lwt.map brassaia_mem_tree (Brassaia_mem.Tree.add_tree t key t')
        | Tezedge_tree t, Tezedge_tree t' ->
            Lwt.map tezedge_tree (Tezedge.Tree.add_tree t key t')
        | ( ( Irmin_disk_tree _ | Irmin_mem_tree _ | Brassaia_disk_tree _
            | Brassaia_mem_tree _ | Tezedge_tree _ ),
            _ ) ->
            backend_mismatch "Tree.add_tree"
      in
      let add_tree _name t t' =
        (add_tree
           t
           t'
         [@profiler.span_s {verbosity = Notice} [_name; "tree"; "add_tree"]])
      in
      let* tree_1 = add_tree backend_1_name t.tree_1 t'.tree_1 in
      let* tree_2 = add_tree backend_2_name t.tree_2 t'.tree_2 in
      Lwt.return {tree_1; tree_2}

    let remove : tree -> key -> tree Lwt.t =
     fun t k ->
      let remove = function
        | Irmin_disk_tree t ->
            Lwt.map irmin_disk_tree (Irmin_disk.Tree.remove t k)
        | Irmin_mem_tree t -> Lwt.map irmin_mem_tree (Irmin_mem.Tree.remove t k)
        | Brassaia_disk_tree t ->
            Lwt.map brassaia_disk_tree (Brassaia_disk.Tree.remove t k)
        | Brassaia_mem_tree t ->
            Lwt.map brassaia_mem_tree (Brassaia_mem.Tree.remove t k)
        | Tezedge_tree t -> Lwt.map tezedge_tree (Tezedge.Tree.remove t k)
      in
      let remove _name t =
        (remove
           t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "remove"]])
      in
      let* tree_1 = remove backend_1_name t.tree_1 in
      let* tree_2 = remove backend_2_name t.tree_2 in
      Lwt.return {tree_1; tree_2}

    (** [fold] MUST NOT be used with Duo_context when returning a new [tree]  *)
    let fold :
        ?depth:Tezos_context_sigs.Context.depth ->
        tree ->
        key ->
        order:[`Sorted | `Undefined] ->
        init:'a ->
        f:(key -> tree -> 'a -> 'a Lwt.t) ->
        'a Lwt.t =
     fun ?depth t key ~order ~init ~f ->
      let fold folder wrapper t =
        let f key tree acc =
          f key {tree_1 = wrapper tree; tree_2 = wrapper tree} acc
        in
        folder ?depth t key ~order ~init ~f
      in
      let fold = function
        | Irmin_disk_tree t -> fold Irmin_disk.Tree.fold irmin_disk_tree t
        | Irmin_mem_tree t -> fold Irmin_mem.Tree.fold irmin_mem_tree t
        | Brassaia_disk_tree t ->
            fold Brassaia_disk.Tree.fold brassaia_disk_tree t
        | Brassaia_mem_tree t -> fold Brassaia_mem.Tree.fold brassaia_mem_tree t
        | Tezedge_tree t -> fold Tezedge.Tree.fold tezedge_tree t
      in
      let fold _name t =
        (fold t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "fold"]])
      in
      let* res1 = fold backend_1_name t.tree_1 in
      let* _res2 = fold backend_2_name t.tree_2 in
      (* This assertion fails if [f] returns a [tree] since we would end with
         [res1] as [{ Backend1.tree; Backend1.tree }] and [res2] as [{
         Backend2.tree; Backend2.tree }]. This is a good thing as it would
         corrupt the rest of the executions in context. If returning a new tree
         is needed, we need to provide a new fold function with [f]'s return
         type specialized so that we can destruct it and build the correct value
         to be returned. *)
      (* assert (res1 = res2) ; *)
      Lwt.return res1

    let config : tree -> Tezos_context_sigs.Config.t =
     fun t ->
      let config = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.config t
        | Irmin_mem_tree t -> Irmin_mem.Tree.config t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.config t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.config t
        | Tezedge_tree t -> Tezedge.Tree.config t
      in
      let config _name t =
        (config
           t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "config"]])
      in
      let config1 = config backend_1_name t.tree_1 in
      let config2 = config backend_2_name t.tree_2 in
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
      let empty = function
        | Irmin_disk_context t -> Irmin_disk_tree (Irmin_disk.Tree.empty t)
        | Irmin_mem_context t -> Irmin_mem_tree (Irmin_mem.Tree.empty t)
        | Brassaia_disk_context t ->
            Brassaia_disk_tree (Brassaia_disk.Tree.empty t)
        | Brassaia_mem_context t ->
            Brassaia_mem_tree (Brassaia_mem.Tree.empty t)
        | Tezedge_context t -> Tezedge_tree (Tezedge.Tree.empty t)
      in
      let empty _name ctxt =
        (empty
           ctxt [@profiler.span_f {verbosity = Notice} [_name; "tree"; "empty"]])
      in
      let tree_1 = empty backend_1_name t.context_1 in
      let tree_2 = empty backend_2_name t.context_2 in
      {tree_1; tree_2}

    let is_empty : tree -> bool =
     fun t ->
      let is_empty = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.is_empty t
        | Irmin_mem_tree t -> Irmin_mem.Tree.is_empty t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.is_empty t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.is_empty t
        | Tezedge_tree t -> Tezedge.Tree.is_empty t
      in
      let is_empty _name ctxt =
        (is_empty
           ctxt
         [@profiler.span_f {verbosity = Notice} [_name; "tree"; "is_empty"]])
      in
      let bool1 = is_empty backend_1_name t.tree_1 in
      let bool2 = is_empty backend_2_name t.tree_2 in
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
      let kind = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.kind t
        | Irmin_mem_tree t -> Irmin_mem.Tree.kind t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.kind t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.kind t
        | Tezedge_tree t -> Tezedge.Tree.kind t
      in
      let kind _name t =
        (kind t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "kind"]])
      in
      let kind1 = kind backend_1_name t.tree_1 in
      let kind2 = kind backend_2_name t.tree_2 in
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
      let to_value = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.to_value t
        | Irmin_mem_tree t -> Irmin_mem.Tree.to_value t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.to_value t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.to_value t
        | Tezedge_tree t -> Tezedge.Tree.to_value t
      in
      let to_value _name t =
        (to_value
           t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "to_value"]])
      in
      let* value1 = to_value backend_1_name t.tree_1 in
      let* value2 = to_value backend_2_name t.tree_2 in
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
      let of_value = function
        | Irmin_disk_context t ->
            Lwt.map irmin_disk_tree (Irmin_disk.Tree.of_value t value)
        | Irmin_mem_context t ->
            Lwt.map irmin_mem_tree (Irmin_mem.Tree.of_value t value)
        | Brassaia_disk_context t ->
            Lwt.map brassaia_disk_tree (Brassaia_disk.Tree.of_value t value)
        | Brassaia_mem_context t ->
            Lwt.map brassaia_mem_tree (Brassaia_mem.Tree.of_value t value)
        | Tezedge_context t ->
            Lwt.map tezedge_tree (Tezedge.Tree.of_value t value)
      in
      let of_value _name t =
        (of_value
           t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "of_value"]])
      in
      let* tree_1 = of_value backend_1_name t.context_1 in
      let* tree_2 = of_value backend_2_name t.context_2 in
      Lwt.return {tree_1; tree_2}

    let hash : tree -> Context_hash.t =
     fun t ->
      let hash t =
        match t with
        | Irmin_disk_tree t -> Irmin_disk.Tree.hash t
        | Irmin_mem_tree t -> Irmin_mem.Tree.hash t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.hash t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.hash t
        | Tezedge_tree t -> Tezedge.Tree.hash t
      in
      let hash _name t =
        (hash t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "hash"]])
      in
      let context_hash1 = hash backend_1_name t.tree_1 in
      let context_hash2 = hash backend_2_name t.tree_2 in
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
      let equal t1 t2 =
        match (t1, t2) with
        | Irmin_disk_tree t1, Irmin_disk_tree t2 -> Irmin_disk.Tree.equal t1 t2
        | Irmin_mem_tree t1, Irmin_mem_tree t2 -> Irmin_mem.Tree.equal t1 t2
        | Brassaia_disk_tree t1, Brassaia_disk_tree t2 ->
            Brassaia_disk.Tree.equal t1 t2
        | Brassaia_mem_tree t1, Brassaia_mem_tree t2 ->
            Brassaia_mem.Tree.equal t1 t2
        | Tezedge_tree t1, Tezedge_tree t2 -> Tezedge.Tree.equal t1 t2
        | ( ( Irmin_disk_tree _ | Irmin_mem_tree _ | Brassaia_disk_tree _
            | Brassaia_mem_tree _ | Tezedge_tree _ ),
            _ ) ->
            backend_mismatch "Tree.equal"
      in
      let equal _name t1 t2 =
        (equal
           t1
           t2 [@profiler.span_f {verbosity = Notice} [_name; "tree"; "equal"]])
      in
      let bool1 = equal backend_1_name t1.tree_1 t2.tree_1 in
      let bool2 = equal backend_2_name t1.tree_2 t2.tree_2 in
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
      let clear = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.clear ?depth t
        | Irmin_mem_tree t -> Irmin_mem.Tree.clear ?depth t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.clear ?depth t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.clear ?depth t
        | Tezedge_tree t -> Tezedge.Tree.clear ?depth t
      in
      let clear _name t =
        (clear
           t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "clear"]])
      in
      clear backend_1_name t.tree_1 ;
      clear backend_2_name t.tree_2

    let pp : Format.formatter -> tree -> unit =
     fun ppf t ->
      let pp ppf = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.pp ppf t
        | Irmin_mem_tree t -> Irmin_mem.Tree.pp ppf t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.pp ppf t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.pp ppf t
        | Tezedge_tree t -> Tezedge.Tree.pp ppf t
      in
      let pp _name ppf t =
        (pp ppf t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "pp"]])
      in
      pp backend_1_name ppf t.tree_1 ;
      pp backend_2_name ppf t.tree_2

    type raw = [`Value of bytes | `Tree of raw String.Map.t]

    let raw_encoding = Tezos_context_helpers.Context.raw_encoding

    let to_raw : tree -> raw Lwt.t =
     fun t ->
      let to_raw = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.to_raw t
        | Irmin_mem_tree t -> Irmin_mem.Tree.to_raw t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.to_raw t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.to_raw t
        | Tezedge_tree _t -> assert false
      in
      let to_raw _name t =
        (to_raw
           t [@profiler.span_f {verbosity = Notice} [_name; "tree"; "to_raw"]])
      in
      let* raw1 = to_raw backend_1_name t.tree_1 in
      let* raw2 = to_raw backend_2_name t.tree_2 in
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
      Lwt.return
        (assert_and_return_result raw1 raw2 equal pp pp "Tree.to_raw" raw1)

    let of_raw : raw -> tree =
     fun raw ->
      let of_raw = function
        | Irmin_disk -> Irmin_disk_tree (Irmin_disk.Tree.of_raw raw)
        | Irmin_mem -> Irmin_mem_tree (Irmin_mem.Tree.of_raw raw)
        | Brassaia_disk -> Brassaia_disk_tree (Brassaia_disk.Tree.of_raw raw)
        | Brassaia_mem -> Brassaia_mem_tree (Brassaia_mem.Tree.of_raw raw)
        | Tezedge -> assert false
      in
      let of_raw _name b =
        (of_raw
           b [@profiler.span_f {verbosity = Notice} [_name; "tree"; "of_raw"]])
      in
      let tree_1 = of_raw backend_1_name backend_1 in
      let tree_2 = of_raw backend_2_name backend_2 in
      {tree_1; tree_2}

    let unshallow : tree -> tree Lwt.t =
     fun t ->
      let unshallow = function
        | Irmin_disk_tree t ->
            Lwt.map irmin_disk_tree (Irmin_disk.Tree.unshallow t)
        | Irmin_mem_tree t ->
            Lwt.map irmin_mem_tree (Irmin_mem.Tree.unshallow t)
        | Brassaia_disk_tree t ->
            Lwt.map brassaia_disk_tree (Brassaia_disk.Tree.unshallow t)
        | Brassaia_mem_tree t ->
            Lwt.map brassaia_mem_tree (Brassaia_mem.Tree.unshallow t)
        | Tezedge_tree _t -> assert false
      in
      let unshallow _name t =
        (unshallow
           t
         [@profiler.span_f {verbosity = Notice} [_name; "tree"; "unshallow"]])
      in
      let* tree_1 = unshallow backend_1_name t.tree_1 in
      let* tree_2 = unshallow backend_2_name t.tree_2 in
      Lwt.return {tree_1; tree_2}

    let is_shallow : tree -> bool =
     fun t ->
      let is_shallow = function
        | Irmin_disk_tree t -> Irmin_disk.Tree.is_shallow t
        | Irmin_mem_tree t -> Irmin_mem.Tree.is_shallow t
        | Brassaia_disk_tree t -> Brassaia_disk.Tree.is_shallow t
        | Brassaia_mem_tree t -> Brassaia_mem.Tree.is_shallow t
        | Tezedge_tree _t -> assert false
      in
      let is_shallow _name t =
        (is_shallow
           t
         [@profiler.span_f {verbosity = Notice} [_name; "tree"; "is_shallow"]])
      in
      let bool1 = is_shallow backend_1_name t.tree_1 in
      let bool2 = is_shallow backend_2_name t.tree_2 in
      assert_and_return_result
        bool1
        bool2
        Bool.equal
        Format.pp_print_bool
        Format.pp_print_bool
        "Tree.is_shallow"
        bool1

    let kinded_key : tree -> kinded_key option =
     fun t ->
      let kinded_key = function
        | Irmin_disk_tree t ->
            Option.map (fun k -> Irmin_disk_kk k) (Irmin_disk.Tree.kinded_key t)
        | Irmin_mem_tree t ->
            Option.map (fun k -> Irmin_mem_kk k) (Irmin_mem.Tree.kinded_key t)
        | Brassaia_disk_tree t ->
            Option.map
              (fun k -> Brassaia_disk_kk k)
              (Brassaia_disk.Tree.kinded_key t)
        | Brassaia_mem_tree t ->
            Option.map
              (fun k -> Brassaia_mem_kk k)
              (Brassaia_mem.Tree.kinded_key t)
        | Tezedge_tree _t -> assert false
      in
      let kinded_key _name t =
        (kinded_key
           t
         [@profiler.span_f {verbosity = Notice} [_name; "tree"; "kinded_key"]])
      in
      let kinded_key1 = kinded_key backend_1_name t.tree_1 in
      let kinded_key2 = kinded_key backend_2_name t.tree_2 in
      let node_key = function
        | Irmin_disk_kk (`Node k) -> Some (Irmin_disk_nk k)
        | Irmin_mem_kk (`Node k) -> Some (Irmin_mem_nk k)
        | Brassaia_disk_kk (`Node k) -> Some (Brassaia_disk_nk k)
        | Brassaia_mem_kk (`Node k) -> Some (Brassaia_mem_nk k)
        | Irmin_disk_kk (`Value _)
        | Irmin_mem_kk (`Value _)
        | Brassaia_disk_kk (`Value _)
        | Brassaia_mem_kk (`Value _) ->
            None
      in
      let value_key = function
        | Irmin_disk_kk (`Value k) -> Some (Irmin_disk_vk k)
        | Irmin_mem_kk (`Value k) -> Some (Irmin_mem_vk k)
        | Brassaia_disk_kk (`Value k) -> Some (Brassaia_disk_vk k)
        | Brassaia_mem_kk (`Value k) -> Some (Brassaia_mem_vk k)
        | Irmin_disk_kk (`Node _)
        | Irmin_mem_kk (`Node _)
        | Brassaia_disk_kk (`Node _)
        | Brassaia_mem_kk (`Node _) ->
            None
      in
      let failwith_availability () =
        Fmt.failwith "Duo_context.Tree.kinded_key: data availability mismatch"
      in
      let failwith_kind () =
        Fmt.failwith "Duo_context.Tree.kinded_key: kind mismatch"
      in
      match kinded_key1 with
      | None ->
          if kinded_key2 <> None then failwith_availability () ;
          None
      | Some kinded_key1 -> (
          match kinded_key2 with
          | None -> failwith_availability ()
          | Some kinded_key2 -> (
              match value_key kinded_key1 with
              | Some value_key_1 -> (
                  match value_key kinded_key2 with
                  | None -> failwith_kind ()
                  | Some value_key_2 -> Some (`Value {value_key_1; value_key_2})
                  )
              | None -> (
                  match node_key kinded_key1 with
                  | None -> failwith_kind ()
                  | Some node_key_1 -> (
                      match node_key kinded_key2 with
                      | None -> failwith_kind ()
                      | Some node_key_2 -> Some (`Node {node_key_1; node_key_2})
                      ))))
  end

  module Proof = Tezos_context_sigs.Context.Proof_types

  let add_protocol : t -> Protocol_hash.t -> t Lwt.t =
   fun t hash ->
    let add_protocol = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_context (Irmin_disk.add_protocol t hash)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_context (Irmin_mem.add_protocol t hash)
      | Brassaia_disk_context t ->
          Lwt.map brassaia_disk_context (Brassaia_disk.add_protocol t hash)
      | Brassaia_mem_context t ->
          Lwt.map brassaia_mem_context (Brassaia_mem.add_protocol t hash)
      | Tezedge_context t ->
          Lwt.map tezedge_context (Tezedge.add_protocol t hash)
    in
    let add_protocol _name t =
      (add_protocol
         t [@profiler.span_s {verbosity = Notice} [_name; "add_protocol"]])
    in
    let* context_1 = add_protocol backend_1_name t.context_1 in
    let* context_2 = add_protocol backend_2_name t.context_2 in
    Lwt.return {context_1; context_2}

  let equal_config config1 config2 =
    let equal_config = function
      | Irmin_disk -> Irmin_disk.equal_config
      | Irmin_mem -> Irmin_mem.equal_config
      | Brassaia_disk -> Brassaia_disk.equal_config
      | Brassaia_mem -> Brassaia_mem.equal_config
      | Tezedge -> Tezedge.equal_config
    in
    let equal_config _name backend config1 config2 =
      (equal_config
         backend
         config1
         config2 [@profiler.span_f {verbosity = Notice} [_name; "equal_config"]])
    in
    let bool1 = equal_config backend_1_name backend_1 config1 config2 in
    let bool2 = equal_config backend_2_name backend_2 config1 config2 in
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
    let mem = function
      | Irmin_disk_context t -> Irmin_disk.mem t key
      | Irmin_mem_context t -> Irmin_mem.mem t key
      | Brassaia_disk_context t -> Brassaia_disk.mem t key
      | Brassaia_mem_context t -> Brassaia_mem.mem t key
      | Tezedge_context t -> Tezedge.mem t key
    in
    let mem _name t =
      (mem t [@profiler.span_s {verbosity = Notice} [_name; "mem"]])
    in
    let* bool1 = mem backend_1_name t.context_1 in
    let* bool2 = mem backend_2_name t.context_2 in
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
    let mem_tree = function
      | Irmin_disk_context t -> Irmin_disk.mem_tree t key
      | Irmin_mem_context t -> Irmin_mem.mem_tree t key
      | Brassaia_disk_context t -> Brassaia_disk.mem_tree t key
      | Brassaia_mem_context t -> Brassaia_mem.mem_tree t key
      | Tezedge_context t -> Tezedge.mem_tree t key
    in
    let mem_tree _name t =
      (mem_tree t [@profiler.span_s {verbosity = Notice} [_name; "mem_tree"]])
    in
    let* bool1 = mem_tree backend_1_name t.context_1 in
    let* bool2 = mem_tree backend_2_name t.context_2 in
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
    let find = function
      | Irmin_disk_context t -> Irmin_disk.find t key
      | Irmin_mem_context t -> Irmin_mem.find t key
      | Brassaia_disk_context t -> Brassaia_disk.find t key
      | Brassaia_mem_context t -> Brassaia_mem.find t key
      | Tezedge_context t -> Tezedge.find t key
    in
    let find _name t =
      (find t [@profiler.span_s {verbosity = Notice} [_name; "find"]])
    in
    let* value1 = find backend_1_name t.context_1 in
    let* value2 = find backend_2_name t.context_2 in
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
    let find_tree = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_tree_o (Irmin_disk.find_tree t key)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_tree_o (Irmin_mem.find_tree t key)
      | Brassaia_disk_context t ->
          Lwt.map brassaia_disk_tree_o (Brassaia_disk.find_tree t key)
      | Brassaia_mem_context t ->
          Lwt.map brassaia_mem_tree_o (Brassaia_mem.find_tree t key)
      | Tezedge_context t -> Lwt.map tezedge_tree_o (Tezedge.find_tree t key)
    in
    let find_tree _name t =
      (find_tree
         t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "find_tree"]])
    in
    let* tree_1 = find_tree backend_1_name t.context_1 in
    let* tree_2 = find_tree backend_2_name t.context_2 in
    let final_res =
      match (tree_1, tree_2) with
      | Some tree_1, Some tree_2 -> Some {tree_1; tree_2}
      | _ -> None
    in
    assert_and_return_result_lwt
      tree_1
      tree_2
      option_equal_no_traversal
      pp_option_no_traversal
      pp_option_no_traversal
      "find_tree"
      final_res

  let list :
      t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t =
   fun t ?offset ?length key ->
    let wrap wrapper = Lwt.map (List.map (fun (str, t) -> (str, wrapper t))) in
    let list = function
      | Irmin_disk_context t ->
          wrap irmin_disk_tree (Irmin_disk.list t ?offset ?length key)
      | Irmin_mem_context t ->
          wrap irmin_mem_tree (Irmin_mem.list t ?offset ?length key)
      | Brassaia_disk_context t ->
          wrap brassaia_disk_tree (Brassaia_disk.list t ?offset ?length key)
      | Brassaia_mem_context t ->
          wrap brassaia_mem_tree (Brassaia_mem.list t ?offset ?length key)
      | Tezedge_context t ->
          wrap tezedge_tree (Tezedge.list t ?offset ?length key)
    in
    let list _name t =
      (list t [@profiler.span_s {verbosity = Notice} [_name; "list"]])
    in
    let* a_list = list backend_1_name t.context_1 in
    let* b_list = list backend_2_name t.context_2 in
    let final_res =
      List.map
        (fun (key, tree_1) ->
          let tree_2 =
            List.find (fun (key2, _) -> String.equal key key2) b_list
            |> Option.value
                 ~default:(Fmt.failwith "No value associated to %s" key)
            |> snd
          in
          (key, {tree_1; tree_2}))
        a_list
    in
    let pp_list ppf l =
      Format.pp_print_list (fun ppf (s, _) -> Format.fprintf ppf "%s" s) ppf l
    in
    assert_and_return_result_lwt
      a_list
      b_list
      (fun l1 l2 -> List.compare_lengths l1 l2 = 0)
      pp_list
      pp_list
      "list"
      final_res

  let length : t -> key -> int Lwt.t =
   fun t key ->
    let length = function
      | Irmin_disk_context t -> Irmin_disk.length t key
      | Irmin_mem_context t -> Irmin_mem.length t key
      | Brassaia_disk_context t -> Brassaia_disk.length t key
      | Brassaia_mem_context t -> Brassaia_mem.length t key
      | Tezedge_context t -> Tezedge.length t key
    in
    let length _name t =
      (length t [@profiler.span_s {verbosity = Notice} [_name; "length"]])
    in
    let* length1 = length backend_1_name t.context_1 in
    let* length2 = length backend_2_name t.context_2 in
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
    let add = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_context (Irmin_disk.add t key value)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_context (Irmin_mem.add t key value)
      | Brassaia_disk_context t ->
          Lwt.map brassaia_disk_context (Brassaia_disk.add t key value)
      | Brassaia_mem_context t ->
          Lwt.map brassaia_mem_context (Brassaia_mem.add t key value)
      | Tezedge_context t -> Lwt.map tezedge_context (Tezedge.add t key value)
    in
    let add _name t =
      (add t [@profiler.span_s {verbosity = Notice} [_name; "tree"; "add"]])
    in
    let* context_1 = add backend_1_name t.context_1 in
    let* context_2 = add backend_2_name t.context_2 in
    Lwt.return {context_1; context_2}

  let add_tree : t -> key -> tree -> t Lwt.t =
   fun t key tree ->
    let add_tree t tree =
      match (t, tree) with
      | Irmin_disk_context t, Irmin_disk_tree tree ->
          Lwt.map irmin_disk_context (Irmin_disk.add_tree t key tree)
      | Irmin_mem_context t, Irmin_mem_tree tree ->
          Lwt.map irmin_mem_context (Irmin_mem.add_tree t key tree)
      | Brassaia_disk_context t, Brassaia_disk_tree tree ->
          Lwt.map brassaia_disk_context (Brassaia_disk.add_tree t key tree)
      | Brassaia_mem_context t, Brassaia_mem_tree tree ->
          Lwt.map brassaia_mem_context (Brassaia_mem.add_tree t key tree)
      | Tezedge_context t, Tezedge_tree tree ->
          Lwt.map tezedge_context (Tezedge.add_tree t key tree)
      | ( ( Irmin_disk_context _ | Irmin_mem_context _ | Brassaia_disk_context _
          | Brassaia_mem_context _ | Tezedge_context _ ),
          _ ) ->
          backend_mismatch "add_tree"
    in
    let add_tree _name t tree =
      (add_tree
         t
         tree
       [@profiler.span_s {verbosity = Notice} [_name; "tree"; "add_tree"]])
    in
    let* context_1 = add_tree backend_1_name t.context_1 tree.tree_1 in
    let* context_2 = add_tree backend_2_name t.context_2 tree.tree_2 in
    Lwt.return {context_1; context_2}

  let remove : t -> key -> t Lwt.t =
   fun t key ->
    let remove = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_context (Irmin_disk.remove t key)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_context (Irmin_mem.remove t key)
      | Brassaia_disk_context t ->
          Lwt.map brassaia_disk_context (Brassaia_disk.remove t key)
      | Brassaia_mem_context t ->
          Lwt.map brassaia_mem_context (Brassaia_mem.remove t key)
      | Tezedge_context t -> Lwt.map tezedge_context (Tezedge.remove t key)
    in
    let remove _name t =
      (remove t [@profiler.span_s {verbosity = Notice} [_name; "remove"]])
    in
    let* context_1 = remove backend_1_name t.context_1 in
    let* context_2 = remove backend_2_name t.context_2 in
    Lwt.return {context_1; context_2}

  (** [fold] MUST NOT be used with Duo_context when returning a new [t]  *)
  let fold :
      ?depth:Tezos_context_sigs.Context.depth ->
      t ->
      key ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(key -> tree -> 'a -> 'a Lwt.t) ->
      'a Lwt.t =
   fun ?depth t key ~order ~init ~f ->
    let fold folder wrapper t =
      let f key tree acc =
        f key {tree_1 = wrapper tree; tree_2 = wrapper tree} acc
      in
      folder ?depth t key ~order ~init ~f
    in
    let fold = function
      | Irmin_disk_context t -> fold Irmin_disk.fold irmin_disk_tree t
      | Irmin_mem_context t -> fold Irmin_mem.fold irmin_mem_tree t
      | Brassaia_disk_context t -> fold Brassaia_disk.fold brassaia_disk_tree t
      | Brassaia_mem_context t -> fold Brassaia_mem.fold brassaia_mem_tree t
      | Tezedge_context t -> fold Tezedge.fold tezedge_tree t
    in
    let fold _name t =
      (fold t [@profiler.span_f {verbosity = Notice} [_name; "fold"]])
    in
    let res1 = fold backend_1_name t.context_1 in
    let _res2 = fold backend_2_name t.context_2 in
    (* This assertion fails if [f] returns a [t] since we would end with [res1]
       as [{ Backend1.t; Backend1.t }] and [res2] as [{ Backend2.t; Backend2.t
       }]. This is a good thing as it would corrupt the rest of the executions
       in context. If returning a new t is needed, we need to provide a new fold
       function with [f]'s return type specialized so that we can destruct it
       and build the correct value to be returned. *)
    (* assert (res1 = res2) ; *)
    res1

  let config : t -> Tezos_context_sigs.Config.t =
   fun t ->
    let config = function
      | Irmin_disk_context t -> Irmin_disk.config t
      | Irmin_mem_context t -> Irmin_mem.config t
      | Brassaia_disk_context t -> Brassaia_disk.config t
      | Brassaia_mem_context t -> Brassaia_mem.config t
      | Tezedge_context t -> Tezedge.config t
    in
    let config _name t =
      (config t [@profiler.span_f {verbosity = Notice} [_name; "config"]])
    in
    let config1 = config backend_1_name t.context_1 in
    let config2 = config backend_2_name t.context_2 in
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
    let get_protocol = function
      | Irmin_disk_context t -> Irmin_disk.get_protocol t
      | Irmin_mem_context t -> Irmin_mem.get_protocol t
      | Brassaia_disk_context t -> Brassaia_disk.get_protocol t
      | Brassaia_mem_context t -> Brassaia_mem.get_protocol t
      | Tezedge_context t -> Tezedge.get_protocol t
    in
    let get_protocol _name t =
      (get_protocol
         t [@profiler.span_f {verbosity = Notice} [_name; "get_protocol"]])
    in
    let* hash1 = get_protocol backend_1_name t.context_1 in
    let* hash2 = get_protocol backend_2_name t.context_2 in
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
    let fork_test_chain = function
      | Irmin_disk_context t ->
          Lwt.map
            irmin_disk_context
            (Irmin_disk.fork_test_chain ~protocol ~expiration t)
      | Irmin_mem_context t ->
          Lwt.map
            irmin_mem_context
            (Irmin_mem.fork_test_chain ~protocol ~expiration t)
      | Brassaia_disk_context t ->
          Lwt.map
            brassaia_disk_context
            (Brassaia_disk.fork_test_chain ~protocol ~expiration t)
      | Brassaia_mem_context t ->
          Lwt.map
            brassaia_mem_context
            (Brassaia_mem.fork_test_chain ~protocol ~expiration t)
      | Tezedge_context t ->
          Lwt.map
            tezedge_context
            (Tezedge.fork_test_chain ~protocol ~expiration t)
    in
    let fork_test_chain _name t =
      (fork_test_chain
         t [@profiler.span_f {verbosity = Notice} [_name; "fork_test_chain"]])
    in
    let* context_1 = fork_test_chain backend_1_name t.context_1 in
    let* context_2 = fork_test_chain backend_2_name t.context_2 in
    Lwt.return {context_1; context_2}

  let set_hash_version : t -> Context_hash.version -> t tzresult Lwt.t =
   fun t version ->
    let open Lwt_result_syntax in
    let set_hash_version = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_context_r (Irmin_disk.set_hash_version t version)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_context_r (Irmin_mem.set_hash_version t version)
      | Brassaia_disk_context t ->
          Lwt.map
            brassaia_disk_context_r
            (Brassaia_disk.set_hash_version t version)
      | Brassaia_mem_context t ->
          Lwt.map
            brassaia_mem_context_r
            (Brassaia_mem.set_hash_version t version)
      | Tezedge_context t ->
          Lwt.map tezedge_context_r (Tezedge.set_hash_version t version)
    in
    let set_hash_version _name t =
      (set_hash_version
         t [@profiler.span_f {verbosity = Notice} [_name; "set_hash_version"]])
    in
    let* context_1 = set_hash_version backend_1_name t.context_1 in
    let* context_2 = set_hash_version backend_2_name t.context_2 in
    return {context_1; context_2}

  let get_hash_version : t -> Context_hash.version =
   fun t ->
    let get_hash_version = function
      | Irmin_disk_context t -> Irmin_disk.get_hash_version t
      | Irmin_mem_context t -> Irmin_mem.get_hash_version t
      | Brassaia_disk_context t -> Brassaia_disk.get_hash_version t
      | Brassaia_mem_context t -> Brassaia_mem.get_hash_version t
      | Tezedge_context t -> Tezedge.get_hash_version t
    in
    let get_hash_version _name t =
      (get_hash_version
         t [@profiler.span_f {verbosity = Notice} [_name; "get_hash_version"]])
    in
    let version1 = get_hash_version backend_1_name t.context_1 in
    let version2 = get_hash_version backend_2_name t.context_2 in
    assert_and_return_result
      version1
      version2
      Context_hash.Version.equal
      Context_hash.Version.pp
      Context_hash.Version.pp
      "get_hash_version"
      version1

  let verify_tree_proof (type a) :
      Proof.tree Proof.t ->
      (tree -> (tree * a) Lwt.t) ->
      ( tree * a,
        [ `Proof_mismatch of string
        | `Stream_too_long of string
        | `Stream_too_short of string ] )
      result
      Lwt.t =
   fun proof f ->
    let open Lwt_result_syntax in
    let verify_tree_proof (type t)
        (verifier :
          Proof.tree Proof.t -> (t -> (t * a) Lwt.t) -> (t * a, 'b) result Lwt.t)
        (wrapper : t -> wrapped_tree) (extract : tree -> t) :
        (wrapped_tree * a, 'b) result Lwt.t =
      let f t =
        let tree = wrapper t in
        let open Lwt_syntax in
        let+ tree, res = f {tree_1 = tree; tree_2 = tree} in
        let tree = extract tree in
        (tree, res)
      in
      Lwt.map (Result.map (fun (t, a) -> (wrapper t, a))) (verifier proof f)
    in
    let verify_tree_proof = function
      | Irmin_disk ->
          verify_tree_proof
            Irmin_disk.verify_tree_proof
            irmin_disk_tree
            (function
            | {tree_1 = Irmin_disk_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_tree_proof")
      | Irmin_mem ->
          verify_tree_proof Irmin_mem.verify_tree_proof irmin_mem_tree (function
            | {tree_1 = Irmin_mem_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_tree_proof")
      | Brassaia_disk ->
          verify_tree_proof
            Brassaia_disk.verify_tree_proof
            brassaia_disk_tree
            (function
            | {tree_1 = Brassaia_disk_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_tree_proof")
      | Brassaia_mem ->
          verify_tree_proof
            Brassaia_mem.verify_tree_proof
            brassaia_mem_tree
            (function
            | {tree_1 = Brassaia_mem_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_tree_proof")
      | Tezedge -> assert false
    in
    let verify_tree_proof _name b =
      (verify_tree_proof
         b [@profiler.span_s {verbosity = Notice} [_name; "verify_tree_proof"]])
    in
    let* tree_1, res1 = verify_tree_proof backend_1_name backend_1 in
    let* tree_2, res2 = verify_tree_proof backend_2_name backend_2 in
    assert (res1 = res2) ;
    return ({tree_1; tree_2}, res1)

  (* FIXME: either remove one of the stream/tree version or factorize code.
     See https://gitlab.com/tezos/tezos/-/issues/7998 *)
  let verify_stream_proof (type a) :
      Proof.stream Proof.t ->
      (tree -> (tree * a) Lwt.t) ->
      ( tree * a,
        [ `Proof_mismatch of string
        | `Stream_too_long of string
        | `Stream_too_short of string ] )
      result
      Lwt.t =
   fun proof f ->
    let open Lwt_result_syntax in
    let verify_stream_proof (type t)
        (verifier :
          Proof.stream Proof.t ->
          (t -> (t * a) Lwt.t) ->
          (t * a, 'b) result Lwt.t) (wrapper : t -> wrapped_tree)
        (extract : tree -> t) : (wrapped_tree * a, 'b) result Lwt.t =
      let f t =
        let tree = wrapper t in
        let open Lwt_syntax in
        let+ tree, res = f {tree_1 = tree; tree_2 = tree} in
        let tree = extract tree in
        (tree, res)
      in
      Lwt.map (Result.map (fun (t, a) -> (wrapper t, a))) (verifier proof f)
    in
    let verify_stream_proof = function
      | Irmin_disk ->
          verify_stream_proof
            Irmin_disk.verify_stream_proof
            irmin_disk_tree
            (function
            | {tree_1 = Irmin_disk_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_stream_proof")
      | Irmin_mem ->
          verify_stream_proof
            Irmin_mem.verify_stream_proof
            irmin_mem_tree
            (function
            | {tree_1 = Irmin_mem_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_stream_proof")
      | Brassaia_disk ->
          verify_stream_proof
            Brassaia_disk.verify_stream_proof
            brassaia_disk_tree
            (function
            | {tree_1 = Brassaia_disk_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_stream_proof")
      | Brassaia_mem ->
          verify_stream_proof
            Brassaia_mem.verify_stream_proof
            brassaia_mem_tree
            (function
            | {tree_1 = Brassaia_mem_tree tree; _} -> tree
            | _ -> backend_mismatch "verify_stream_proof")
      | Tezedge -> assert false
    in
    let verify_stream_proof _name b =
      (verify_stream_proof
         b
       [@profiler.span_s {verbosity = Notice} [_name; "verify_stream_proof"]])
    in
    let* tree_1, res1 = verify_stream_proof backend_1_name backend_1 in
    let* tree_2, res2 = verify_stream_proof backend_2_name backend_2 in
    assert (res1 = res2) ;
    return ({tree_1; tree_2}, res1)

  (** Exported functions *)

  let index t =
    let index = function
      | Irmin_disk_context t -> Irmin_disk_index (Irmin_disk.index t)
      | Irmin_mem_context t -> Irmin_mem_index (Irmin_mem.index t)
      | Brassaia_disk_context t -> Brassaia_disk_index (Brassaia_disk.index t)
      | Brassaia_mem_context t -> Brassaia_mem_index (Brassaia_mem.index t)
      | Tezedge_context t -> Tezedge_index (Tezedge.index t)
    in
    let index _name t =
      (index t [@profiler.span_f {verbosity = Notice} [_name; "index"]])
    in
    let index_1 = index backend_1_name t.context_1 in
    let index_2 = index backend_2_name t.context_2 in
    {index_1; index_2}

  (** GC functions *)

  let gc : index -> Context_hash.t -> unit Lwt.t =
   fun index context_hash ->
    let gc = function
      | Irmin_disk_index i -> Irmin_disk.gc i context_hash
      | Irmin_mem_index i -> Irmin_mem.gc i context_hash
      | Brassaia_disk_index i -> Brassaia_disk.gc i context_hash
      | Brassaia_mem_index i -> Brassaia_mem.gc i context_hash
      | Tezedge_index _i -> Lwt.return_unit
    in
    let gc _name i =
      (gc i [@profiler.span_f {verbosity = Notice} [_name; "gc"]])
    in
    let* () = gc backend_1_name index.index_1 in
    gc backend_2_name index.index_2

  let wait_gc_completion : index -> unit Lwt.t =
   fun index ->
    let wait_gc_completion = function
      | Irmin_disk_index i -> Irmin_disk.wait_gc_completion i
      | Irmin_mem_index i -> Irmin_mem.wait_gc_completion i
      | Brassaia_disk_index i -> Brassaia_disk.wait_gc_completion i
      | Brassaia_mem_index i -> Brassaia_mem.wait_gc_completion i
      | Tezedge_index _i -> Lwt.return_unit
    in
    let wait_gc_completion _name i =
      (wait_gc_completion
         i [@profiler.span_f {verbosity = Notice} [_name; "wait_gc_completion"]])
    in
    let* () = wait_gc_completion backend_1_name index.index_1 in
    wait_gc_completion backend_2_name index.index_2

  let is_gc_allowed : index -> bool =
   fun index ->
    let is_gc_allowed = function
      | Irmin_disk_index i -> Irmin_disk.is_gc_allowed i
      | Irmin_mem_index i -> Irmin_mem.is_gc_allowed i
      | Brassaia_disk_index i -> Brassaia_disk.is_gc_allowed i
      | Brassaia_mem_index i -> Brassaia_mem.is_gc_allowed i
      | Tezedge_index _i -> false
    in
    let is_gc_allowed _name i =
      (is_gc_allowed
         i [@profiler.span_f {verbosity = Notice} [_name; "is_gc_allowed"]])
    in
    let bool1 = is_gc_allowed backend_1_name index.index_1 in
    let bool2 = is_gc_allowed backend_2_name index.index_2 in
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
    let split = function
      | Irmin_disk_index i -> Irmin_disk.split i
      | Irmin_mem_index i -> Irmin_mem.split i
      | Brassaia_disk_index i -> Brassaia_disk.split i
      | Brassaia_mem_index i -> Brassaia_mem.split i
      | Tezedge_index _i -> Lwt.return_unit
    in
    let split _name i =
      (split i [@profiler.span_f {verbosity = Notice} [_name; "split"]])
    in
    let* () = split backend_1_name index.index_1 in
    split backend_2_name index.index_2

  let sync : index -> unit Lwt.t =
   fun index ->
    let sync = function
      | Irmin_disk_index i -> Irmin_disk.sync i
      | Irmin_mem_index i -> Irmin_mem.sync i
      | Brassaia_disk_index i -> Brassaia_disk.sync i
      | Brassaia_mem_index i -> Brassaia_mem.sync i
      | Tezedge_index _i -> Lwt.return_unit
    in
    let sync _name i =
      (sync i [@profiler.span_f {verbosity = Notice} [_name; "sync"]])
    in
    let* () = sync backend_1_name index.index_1 in
    sync backend_2_name index.index_2

  let exists : index -> Context_hash.t -> bool Lwt.t =
   fun index context_hash ->
    let exists = function
      | Irmin_disk_index i -> Irmin_disk.exists i context_hash
      | Irmin_mem_index i -> Irmin_mem.exists i context_hash
      | Brassaia_disk_index i -> Brassaia_disk.exists i context_hash
      | Brassaia_mem_index i -> Brassaia_mem.exists i context_hash
      | Tezedge_index i -> Lwt.return @@ Tezedge.exists i context_hash
    in
    let exists _name i =
      (exists i [@profiler.span_f {verbosity = Notice} [_name; "exists"]])
    in
    let* bool1 = exists backend_1_name index.index_1 in
    let* bool2 = exists backend_2_name index.index_2 in
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
    let close = function
      | Irmin_disk_index i -> Irmin_disk.close i
      | Irmin_mem_index i -> Irmin_mem.close i
      | Brassaia_disk_index i -> Brassaia_disk.close i
      | Brassaia_mem_index i -> Brassaia_mem.close i
      | Tezedge_index _i -> assert false
    in
    let close _name i =
      (close i [@profiler.span_f {verbosity = Notice} [_name; "close"]])
    in
    let* () = close backend_1_name index.index_1 in
    close backend_2_name index.index_2

  let compute_testchain_chain_id : Block_hash.t -> Chain_id.t =
   fun block_hash ->
    let compute_testchain_chain_id = function
      | Irmin_disk -> Irmin_disk.compute_testchain_chain_id block_hash
      | Irmin_mem -> Irmin_mem.compute_testchain_chain_id block_hash
      | Brassaia_disk -> Brassaia_disk.compute_testchain_chain_id block_hash
      | Brassaia_mem -> Brassaia_mem.compute_testchain_chain_id block_hash
      | Tezedge -> Tezedge.compute_testchain_chain_id block_hash
    in
    let compute_testchain_chain_id _name b =
      (compute_testchain_chain_id
         b
       [@profiler.span_f
         {verbosity = Notice} [_name; "compute_testchain_chain_id"]])
    in
    let chain_id1 = compute_testchain_chain_id backend_1_name backend_1 in
    let chain_id2 = compute_testchain_chain_id backend_2_name backend_2 in
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
    let add_predecessor_block_metadata_hash = function
      | Irmin_disk_context t ->
          Lwt.map
            irmin_disk_context
            (Irmin_disk.add_predecessor_block_metadata_hash t hash)
      | Irmin_mem_context t ->
          Lwt.map
            irmin_mem_context
            (Irmin_mem.add_predecessor_block_metadata_hash t hash)
      | Brassaia_disk_context t ->
          Lwt.map
            brassaia_disk_context
            (Brassaia_disk.add_predecessor_block_metadata_hash t hash)
      | Brassaia_mem_context t ->
          Lwt.map
            brassaia_mem_context
            (Brassaia_mem.add_predecessor_block_metadata_hash t hash)
      | Tezedge_context t ->
          Lwt.map
            tezedge_context
            (Tezedge.add_predecessor_block_metadata_hash t hash)
    in
    let add_predecessor_block_metadata_hash _name t =
      (add_predecessor_block_metadata_hash
         t
       [@profiler.span_f
         {verbosity = Notice} [_name; "add_predecessor_block_metadata_hash"]])
    in
    let* context_1 =
      add_predecessor_block_metadata_hash backend_1_name t.context_1
    in
    let* context_2 =
      add_predecessor_block_metadata_hash backend_2_name t.context_2
    in
    Lwt.return {context_1; context_2}

  let add_predecessor_ops_metadata_hash :
      t -> Operation_metadata_list_list_hash.t -> t Lwt.t =
   fun t hash ->
    let add_predecessor_ops_metadata_hash = function
      | Irmin_disk_context t ->
          Lwt.map
            irmin_disk_context
            (Irmin_disk.add_predecessor_ops_metadata_hash t hash)
      | Irmin_mem_context t ->
          Lwt.map
            irmin_mem_context
            (Irmin_mem.add_predecessor_ops_metadata_hash t hash)
      | Brassaia_disk_context t ->
          Lwt.map
            brassaia_disk_context
            (Brassaia_disk.add_predecessor_ops_metadata_hash t hash)
      | Brassaia_mem_context t ->
          Lwt.map
            brassaia_mem_context
            (Brassaia_mem.add_predecessor_ops_metadata_hash t hash)
      | Tezedge_context t ->
          Lwt.map
            tezedge_context
            (Tezedge.add_predecessor_ops_metadata_hash t hash)
    in
    let add_predecessor_ops_metadata_hash _name t =
      (add_predecessor_ops_metadata_hash
         t
       [@profiler.span_f
         {verbosity = Notice} [_name; "add_predecessor_ops_metadata_hash"]])
    in
    let* context_1 =
      add_predecessor_ops_metadata_hash backend_1_name t.context_1
    in
    let* context_2 =
      add_predecessor_ops_metadata_hash backend_2_name t.context_2
    in
    Lwt.return {context_1; context_2}

  let hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t =
   fun ~time ?message t ->
    let hash = function
      | Irmin_disk_context t -> Irmin_disk.hash ~time ?message t
      | Irmin_mem_context t -> Irmin_mem.hash ~time ?message t
      | Brassaia_disk_context t -> Brassaia_disk.hash ~time ?message t
      | Brassaia_mem_context t -> Brassaia_mem.hash ~time ?message t
      | Tezedge_context t -> Tezedge.hash ~time ?message t
    in
    let hash _name t =
      (hash t [@profiler.span_f {verbosity = Notice} [_name; "hash"]])
    in
    let context_hash1 = hash backend_1_name t.context_1 in
    let context_hash2 = hash backend_2_name t.context_2 in
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
    let commit_test_chain_genesis = function
      | Irmin_disk_context t ->
          Irmin_disk.commit_test_chain_genesis t block_header
      | Irmin_mem_context t ->
          Irmin_mem.commit_test_chain_genesis t block_header
      | Brassaia_disk_context t ->
          Brassaia_disk.commit_test_chain_genesis t block_header
      | Brassaia_mem_context t ->
          Brassaia_mem.commit_test_chain_genesis t block_header
      | Tezedge_context t -> Tezedge.commit_test_chain_genesis t block_header
    in
    let commit_test_chain_genesis _name t =
      (commit_test_chain_genesis
         t
       [@profiler.span_f
         {verbosity = Notice} [_name; "commit_test_chain_genesis"]])
    in
    let* block_header1 =
      commit_test_chain_genesis backend_1_name context.context_1
    in
    let* block_header2 =
      commit_test_chain_genesis backend_2_name context.context_2
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
    let get_test_chain = function
      | Irmin_disk_context t -> Irmin_disk.get_test_chain t
      | Irmin_mem_context t -> Irmin_mem.get_test_chain t
      | Brassaia_disk_context t -> Brassaia_disk.get_test_chain t
      | Brassaia_mem_context t -> Brassaia_mem.get_test_chain t
      | Tezedge_context t -> Tezedge.get_test_chain t
    in
    let get_test_chain _name t =
      (get_test_chain
         t [@profiler.span_f {verbosity = Notice} [_name; "get_test_chain"]])
    in
    let* status1 = get_test_chain backend_1_name t.context_1 in
    let* status2 = get_test_chain backend_2_name t.context_2 in
    assert (Test_chain_status.equal status1 status2) ;
    Lwt.return status1

  let add_test_chain : t -> Test_chain_status.t -> t Lwt.t =
   fun t status ->
    let add_test_chain = function
      | Irmin_disk_context t ->
          Lwt.map irmin_disk_context (Irmin_disk.add_test_chain t status)
      | Irmin_mem_context t ->
          Lwt.map irmin_mem_context (Irmin_mem.add_test_chain t status)
      | Brassaia_disk_context t ->
          Lwt.map brassaia_disk_context (Brassaia_disk.add_test_chain t status)
      | Brassaia_mem_context t ->
          Lwt.map brassaia_mem_context (Brassaia_mem.add_test_chain t status)
      | Tezedge_context t ->
          Lwt.map tezedge_context (Tezedge.add_test_chain t status)
    in
    let add_test_chain _name t =
      (add_test_chain
         t [@profiler.span_f {verbosity = Notice} [_name; "add_test_chain"]])
    in
    let* context_1 = add_test_chain backend_1_name t.context_1 in
    let* context_2 = add_test_chain backend_2_name t.context_2 in
    Lwt.return {context_1; context_2}

  let commit :
      time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t =
   fun ~time ?message t ->
    let commit = function
      | Irmin_disk_context t -> Irmin_disk.commit ~time ?message t
      | Irmin_mem_context t -> Irmin_mem.commit ~time ?message t
      | Brassaia_disk_context t -> Brassaia_disk.commit ~time ?message t
      | Brassaia_mem_context t -> Brassaia_mem.commit ~time ?message t
      | Tezedge_context t -> Tezedge.commit ~time ?message t
    in
    let commit _name t =
      (commit t [@profiler.span_f {verbosity = Notice} [_name; "commit"]])
    in
    let* context_hash1 = commit backend_1_name t.context_1 in
    let* context_hash2 = commit backend_2_name t.context_2 in
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
    let commit_genesis = function
      | Irmin_disk_index i ->
          Irmin_disk.commit_genesis i ~chain_id ~time ~protocol
      | Irmin_mem_index i ->
          Irmin_mem.commit_genesis i ~chain_id ~time ~protocol
      | Brassaia_disk_index i ->
          Brassaia_disk.commit_genesis i ~chain_id ~time ~protocol
      | Brassaia_mem_index i ->
          Brassaia_mem.commit_genesis i ~chain_id ~time ~protocol
      | Tezedge_index i -> Tezedge.commit_genesis i ~chain_id ~time ~protocol
    in
    let commit_genesis _name t =
      (commit_genesis
         t [@profiler.span_f {verbosity = Notice} [_name; "commit_genesis"]])
    in
    let* context_hash1 = commit_genesis backend_1_name index.index_1 in
    let* context_hash2 = commit_genesis backend_2_name index.index_2 in
    assert (Context_hash.equal context_hash1 context_hash2) ;
    return context_hash1

  let compute_testchain_genesis : Block_hash.t -> Block_hash.t =
   fun block_hash ->
    let compute_testchain_genesis = function
      | Irmin_disk -> Irmin_disk.compute_testchain_genesis block_hash
      | Irmin_mem -> Irmin_mem.compute_testchain_genesis block_hash
      | Brassaia_disk -> Brassaia_disk.compute_testchain_genesis block_hash
      | Brassaia_mem -> Brassaia_mem.compute_testchain_genesis block_hash
      | Tezedge -> Tezedge.compute_testchain_genesis block_hash
    in
    let compute_testchain_genesis _name b =
      (compute_testchain_genesis
         b
       [@profiler.span_f
         {verbosity = Notice} [_name; "compute_testchain_genesis"]])
    in
    let block_hash1 = compute_testchain_genesis backend_1_name backend_1 in
    let block_hash2 = compute_testchain_genesis backend_2_name backend_2 in
    assert_and_return_result
      block_hash1
      block_hash2
      Block_hash.equal
      Block_hash.pp
      Block_hash.pp
      "compute_testchain_genesis"
      block_hash1

  let export_snapshot : index -> Context_hash.t -> path:string -> unit Lwt.t =
   fun index context_hash ~path ->
    let export_snapshot = function
      | Irmin_disk_index i -> Irmin_disk.export_snapshot i context_hash ~path
      | Irmin_mem_index i -> Irmin_mem.export_snapshot i context_hash ~path
      | Brassaia_disk_index i ->
          Brassaia_disk.export_snapshot i context_hash ~path
      | Brassaia_mem_index i ->
          Brassaia_mem.export_snapshot i context_hash ~path
      | Tezedge_index _i -> assert false
    in
    let export_snapshot _name i =
      (export_snapshot
         i [@profiler.span_f {verbosity = Notice} [_name; "export_snapshot"]])
    in
    let* () = export_snapshot backend_1_name index.index_1 in
    export_snapshot backend_2_name index.index_2

  let merkle_tree :
      t ->
      Proof.merkle_leaf_kind ->
      key ->
      Tezos_context_sigs.Context.Proof_types.merkle_tree Lwt.t =
   fun context leaf_kind path ->
    let merkle_tree = function
      | Irmin_disk_context t -> Irmin_disk.merkle_tree t leaf_kind path
      | Irmin_mem_context t -> Irmin_mem.merkle_tree t leaf_kind path
      | Brassaia_disk_context t -> Brassaia_disk.merkle_tree t leaf_kind path
      | Brassaia_mem_context t -> Brassaia_mem.merkle_tree t leaf_kind path
      | Tezedge_context _t -> assert false
    in
    let merkle_tree _name t =
      (merkle_tree
         t [@profiler.span_f {verbosity = Notice} [_name; "merkle_tree"]])
    in
    let* proof1 = merkle_tree backend_1_name context.context_1 in
    let* proof2 = merkle_tree backend_2_name context.context_2 in
    let pp ppf _ = Format.fprintf ppf "[proof]" in
    assert_and_return_result_lwt proof1 proof2 ( = ) pp pp "merkle_tree" proof1

  let merkle_tree_v2 :
      t -> Proof.merkle_leaf_kind -> key -> Proof.tree Proof.t Lwt.t =
   fun context leaf_kind path ->
    let merkle_tree_v2 = function
      | Irmin_disk_context t -> Irmin_disk.merkle_tree_v2 t leaf_kind path
      | Irmin_mem_context t -> Irmin_mem.merkle_tree_v2 t leaf_kind path
      | Brassaia_disk_context t -> Brassaia_disk.merkle_tree_v2 t leaf_kind path
      | Brassaia_mem_context t -> Brassaia_mem.merkle_tree_v2 t leaf_kind path
      | Tezedge_context _t -> assert false
    in
    let merkle_tree_v2 _name t =
      (merkle_tree_v2
         t [@profiler.span_f {verbosity = Notice} [_name; "merkle_tree_v2"]])
    in
    let* proof1 = merkle_tree_v2 backend_1_name context.context_1 in
    let* proof2 = merkle_tree_v2 backend_2_name context.context_2 in
    let pp ppf _ = Format.fprintf ppf "[proof]" in
    assert_and_return_result_lwt
      proof1
      proof2
      ( = )
      pp
      pp
      "merkle_tree_v2"
      proof1

  let checkout index context_hash =
    let checkout = function
      | Irmin_disk_index i ->
          Lwt.map irmin_disk_context_o (Irmin_disk.checkout i context_hash)
      | Irmin_mem_index i ->
          Lwt.map irmin_mem_context_o (Irmin_mem.checkout i context_hash)
      | Brassaia_disk_index i ->
          Lwt.map
            brassaia_disk_context_o
            (Brassaia_disk.checkout i context_hash)
      | Brassaia_mem_index i ->
          Lwt.map brassaia_mem_context_o (Brassaia_mem.checkout i context_hash)
      | Tezedge_index i ->
          Lwt.map
            tezedge_context_o
            (Lwt.return @@ Tezedge.checkout i context_hash)
    in
    let checkout _name t =
      (checkout t [@profiler.span_f {verbosity = Notice} [_name; "checkout"]])
    in
    let* context_1 = checkout backend_1_name index.index_1 in
    let* context_2 = checkout backend_2_name index.index_2 in
    match (context_1, context_2) with
    | Some context_1, Some context_2 -> Lwt.return_some {context_1; context_2}
    | _ -> Lwt.return_none

  let checkout_exn index context_hash =
    let checkout_exn = function
      | Irmin_disk_index i ->
          Lwt.map irmin_disk_context (Irmin_disk.checkout_exn i context_hash)
      | Irmin_mem_index i ->
          Lwt.map irmin_mem_context (Irmin_mem.checkout_exn i context_hash)
      | Brassaia_disk_index i ->
          Lwt.map
            brassaia_disk_context
            (Brassaia_disk.checkout_exn i context_hash)
      | Brassaia_mem_index i ->
          Lwt.map
            brassaia_mem_context
            (Brassaia_mem.checkout_exn i context_hash)
      | Tezedge_index i ->
          Lwt.map tezedge_context (Tezedge.checkout_exn i context_hash)
    in
    let checkout_exn _name t =
      (checkout_exn
         t [@profiler.span_f {verbosity = Notice} [_name; "checkout_exn"]])
    in
    let* context_1 = checkout_exn backend_1_name index.index_1 in
    let* context_2 = checkout_exn backend_2_name index.index_2 in
    Lwt.return {context_1; context_2}

  (* Have the interface compatible with Register *)
  let set_protocol = add_protocol
end
