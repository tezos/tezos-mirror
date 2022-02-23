(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Code that is only meant to be used by src/lib_proxy/light*.ml files *)

let key_to_string = String.concat ";"

module Store = Local_context
module StringMap = TzString.Map

let rec raw_context_to_irmin_tree
    (raw : Tezos_shell_services.Block_services.raw_context) : Store.tree Lwt.t =
  let open Lwt_syntax in
  match raw with
  | Key (bytes : Bytes.t) -> Lwt.return (Store.Tree.of_raw (`Value bytes))
  | Cut -> Lwt.return (Store.Tree.empty Store.empty)
  | Dir pairs ->
      let process_recursively tree (key, sub_raw_context) =
        let* sub_tree = raw_context_to_irmin_tree sub_raw_context in
        Store.Tree.add_tree tree [key] sub_tree
      in
      List.fold_left_s
        process_recursively
        (Store.Tree.empty Store.empty)
        (TzString.Map.bindings pairs)

module Merkle = struct
  (** Parse a [string] to a [Context_hash.t], transforming the error into
      a [string] in case of failure for convenience. *)
  let string_to_hash (s : string) : (Context_hash.t, string) result =
    Context_hash.of_b58check s
    |> Result.map_error
         (Format.asprintf
            "Failed to convert %s to a Context hash. Error is: %a"
            s
            pp_print_trace)

  let rec merkle_node_to_irmin_tree repo mnode :
      (Store.tree, string) result Lwt.t =
    let open Tezos_shell_services.Block_services in
    let open Lwt_result_syntax in
    match mnode with
    | Hash (kind, s) -> (
        match string_to_hash s with
        | Error _ as e -> Lwt.return e
        | Ok hash_str ->
            let irmin_hash =
              match kind with
              | Contents -> `Contents hash_str
              | Node -> `Node hash_str
            in
            return @@ Store.Tree.shallow repo irmin_hash)
    | Data raw_context -> Lwt_result.ok @@ raw_context_to_irmin_tree raw_context
    | Continue subtree -> merkle_tree_to_irmin_tree repo subtree

  and merkle_tree_to_irmin_tree repo mtree : (Store.tree, string) result Lwt.t =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun built_tree (key, mnode) ->
        let* subtree = merkle_node_to_irmin_tree repo mnode in
        Lwt_result.ok @@ Store.Tree.add_tree built_tree [key] subtree)
      (Store.Tree.empty Store.empty)
      (StringMap.bindings mtree)

  let rev_key_to_string k = key_to_string @@ List.rev k

  (** [reversed_key] is the key being inspected (reversed)
      Its only purpose is to provide detailed error messages.
      Returns [None] if no change was necessary, [Some result_tree]
      where [result_tree] is the union of [tree] and [mtree]. *)
  let rec union_irmin_tree_merkle_node repo reversed_key tree =
    let open Lwt_result_syntax in
    function
    | Tezos_shell_services.Block_services.Hash (_, mnode_hash_str) -> (
        match string_to_hash mnode_hash_str with
        | Error _ as err -> Lwt.return err
        | Ok mnode_hash ->
            let tree_hash = Store.Tree.hash tree in
            if Context_hash.equal tree_hash mnode_hash then
              (* The point of the 'Hash' case is to do a check: no addition
                     of data. Hence: *)
              return_none
            else
              fail
                (Format.sprintf
                   "At %s: Irmin tree hash is %s while Merkle node hash is %s"
                   (rev_key_to_string reversed_key)
                   (Context_hash.to_string tree_hash)
                   mnode_hash_str))
    | Data raw_context ->
        (* Because the light mode only performs RPC requests when
           necessary, we're sure the available [tree] has some shallow
           content: that is why we MUST take [raw_context].
           If Irmin exposed a Store.Tree.has_shallow: tree -> bool function,
           we could assert that it returns true on [tree].
           Unfortunately, this function does not exist,
           and cannot be implemented externally in an efficient manner. *)
        (* Check the incoming data ([raw_context]) agrees with
                 the data in place ([tree]) *)
        let*! tree_in_merkle = raw_context_to_irmin_tree raw_context in
        let hash_in_merkle = Store.Tree.hash tree_in_merkle in
        let tree_hash = Store.Tree.hash tree in
        if Context_hash.equal hash_in_merkle tree_hash then
          (* take incoming data *)
          return_some tree_in_merkle
        else
          Lwt.return_error
            (Format.sprintf
               "At %s: hash of Data is %s while incoming hash is %s"
               (rev_key_to_string reversed_key)
               (Context_hash.to_string tree_hash)
               (Context_hash.to_string hash_in_merkle))
    | Continue msubtree ->
        union_irmin_tree_merkle_tree repo reversed_key tree msubtree

  (** [reversed_key] is the key being inspected (reversed)
      Its only purpose is to provide detailed error messages.
      Returns [None] if no change was necessary, [Some result_tree] where
      [result_tree] is the union of [tree] and [mtree]. *)
  and union_irmin_tree_merkle_tree repo reversed_key tree mtree =
    let open Lwt_result_syntax in
    let hash_start_tree = Store.Tree.hash tree in
    let change = ref false in
    let* res =
      List.fold_left_es
        (fun built_tree (subkey, mnode) ->
          let*! subtree_opt = Store.Tree.find_tree built_tree [subkey] in
          match subtree_opt with
          | None ->
              (* Integrating new data *)
              change := true ;
              let* subtree = merkle_node_to_irmin_tree repo mnode in
              let*! t = Store.Tree.add_tree built_tree [subkey] subtree in
              return t
          | Some subtree -> (
              (* Unioning existing data *)
              let* subtree'_opt =
                union_irmin_tree_merkle_node
                  repo
                  (subkey :: reversed_key)
                  subtree
                  mnode
              in
              match subtree'_opt with
              | None -> Lwt.return_ok built_tree (* no change (hashes agree) *)
              | Some subtree' ->
                  change := true ;
                  (* This call to [Store.Tree.remove] should NOT
                     be necessary. Is there a bug in add_tree? *)
                  let*! build_tree' = Store.Tree.remove built_tree [subkey] in
                  let*! t = Store.Tree.add_tree build_tree' [subkey] subtree' in
                  return t))
        tree
        (StringMap.bindings mtree)
    in
    let hash_end_tree = Store.Tree.hash res in
    if not (Context_hash.equal hash_start_tree hash_end_tree) then
      Lwt.return_error
      @@ Format.asprintf
           "The hash of the shallow tree %a does not correspond to the \
            unshallowed one %a"
           Context_hash.pp
           hash_start_tree
           Context_hash.pp
           hash_end_tree
    else if !change then return_some res
    else return_none

  let union_irmin_tree_merkle_tree repo tree mtree =
    let open Lwt_tzresult_syntax in
    let* tree_opt = union_irmin_tree_merkle_tree repo [] tree mtree in
    let tree = Option.value ~default:tree tree_opt in
    return tree

  let sequence_result_unit (results : (unit, 'b) result list) :
      (unit, 'b) result =
    (* ignore the [()]s with [Fun.id], simply stop on the first [Error] with
       [iter_e]. *)
    List.iter_e Fun.id results

  (** Whether [tree] contains [mnode]. Returns unit if yes, otherwise
      an explanation as to why [tree] doesn't contain [mnode]. *)
  let rec contains_merkle_node tree key mnode : (unit, string) result Lwt.t =
    let open Tezos_shell_services.Block_services in
    let open Lwt_result_syntax in
    let apply_or_fail_at k f =
      (* Applies [f] on the tree mapped by [k] if present, otherwise fail *)
      let*! tree_opt = Store.Tree.find_tree tree [k] in
      match tree_opt with
      | None ->
          fail
          @@ Format.sprintf
               "Key %s, which is in Merkle tree, cannot be found in in-memory \
                tree"
               k
      | Some subtree -> f subtree
    in
    match mnode with
    | Hash (_, mnode_hash_str) -> (
        apply_or_fail_at key @@ fun subtree ->
        let subtree_hash = Store.Tree.hash subtree in
        let mnode_hash_res = string_to_hash mnode_hash_str in
        match mnode_hash_res with
        | Error _ as e -> Lwt.return e
        | Ok mnode_hash ->
            if Context_hash.equal subtree_hash mnode_hash then return_unit
            else
              fail
                (Format.asprintf
                   "Key %s has hash %a in Irmin tree while node hash is %s"
                   key
                   Context_hash.pp
                   subtree_hash
                   mnode_hash_str))
    | Data _ ->
        fail
          "Data node found in Merkle tree. While I could check that the data \
           is in the in-memory tree, this is unexpected: only inclusion of \
           hashes should be checked."
    | Continue submtree ->
        apply_or_fail_at key @@ fun subtree ->
        contains_merkle_tree subtree submtree

  and contains_merkle_tree tree mtree =
    let open Lwt_syntax in
    let seq = StringMap.bindings mtree in
    let* seq =
      List.rev_map_p
        (fun (key, mnode) -> contains_merkle_node tree key mnode)
        seq
    in
    return @@ sequence_result_unit seq

  (** Wether we are on the path to ignore for tree shape comparison. Technically isomorphic
      to [string list option] but more expressive. *)
  type path_to_ignore = NotThisPath | ThisPath of string list

  let rec nodes_shape_match path_to_ignore
      (left : Tezos_shell_services.Block_services.merkle_node)
      (right : Tezos_shell_services.Block_services.merkle_node) =
    let open Tezos_shell_services.Block_services in
    match (left, right, path_to_ignore) with
    | (Hash _, Hash _, _) | (Data _, Data _, _) -> None
    | (Continue left_tree, Continue right_tree, _) -> (
        trees_shape_match path_to_ignore left_tree right_tree |> function
        | [] -> None
        | errors -> Some errors)
    | (_, _, ThisPath _) ->
        (* Shapes are different but this is the path to ignore. *)
        None
    | _ ->
        Some
          [
            Format.asprintf
              "@[<v 2>Nodes have different shapes.@,Left:@,%a@,Right:@,%a@]"
              pp_merkle_node
              left
              pp_merkle_node
              right;
          ]

  and trees_shape_match path_to_ignore
      (left : Tezos_shell_services.Block_services.merkle_tree)
      (right : Tezos_shell_services.Block_services.merkle_tree) =
    TzString.Map.merge
      (fun key left_val_opt right_val_opt ->
        match (left_val_opt, right_val_opt, path_to_ignore) with
        | (Some _, None, _) | (None, Some _, _) ->
            Some
              [Format.asprintf "Key \"%s\" is missing in one of the trees." key]
        | (None, None, _) ->
            (* Unreachable, at least one of the maps has the key *)
            assert false
        | (Some left_value, Some right_value, ThisPath (hd_key :: tl_key))
          when String.equal hd_key key ->
            nodes_shape_match (ThisPath tl_key) left_value right_value
        | (Some left_value, Some right_value, _) ->
            nodes_shape_match NotThisPath left_value right_value)
      left
      right
    |> TzString.Map.bindings
    |> List.map (fun (_key, errors) -> errors)
    |> List.flatten

  let trees_shape_match key_path
      (left : Tezos_shell_services.Block_services.merkle_tree)
      (right : Tezos_shell_services.Block_services.merkle_tree) =
    trees_shape_match (ThisPath key_path) left right |> function
    | [] -> Ok ()
    | errors -> Error errors
end
