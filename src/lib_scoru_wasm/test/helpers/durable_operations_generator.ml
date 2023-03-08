(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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

open QCheck2

type key = string list

(* Weighted operations list:
   operations will be generated proportionally to their weight *)
type operations_distribution = (int * Durable_operation.some_op) list

type testcase = {
  inital_state : (key * string) list;
  operations : Durable_operation.some_input list;
}

module Operations_generator = struct
  open Gen

  let range i j =
    WithExceptions.List.init ~loc:__LOC__ (1 + j - i) (fun n -> i + n)

  let range_chars (i : char) (j : char) =
    List.map Char.chr @@ range (Char.code i) (Char.code j)

  let max_path_segments = 10

  let gen_path_char =
    Gen.oneof
      [
        Gen.oneofl ['.'; '-'; '_'];
        Gen.map Char.chr (Gen.int_range (Char.code 'a') (Char.code 'z'));
        Gen.map Char.chr (Gen.int_range (Char.code 'A') (Char.code 'Z'));
        Gen.map Char.chr (Gen.int_range (Char.code '0') (Char.code '9'));
      ]

  let gen_arbitrary_path =
    let* segments = Gen.int_range 1 max_path_segments in
    Gen.list_repeat segments
    @@ Gen.string_size ~gen:gen_path_char (Gen.int_range 1 10)

  let generate_initial_keys (initial_tree_size : int) :
      (key * string) list Gen.t =
    let+ keys = Gen.list_size (Gen.return initial_tree_size) gen_arbitrary_path
    and+ values =
      Gen.list_size
        (Gen.return initial_tree_size)
        (Gen.string_size ~gen:Gen.char (Gen.int_bound 2048))
    in
    let kv = WithExceptions.List.combine ~loc:__LOC__ keys values in
    (* 20% of all keys go to readonly subpath *)
    let ro_ops = Int.(div (List.length kv) 5) in
    List.mapi
      (fun i (k, v) -> if i < ro_ops then ("readonly" :: k, v) else (k, v))
      kv

  (* TODO: this will be implemented properly in next MR *)
  let gen_op _trie _ops_distribution =
    let+ key = gen_arbitrary_path in
    Durable_operation.Some_input (Find_value, key)

  let rec gen_ops ops_distribution (trie : int Trie.t) n ops =
    let open Durable_operation in
    if n <= 0 then Gen.return @@ List.rev ops
    else
      let* some_inp = gen_op trie ops_distribution in
      let new_trie =
        Option.value ~default:trie
        @@
        match some_inp with
        | Some_input (Set_value_exn, (edit_readonly, key, value)) ->
            Trie.set_value ~edit_readonly key (String.length value) trie
        | Some_input (Copy_tree_exn, (edit_readonly, from_key, to_key)) ->
            Trie.copy_tree ~edit_readonly ~from_key ~to_key trie
        | Some_input (Move_tree_exn, (from_key, to_key)) ->
            Trie.move_tree ~from_key ~to_key trie
        | Some_input (Delete, (edit_readonly, key)) ->
            Trie.delete ~edit_readonly key trie
        | Some_input (Write_value_exn, (edit_readonly, key, offset, value)) ->
            let new_value =
              Int.max
                (Option.value ~default:0 @@ Trie.get_value key trie)
                (Int64.to_int offset + String.length value)
            in
            Trie.set_value ~edit_readonly key new_value trie
        | _ -> None
      in
      (gen_ops [@tailcall]) ops_distribution new_trie (n - 1) (some_inp :: ops)

  let gen_testcase ~(initial_size : int) ~(operations_number : int)
      (distirbution : operations_distribution) =
    let* init_kvs = generate_initial_keys initial_size in
    let initial_trie =
      List.fold_left
        (fun trie (k, v) ->
          WithExceptions.Option.get ~loc:__LOC__
          @@ Trie.set_value ~edit_readonly:true k (String.length v) trie)
        Trie.empty
        init_kvs
    in
    let+ ops = gen_ops distirbution initial_trie operations_number [] in
    {inital_state = init_kvs; operations = ops}
end

module Make_durable_operations_runner
    (Durable : Durable_snapshot_util.Testable_durable_sig) =
struct
  open Lwt_syntax
  open Tezos_scoru_wasm_helpers.Encodings_util
  open Tezos_scoru_wasm_helpers.Wasm_utils

  let durable_exn_handler (act : unit -> 'a Lwt.t)
      (cont : ('a, exn) result -> 'b Lwt.t) =
    Lwt.try_bind
      act
      (fun res -> cont @@ Ok res)
      (fun e ->
        Tezos_scoru_wasm_durable_snapshot.Durable.(
          match e with
          | Invalid_key _ | Index_too_large _ | Value_not_found | Tree_not_found
          | Out_of_bounds _ | Durable_empty | Readonly_value | IO_too_large
          | Tezos_lazy_containers.Chunked_byte_vector.Bounds
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4958 *)
          | Tezos_tree_encoding.Key_not_found _ ->
              cont @@ Error e
          (* If it's another kind of exn:
             something went wrong, re-throw it*)
          | _ -> raise e))

  (* Stress test doesn't care about exceptions
     thrown out of functions.
     It's implied that underlying Durable has already checked them.
  *)
  type op_res = Tree : Durable.t -> op_res | Value : 'a -> op_res

  let tree_res (act : Durable.t Lwt.t) = Lwt.map (fun x -> Tree x) act

  let value_res (act : 'a Lwt.t) = Lwt.map (fun x -> Value x) act

  let supress_durable_exn dur (act : unit -> op_res Lwt.t) =
    durable_exn_handler act (fun x ->
        match x with
        | Ok (Tree t) -> Lwt.return t
        | Ok (Value _value) -> Lwt.return dur
        | _ -> Lwt.return dur)

  (* Create new tree with passed list of key values *)
  let initialize_tree (kvs : (key * string) list) =
    let open Lwt_syntax in
    let open Tezos_scoru_wasm_durable_snapshot in
    let ro, wo =
      List.partition
        (fun (k, _) -> Option.equal String.equal (List.hd k) (Some "readonly"))
        kvs
    in
    let ro = List.map (fun (k, v) -> (Durable_operation.key_to_str k, v)) ro in
    let wo = List.map (fun (k, v) -> (Durable_operation.key_to_str k, v)) wo in
    (* Create Durable_storage out of WO keys.
       Basically taking advantage of Current durable encoding
    *)
    let* init_wo = Lwt.map Durable.of_storage_exn @@ make_durable wo in
    (* Add RO keys in the tree *)
    let* init_tezos_durable =
      Lwt_list.fold_left_s
        (fun dur (k, v) ->
          Durable.set_value_exn
            ~edit_readonly:true
            dur
            (Durable.key_of_string_exn k)
            v)
        init_wo
        ro
    in
    (* Encode tree to the irmin one *)
    let* init_tree = empty_tree () in
    Tree_encoding_runner.encode
      Tezos_scoru_wasm_durable_snapshot.Durable.encoding
      init_tezos_durable
      init_tree

  let run_testcase {inital_state; operations} =
    let open Durable_operation in
    let* init_tree = initialize_tree inital_state in
    (* Decode initial paired durable *)
    let* init_durable =
      Tree_encoding_runner.decode Durable.encoding init_tree
    in
    Lwt_list.fold_left_s
      (fun dur op ->
        supress_durable_exn dur @@ fun () ->
        match op with
        | Some_input (Find_value, key) ->
            value_res
            @@ Durable.find_value
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Find_value_exn, key) ->
            value_res
            @@ Durable.find_value_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Set_value_exn, (edit_readonly, key, value)) ->
            tree_res
            @@ Durable.set_value_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 value
        | Some_input (Copy_tree_exn, (edit_readonly, from, to_)) ->
            tree_res
            @@ Durable.copy_tree_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str from)
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str to_)
        | Some_input (Move_tree_exn, (from, to_)) ->
            tree_res
            @@ Durable.move_tree_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str from)
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str to_)
        | Some_input (Delete, (edit_readonly, key)) ->
            tree_res
            @@ Durable.delete
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (List, key) ->
            value_res
            @@ Durable.list
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Count_subtrees, key) ->
            value_res
            @@ Durable.count_subtrees
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Substree_name_at, (key, idx)) ->
            value_res
            @@ Durable.subtree_name_at
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 idx
        | Some_input (Hash, key) ->
            value_res
            @@ Durable.hash
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Hash_exn, key) ->
            value_res
            @@ Durable.hash_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Some_input (Write_value_exn, (edit_readonly, key, offset, value)) ->
            tree_res
            @@ Durable.write_value_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 offset
                 value
        | Some_input (Read_value_exn, (key, offset, len)) ->
            value_res
            @@ Durable.read_value_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 offset
                 len)
      init_durable
      operations
end
