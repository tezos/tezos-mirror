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

open Tezos_test_helpers
open QCheck2
open Probability_utils

(* This module defines interface for generation of inputs
   for durable stress tests in Gen_lwt monad.
    Also it provides an implementation based on probabilities.
*)

type key = string list

type operation_generator = int Trie.t -> Durable_operation.t Gen.t

(* Weighted operations list:
   operations will be generated proportionally to their weight *)
type operations_distribution = (int * operation_generator) list

(* Bunch of helpers *)
let range (i : int) (j : int) =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let range_chars (i : char) (j : char) =
  List.map Char.chr @@ range (Char.code i) (Char.code j)

(* Generate a path in format /aaa/bbb/ccc *)
let gen_arbitrary_path ~(max_len : int) ~(max_segments_num : int)
    (alphabet : char Gen.t) : key Gen.t =
  (*
    And suffix = /x/y/zz, then:
    suffix_segments = ["x"; "y"; "zz"]
    segments_len = 1 + 1 + 2 = 4 (so only segments lengths counted).
  *)
  let open Gen in
  (* As each suffix segment is followed by /, hence,
     we can't have more than max_len / 2 segments *)
  let* suffix_segments = Gen.(1 -- Int.min max_segments_num (max_len / 2)) in
  (* The same about maximum total length of segments *)
  let max_segments_len = max_len / 2 in
  let* segments_len =
    Gen.frequencyl
    @@ Distributions.centered_distribution_l
         (range suffix_segments max_segments_len)
  in
  let* segments_lens =
    Gen.map Array.to_list
    @@ Gen.make_primitive
         ~gen:(QCheck.Gen.pos_split ~size:suffix_segments segments_len)
         ~shrink:(fun _ -> Seq.empty)
  in
  let+ suffix = Gen.string_size ~gen:alphabet (Gen.return segments_len) in
  let rec split_str s lens =
    match (s, lens) with
    | "", [] -> []
    | s, l :: lens ->
        String.sub s 0 l
        :: split_str (String.sub s l @@ (String.length s - l)) lens
    | _ -> assert false
  in
  split_str suffix segments_lens

module Key_generator_params = struct
  let key_alphabet : char Gen.t =
    Gen.oneofl
    @@ List.concat
         [
           ['.'; '-'; '_'];
           range_chars 'a' 'z';
           range_chars 'A' 'Z';
           range_chars '0' '9';
         ]

  let max_key_length : int = 250 - String.length "/durable" - String.length "/@"

  let max_suffix_len_of_nonexisting_key : int = 30

  let max_num_suffix_segments_of_nonexisting_key : int = 5
end

module Operation_probabilities = struct
  (* How often generated key has to exist in durable for all read operations *)
  let key_exists_in_read_operation = Probability.of_percent 50

  (* How often PREFIX of a generated key has to exist in a durable for all read operations.
     So basically this targets validity of underlying Trie implementation.
  *)
  let prefix_exists_in_read_operation = Probability.of_percent 20

  (* How often PREFIX of a generated key has to exist in durable for
     all other operations apart from read once *)
  let prefix_exists_in_operation = Probability.of_percent 10

  (* How often a generated key has to be read-only *)
  let key_to_be_readonly = Probability.of_percent 20

  (* How often a key_from generated for
     copy_tree/move_tree has to exist in a durable *)
  let key_from_exists = Probability.of_percent 70

  (* How often a key_to generated for
     copy_tree/move_tree has to exist in a durable *)
  let key_to_exists = Probability.of_percent 50

  (* How often a key generated for delete operation has to exist in durable *)
  let key_exists_in_delete = Probability.of_percent 40

  (* How often a key generated for set_value has to exist in durable *)
  let key_exists_in_set_value = Probability.of_percent 50

  (* How often a key generated for write_value has to exist in durable *)
  let key_exists_in_write_value = Probability.of_percent 50

  (* How often an offset generated for write_value has to be valid *)
  let valid_offset_in_write_value = Probability.of_percent 95

  (* How often an offset generated for read_value has to be valid *)
  let valid_offset_in_read_value = Probability.of_percent 95
end

open Durable_operation

let gen_existing_prefix ~(should_be_key : bool) ~(should_be_readonly : bool)
    (root : int Trie.t) : key Gen.t =
  let open Gen in
  let root =
    if should_be_readonly then
      WithExceptions.Option.get ~loc:__LOC__ @@ Trie.lookup ["readonly"] root
    else root
  in
  let size v =
    if should_be_key then v.Trie.keys_count else v.Trie.nodes_count
  in
  let+ index = Gen.int_range 0 (size root - 1) in
  (* This function enumerate nodes like
         0
        / \
       1   4
      / \
     2   3
     Basically it enumerate key-nodes and all node,
     and depending on should_be_key it looks for a node
     which corresponds to a target index.
  *)
  let rec find_index index rev_path t =
    let init_size =
      if (not should_be_key) || (should_be_key && Option.is_some t.Trie.value)
      then 1
      else 0
    in
    (* If index = 0 and init_size == 1, then we ended up in the needed node *)
    if index < init_size then List.rev rev_path
    else
      let node, prev_size =
        (* Find a subtree which contains a node with index *)
        List.fold_left
          (fun res (k, c) ->
            match res with
            | (Some _, _) as rs -> rs
            | None, prev_size ->
                let v_size = size c in
                if index < prev_size + v_size then (Some (k, c), prev_size)
                else (None, prev_size + v_size))
          (None, init_size)
        @@ Trie.Children.bindings t.children
      in
      let node = WithExceptions.Option.get ~loc:__LOC__ node in
      (* We recursively reduced task to smaller tree and smaller index *)
      find_index (index - prev_size) (fst node :: rev_path) (snd node)
  in
  List.append (if should_be_readonly then ["readonly"] else [])
  @@ find_index index [] root

let gen_bool p =
  Gen.map (fun x -> x < Probability.to_percent p) @@ Gen.int_range 0 100

(* Key generator.
   We might want to generate existing and non-existing keys
   with different probabilities for different operations.
   It takes current generator context and probability
   for a generated key and prefix to exist in the tree. *)
let gen_key ~(key_exists : Probability.t) ~(prefix_exists : Probability.t)
    (trie : int Trie.t) : key Gen.t =
  let open Gen in
  let open Key_generator_params in
  (* Just check that sum is less than 1.0 *)
  let _res_prob = Probability.(key_exists + prefix_exists) in
  let existing_key_gen =
    (* Check if there is at least one key in readonly subtree *)
    let read_only_exists =
      Option.is_some
      @@ Option.map (fun v -> v.Trie.keys_count > 0)
      @@ Trie.lookup ["readonly"] trie
    in
    let* p_readonly = gen_bool Operation_probabilities.key_to_be_readonly in
    if read_only_exists && p_readonly then
      (* Generate readonly key *)
      gen_existing_prefix ~should_be_key:true ~should_be_readonly:true trie
    else
      (* Generate write key,
         strictly speaking still possible to generate readonly key *)
      gen_existing_prefix ~should_be_key:true ~should_be_readonly:false trie
  in
  let existing_prefix_gen =
    let read_only_exists =
      (* Check if there is at least one node in readonly subtree *)
      Option.is_some
      @@ Option.map (fun v -> v.Trie.nodes_count > 0)
      @@ Trie.lookup ["readonly"] trie
    in
    let* p_readonly = gen_bool Operation_probabilities.key_to_be_readonly in
    let* existing_prefix =
      if read_only_exists && p_readonly then
        gen_existing_prefix ~should_be_key:false ~should_be_readonly:true trie
      else
        (* Generate write prefix,
           strictly speaking still possible to generate readonly prefix *)
        gen_existing_prefix ~should_be_key:false ~should_be_readonly:false trie
    in
    let remain_len =
      max_key_length - Durable_operation.key_len existing_prefix
    in
    let+ suffix =
      gen_arbitrary_path
        ~max_len:(Int.min remain_len max_suffix_len_of_nonexisting_key)
        ~max_segments_num:max_num_suffix_segments_of_nonexisting_key
        key_alphabet
    in
    List.append existing_prefix suffix
  in
  let non_existing_key_gen =
    let* p_readonly = gen_bool Operation_probabilities.key_to_be_readonly in
    let pref = if p_readonly then ["readonly"] else [] in
    let remain_len = max_key_length - Durable_operation.key_len pref in
    let+ suffix =
      gen_arbitrary_path ~max_len:remain_len ~max_segments_num:20 key_alphabet
    in
    List.append pref suffix
  in
  let trie_is_empty = Trie.subtrees_size [] trie = 0 in
  if trie_is_empty then non_existing_key_gen
  else
    let key_exists = Probability.to_percent key_exists in
    let prefix_exists = Probability.to_percent prefix_exists in
    let probability_rest = 100 - key_exists - prefix_exists in
    Gen.frequency
      [
        (key_exists, existing_key_gen);
        (prefix_exists, existing_prefix_gen);
        (probability_rest, non_existing_key_gen);
      ]

let value_len trie key = Option.value ~default:0 @@ Trie.get_value key trie

let gen_find_value trie : Durable_operation.t Gen.t =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Find_value, key)

let gen_find_value_exn trie : Durable_operation.t Gen.t =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Find_value_exn, key)

let gen_set_value_exn trie : Durable_operation.t Gen.t =
  let open Gen in
  let* edit_readonly = Gen.bool in
  let* value = Gen.string_of Gen.char in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_set_value
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Set_value_exn, (edit_readonly, key, value))

let gen_copy_tree_exn trie =
  let open Gen in
  let* edit_readonly = Gen.bool in
  let* key_from =
    gen_key
      ~key_exists:Operation_probabilities.key_from_exists
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  let+ key_to =
    gen_key
      ~key_exists:Operation_probabilities.key_to_exists
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Copy_tree_exn, (edit_readonly, key_from, key_to))

let gen_move_tree_exn trie =
  let open Gen in
  let* key_from =
    gen_key
      ~key_exists:Operation_probabilities.key_from_exists
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  let+ key_to =
    gen_key
      ~key_exists:Operation_probabilities.key_to_exists
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Move_tree_exn, (key_from, key_to))

let gen_delete trie =
  let open Gen in
  let* edit_readonly = Gen.bool in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_delete
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Delete, (edit_readonly, key))

let gen_list trie =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (List, key)

let gen_count_subtrees trie =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Count_subtrees, key)

let gen_subtree_name_at trie =
  let open Gen in
  let* key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  let subtrees_size = Trie.subtrees_size key trie in
  let+ subtree_id = Gen.int_range (-3) (subtrees_size + 3) in
  Operation (Substree_name_at, (key, subtree_id))

let gen_hash trie =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Hash, key)

let gen_hash_exn trie =
  let open Gen in
  let+ key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  Operation (Hash_exn, key)

let gen_write_value_exn trie =
  let open Gen in
  let* key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_write_value
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  let* edit_readonly = Gen.bool in
  let value_len = value_len trie key in
  let* is_valid_offset =
    gen_bool Operation_probabilities.valid_offset_in_write_value
  in
  let+ arguments =
    if not is_valid_offset then
      (* Invalid offset *)
      let* offset = Gen.int_range (value_len + 1) (value_len + 100) in
      let+ value = Gen.string_of Gen.char in
      (edit_readonly, key, Int64.of_int offset, value)
    else
      (* max_store_io_size = 2048 *)
      let* value = Gen.string_size ~gen:Gen.char (Gen.int_bound 2048) in
      let+ offset = Gen.int_bound (value_len + 1) in
      (edit_readonly, key, Int64.of_int offset, value)
  in
  Operation (Write_value_exn, arguments)

let gen_read_value_exn trie =
  let open Gen in
  let* key =
    gen_key
      ~key_exists:Operation_probabilities.key_exists_in_read_operation
      ~prefix_exists:Operation_probabilities.prefix_exists_in_operation
      trie
  in
  let value_len = value_len trie key in
  let* is_valid_offset =
    gen_bool Operation_probabilities.valid_offset_in_read_value
  in
  let+ arguments =
    if not is_valid_offset then
      (* Invalid offset *)
      let* offset = Gen.int_range (value_len + 1) (value_len + 100) in
      let+ len = Gen.int in
      (key, Int64.of_int offset, Int64.of_int len)
    else
      let* len = Gen.int_bound (value_len + 100) in
      let+ offset = Gen.int_bound (value_len + 1) in
      (key, Int64.of_int offset, Int64.of_int len)
  in
  Operation (Read_value_exn, arguments)

(* Groups of operation generators *)
let write_operations : operation_generator list =
  [gen_write_value_exn; gen_set_value_exn]

let read_operations : operation_generator list =
  [gen_find_value; gen_read_value_exn; gen_find_value_exn]

let structure_inspection_operations : operation_generator list =
  [gen_hash; gen_list; gen_count_subtrees; gen_subtree_name_at; gen_hash_exn]

let structure_modification_operations : operation_generator list =
  [gen_delete; gen_copy_tree_exn; gen_move_tree_exn]

let all_operations : operation_generator list =
  let all =
    List.concat
      [
        write_operations;
        read_operations;
        structure_modification_operations;
        structure_inspection_operations;
      ]
  in
  Assert.Int.equal
    ~loc:__LOC__
    ~msg:"Not exhaust list of durable operations"
    (List.length all)
    13 ;
  all
