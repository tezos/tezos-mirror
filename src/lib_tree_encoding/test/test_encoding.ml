(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** Testing
    -------
    Component:    Tree_encoding
    Invocation:   dune exec src/lib_tree_encoding/test/test_tree_encoding.exe \
                  -- test "^Encodings$"
    Subject:      Encoding tests for the tree-encoding library
*)

open Tztest
open Tezos_lazy_containers

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary
module E = Tezos_tree_encoding.Encodings_util.Make (Context)
module Tree = E.Tree

module Map = Lazy_map.Make (struct
  type t = string

  let compare = String.compare

  let to_string x = x
end)

module Tree_encoding = struct
  include Tezos_tree_encoding
  include Lazy_map_encoding.Make (Map)
  include Tezos_tree_encoding.Runner.Make (Tree)
  module Wrapped_runner =
    Tezos_tree_encoding.Runner.Make (Tezos_tree_encoding.Wrapped)
end

let empty_tree () =
  let open Lwt_syntax in
  let* index = Context.init "/tmp" in
  let empty_store = Context.empty index in
  return @@ Context.Tree.empty empty_store

let test_encode_decode enc value f =
  let open Lwt_result_syntax in
  let*! empty_tree = empty_tree () in
  let*! tree = Tree_encoding.encode enc value empty_tree in
  let*! value' = Tree_encoding.decode enc tree in
  f value'

let test_decode_encode_decode tree enc f =
  let open Lwt_syntax in
  let* value = Tree_encoding.decode enc tree in
  let* tree = Tree_encoding.encode enc value tree in
  let* value' = Tree_encoding.decode enc tree in
  f value value'

let encode_decode enc value = test_encode_decode enc value Lwt.return

let decode_encode_decode tree enc =
  test_decode_encode_decode tree enc (fun x y -> Lwt.return (x, y))

let assert_value tree enc v =
  let open Lwt_result_syntax in
  let*! v' = Tree_encoding.decode enc tree in
  assert (v = v') ;
  return_unit

let assert_missing_value tree key =
  let open Lwt_result_syntax in
  let*! candidate = Tree.find tree key in
  match candidate with
  | None -> return_unit
  | Some _ -> failwith "value should be missing"

let assert_round_trip enc value equal =
  let open Lwt_syntax in
  let* value' = encode_decode enc value in
  let open Lwt_result_syntax in
  assert (equal value' value) ;
  return_unit

let assert_decode_round_trip tree enc equal =
  let open Lwt_result_syntax in
  let*! value, value' = decode_encode_decode tree enc in
  assert (equal value' value) ;
  return_unit

let test_string () =
  let enc = Tree_encoding.value ["key"] Data_encoding.string in
  assert_round_trip enc "Hello" String.equal

let test_int () =
  let enc = Tree_encoding.value ["key"] Data_encoding.int32 in
  assert_round_trip enc 42l Int32.equal

let test_tree () =
  let enc =
    Tree_encoding.scope ["foo"]
    @@ Tree_encoding.value ["key"] Data_encoding.int32
  in
  assert_round_trip enc 42l Int32.equal

let test_raw () =
  let enc = Tree_encoding.raw ["key"] in
  assert_round_trip enc (Bytes.of_string "CAFEBABE") Bytes.equal

let test_conv () =
  let open Tree_encoding in
  let enc =
    conv int_of_string string_of_int (value ["key"] Data_encoding.string)
  in
  assert_round_trip enc 42 Int.equal

type contact =
  | Email of string
  | Address of {street : string; number : int}
  | No_address

let contact_enc ?default () =
  let open Tree_encoding in
  tagged_union
    ?default
    (value [] Data_encoding.string)
    [
      case
        "Email"
        (value [] Data_encoding.string)
        (function Email s -> Some s | _ -> None)
        (fun s -> Email s);
      case
        "Address"
        (tup2
           ~flatten:false
           (value ["street"] Data_encoding.string)
           (value ["number"] Data_encoding.int31))
        (function
          | Address {street; number} -> Some (street, number) | _ -> None)
        (fun (street, number) -> Address {street; number});
      case
        "No Address"
        (value [] Data_encoding.unit)
        (function No_address -> Some () | _ -> None)
        (fun () -> No_address);
    ]

let test_tagged_union () =
  let open Lwt_result_syntax in
  let enc = contact_enc () in
  let* () = assert_round_trip enc No_address Stdlib.( = ) in
  let* () = assert_round_trip enc (Email "foo@bar.com") Stdlib.( = ) in
  let* () =
    assert_round_trip
      enc
      (Address {street = "Main Street"; number = 10})
      Stdlib.( = )
  in
  return_unit

let test_tagged_union_default () =
  let open Lwt_result_syntax in
  let enc = contact_enc ~default:(fun () -> No_address) () in
  let*! empty_tree = empty_tree () in
  let* () = assert_value empty_tree enc No_address in
  let* () = assert_round_trip enc No_address Stdlib.( = ) in
  let* () =
    assert_round_trip
      enc
      (Address {street = "Main Street"; number = 10})
      Stdlib.( = )
  in
  return_unit

let test_lazy_mapping () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc = lazy_map (value ["key"] Data_encoding.string) in
  let map = Map.create () in
  let key = "key" in
  let value = "value" in
  let map = Map.set key value map in
  (* Load the [key] from the map *)
  let*! value' = Map.get key map in
  assert (value' = value) ;
  let*! decoded_map = encode_decode enc map in
  (* Load the [key] from the decoded map. *)
  let*! value' = Map.get key decoded_map in
  assert (value' = value) ;
  assert (Map.to_string Fun.id map = Map.to_string Fun.id decoded_map) ;
  return_unit

let test_add_to_decoded_empty_map () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc = lazy_map (value ["key"] Data_encoding.string) in
  let map = Map.create () in
  let*! decoded_map1 = encode_decode enc map in
  let map = Map.set "key" "value" decoded_map1 in
  let*! decoded_map2 = encode_decode enc map in
  let*! value' = Map.get "key" decoded_map2 in
  assert (value' = "value") ;
  return_unit

let test_wrapped_tree () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  (* helpers *)
  let val_enc key = Tree_encoding.value key Data_encoding.int32 in
  let tree_enc = Tree_encoding.scope ["foo"] @@ Tree_encoding.wrapped_tree in
  let assert_none key tree =
    let*! value =
      decode (Tree_encoding.value_option key Data_encoding.int32) tree
    in
    assert (value = None) ;
    return_unit
  in
  let assert_some key expected tree =
    let*! value =
      decode (Tree_encoding.value_option key Data_encoding.int32) tree
    in
    assert (value = Some expected) ;
    return_unit
  in
  (* test *)
  let*! tree = empty_tree () in
  let*! tree = encode (val_enc ["foo"; "bar"; "key"]) 42l tree in
  let*! sub_tree = decode tree_enc tree in
  let*! key_value = Wrapped.find_tree sub_tree ["bar"; "key"] in
  let*! key_value =
    match key_value with
    | Some v -> Wrapped_runner.decode (val_enc []) v
    | None -> assert false
  in
  assert (key_value = 42l) ;
  let*! sub_tree = sub_tree |> Wrapped_runner.encode (val_enc ["baz"]) 41l in
  let*! sub_tree = Wrapped.remove sub_tree ["bar"; "key"] in
  let*! tree = encode tree_enc sub_tree tree in
  let* () = assert_some ["foo"; "baz"] 41l tree in
  let* () = assert_none ["foo"; "bar"; "key"] tree in
  (* Ensure removing another key is encoded correctly *)
  let*! sub_tree = decode tree_enc tree in
  let*! sub_tree = Wrapped.remove sub_tree ["baz"] in
  let*! tree = encode tree_enc sub_tree tree in
  let* () = assert_none ["foo"; "baz"] tree in
  return_unit

let test_lazy_vector () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc =
    int_lazy_vector
      (value [] Data_encoding.int31)
      (value [] Data_encoding.string)
  in
  let vector = Lazy_vector.IntVector.create 100 in
  (* Load the key [K1] from the vector . *)
  let vector = Lazy_vector.IntVector.set 42 "42" vector in
  let*! value = Lazy_vector.IntVector.get 42 vector in
  assert (value = "42") ;
  let*! decoded_vector = encode_decode enc vector in
  (* Load the key [42] from the decoded vector. *)
  let*! value = Lazy_vector.IntVector.get 42 decoded_vector in
  assert (value = "42") ;
  assert (
    Lazy_vector.IntVector.to_string Fun.id vector
    = Lazy_vector.IntVector.to_string Fun.id decoded_vector) ;
  return_unit

let test_lazy_vector_pop () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let int_vec_enc =
    int_lazy_vector
      (value [] Data_encoding.int31)
      (value [] Data_encoding.int31)
  in
  let*! tree = empty_tree () in
  let assert_value_at_index ~ix tree expected =
    let key = string_of_int ix in
    let*! value = Tree.find tree ["contents"; key] in
    assert (value = expected) ;
    return_unit
  in
  let vec = Tezos_lazy_containers.Lazy_vector.IntVector.of_list [0; 1; 2; 3] in
  (* Encode the lazy vector to the tree. *)
  let*! tree = encode int_vec_enc vec tree in
  let* () =
    assert_value_at_index ~ix:0 tree (Some (Bytes.of_string "\000\000\000\000"))
  in
  (* Decode the lazy vector from the tree and check the first value. *)
  let*! vec = decode int_vec_enc tree in
  let*! first_value, vec =
    Tezos_lazy_containers.Lazy_vector.IntVector.pop vec
  in
  assert (first_value = 0) ;
  (* Encode the lazy vector to the tree again and check the value in the tree. *)
  let*! tree = encode int_vec_enc vec tree in
  let* () = assert_value_at_index ~ix:0 tree None in
  (* Decode the lazy vector and check its first value. *)
  let*! vec = decode int_vec_enc tree in
  let*! first_value, _vec =
    Tezos_lazy_containers.Lazy_vector.IntVector.pop vec
  in
  assert (first_value = 1) ;
  return_unit

let test_chunked_byte_vector () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let vector =
    Chunked_byte_vector.of_string
      (String.make 10_000 'a' ^ String.make 10_000 'b')
  in
  let*! value = Chunked_byte_vector.load_byte vector 5L in
  assert (Char.chr value = 'a') ;
  let*! value = Chunked_byte_vector.load_byte vector 10_005L in
  assert (Char.chr value = 'b') ;
  let*! decoded_vector = encode_decode chunked_byte_vector vector in
  let*! value = Chunked_byte_vector.load_byte decoded_vector 5L in
  assert (Char.chr value = 'a') ;
  let*! value = Chunked_byte_vector.load_byte decoded_vector 10_005L in
  assert (Char.chr value = 'b') ;
  return_unit

let test_tuples () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let int = value ["my_int"] Data_encoding.int31 in
  let* () =
    assert_round_trip (tup2 ~flatten:false int int) (1, 2) Stdlib.( = )
  in
  let* () =
    assert_round_trip (tup3 ~flatten:false int int int) (1, 2, 3) Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup4 ~flatten:false int int int int)
      (1, 2, 3, 4)
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup5 ~flatten:false int int int int int)
      (1, 2, 3, 4, 5)
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup6 ~flatten:false int int int int int int)
      (1, 2, 3, 4, 5, 6)
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup7 ~flatten:false int int int int int int int)
      (1, 2, 3, 4, 5, 6, 7)
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup8 ~flatten:false int int int int int int int int)
      (1, 2, 3, 4, 5, 6, 7, 8)
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup2
         ~flatten:false
         (tup2 ~flatten:false int int)
         (tup2 ~flatten:false int int))
      ((1, 2), (3, 4))
      Stdlib.( = )
  in
  let* () =
    assert_round_trip
      (tup9 ~flatten:false int int int int int int int int int)
      (1, 2, 3, 4, 5, 6, 7, 8, 9)
      Stdlib.( = )
  in
  (* Without flatten we override the element since the tree [int]s are all
     stored under the same key [my_int]. *)
  let*! t3 = encode_decode (tup3 ~flatten:true int int int) (1, 2, 3) in
  assert (t3 = (3, 3, 3)) ;
  (* If we wrap the encoders manually we can use flatten to avoid and extra
     layer. *)
  let* () =
    assert_round_trip
      (tup3 ~flatten:true (scope ["A"] int) (scope ["B"] int) (scope ["C"] int))
      (1, 2, 3)
      Stdlib.( = )
  in
  return_unit

let test_option () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let int = value [] Data_encoding.int31 in
  let enc = option (tup2 ~flatten:false int int) in
  let* () = assert_round_trip enc (Some (1, 2)) Stdlib.( = ) in
  let* () = assert_round_trip enc None Stdlib.( = ) in
  (* Check that we can decode a [None] value from an empty tree. *)
  let*! empty_tree = empty_tree () in
  let*! opt = decode enc empty_tree in
  assert (Option.is_none opt) ;
  return_unit

let test_value_default () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let*! tree = empty_tree () in
  let enc = value ~default:42 [] Data_encoding.int31 in
  assert_value tree enc 42

let test_value_option () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let key = [] in
  let enc = value_option key Data_encoding.int31 in
  let*! tree = empty_tree () in
  let*! tree = Tree_encoding.encode enc (Some 0) tree in
  let* () = assert_value tree enc (Some 0) in
  let*! tree = Tree_encoding.encode enc None tree in
  let* () = assert_missing_value tree key in
  return_unit

type cyclic = {name : string; self : unit -> cyclic}

let test_delayed () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let count = ref 0 in
  let enc =
    delayed @@ fun () ->
    incr count ;
    value ~default:"Default" [] Data_encoding.string
  in
  (* Make sure that the function has not been evaluated. *)
  assert (!count = 0) ;
  (* Make sure that decoding works without encoding. *)
  let*! empty_tree = empty_tree () in
  let*! v = decode enc empty_tree in
  assert (v = "Default") ;
  (* Test round-trip. *)
  let*! v = encode_decode enc "Hello" in
  assert (v = "Hello") ;
  (* Make sure the the enc function was only evaluated once. *)
  assert (!count = 1) ;
  return_unit

let test_return () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc = Tree_encoding.return "K" in
  (* Make sure we can decode from an empty tree. *)
  let*! empty_tree = empty_tree () in
  let*! v = decode enc empty_tree in
  assert (v = "K") ;
  (* Make sure that round trip ignores the value. *)
  let*! v = encode_decode enc "Ignored value" in
  assert (v = "K") ;
  return_unit

let test_swap_maps () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let int_map_enc = lazy_map (value [] Data_encoding.int31) in
  let enc = tup2 ~flatten:false int_map_enc int_map_enc in
  let*! tree = empty_tree () in
  let assert_value_at_index ~key vec expected =
    let*! value = Map.get key vec in
    assert (value = expected) ;
    return_unit
  in
  (* Create a pair of maps. *)
  let map1 = Map.(create () |> set "foo" 1) in
  let map2 = Map.(create () |> set "bar" 2) in
  let pair = (map1, map2) in
  (* Encode the lazy maps to the tree. *)
  let*! tree = encode enc pair tree in
  (* Decode the maps. *)
  let*! pair = decode enc tree in
  (* Encode a new pair where the elements have been swapped. *)
  let swapped_pair = (snd pair, fst pair) in
  let*! tree = encode enc swapped_pair tree in
  (* Decode the swapped version. *)
  let*! swapped_pair = decode enc tree in
  (* Check that it's possible to access the elements of both maps. *)
  let* () = assert_value_at_index ~key:"foo" (snd swapped_pair) 1 in
  let* () = assert_value_at_index ~key:"bar" (fst swapped_pair) 2 in
  return_unit

let test_swap_vectors () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let int_vec_enc =
    int_lazy_vector
      (value [] Data_encoding.int31)
      (value [] Data_encoding.int31)
  in
  let enc = tup2 ~flatten:false int_vec_enc int_vec_enc in
  let*! tree = empty_tree () in
  let assert_value_at_index ~ix vec expected =
    let*! value = Tezos_lazy_containers.Lazy_vector.IntVector.get ix vec in
    assert (value = expected) ;
    return_unit
  in
  (* Create a pair of vectors. *)
  let vec_pair =
    ( Tezos_lazy_containers.Lazy_vector.IntVector.create
        ~produce_value:(fun ix -> Lwt.return ix)
        10,
      Tezos_lazy_containers.Lazy_vector.IntVector.create
        ~produce_value:(fun ix -> Lwt.return (100 + ix))
        10 )
  in
  (* Check elements/force evaluation of one element from each vector. *)
  let* () = assert_value_at_index ~ix:1 (fst vec_pair) 1 in
  let* () = assert_value_at_index ~ix:2 (snd vec_pair) 102 in
  (* Encode the lazy vector to the tree. *)
  let*! tree = encode enc vec_pair tree in
  (* Decode the vector. *)
  let*! vec_pair = decode enc tree in
  (* Encode a new pair where the elements have been swapped. *)
  let swapped_vec_pair = (snd vec_pair, fst vec_pair) in
  let*! tree = encode enc swapped_vec_pair tree in
  (* Decode the swapped version. *)
  let*! swapped_vec_pair = decode enc tree in
  (* Check that it's possible to access the elements of both vectors. *)
  let* () = assert_value_at_index ~ix:1 (snd swapped_vec_pair) 1 in
  let* () = assert_value_at_index ~ix:2 (fst swapped_vec_pair) 102 in
  return_unit

let tests =
  [
    tztest "String" `Quick test_string;
    tztest "Int" `Quick test_int;
    tztest "Tree" `Quick test_tree;
    tztest "Raw" `Quick test_raw;
    tztest "Convert" `Quick test_conv;
    tztest "Tagged-union" `Quick test_tagged_union;
    tztest "Tagged-union ~default" `Quick test_tagged_union_default;
    tztest "Lazy mapping" `Quick test_lazy_mapping;
    tztest
      "Add element to decoded empty map"
      `Quick
      test_add_to_decoded_empty_map;
    tztest "Lazy tree" `Quick test_wrapped_tree;
    tztest "Lazy vector" `Quick test_lazy_vector;
    tztest "Lazy vector pop" `Quick test_lazy_vector_pop;
    tztest "Chunked byte vector" `Quick test_chunked_byte_vector;
    tztest "Tuples" `Quick test_tuples;
    tztest "Option" `Quick test_option;
    tztest "Value ~default" `Quick test_value_default;
    tztest "Value-option" `Quick test_value_option;
    tztest "Delayed" `Quick test_delayed;
    tztest "Return" `Quick test_return;
    tztest "Swap maps" `Quick test_swap_maps;
    tztest "Swap vectors" `Quick test_swap_vectors;
  ]
