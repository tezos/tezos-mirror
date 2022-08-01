(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
open Lazy_containers

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary

module Tree : Tree_encoding.TREE with type tree = Context.tree = struct
  type tree = Context.tree

  include Context.Tree
end

module Map =
  Lazy_map.Make
    (Lazy_map.Effect.Lwt)
    (struct
      type t = string

      let compare = String.compare

      let to_string x = x
    end)

module Tree_encoding = struct
  include Tree_encoding.Make (Tree)
  include Lazy_vector_encoding.Int
  include Lazy_map_encoding.Make (Map)
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
  let enc = contact_enc ~default:No_address () in
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

let test_lazy_vector () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc =
    lazy_vector (value [] Data_encoding.int31) (value [] Data_encoding.string)
  in
  let vector = Lazy_vector.LwtIntVector.create 100 in
  (* Load the key [K1] from the vector . *)
  let vector = Lazy_vector.LwtIntVector.set 42 "42" vector in
  let*! value = Lazy_vector.LwtIntVector.get 42 vector in
  assert (value = "42") ;
  let*! decoded_vector = encode_decode enc vector in
  (* Load the key [42] from the decoded vector. *)
  let*! value = Lazy_vector.LwtIntVector.get 42 decoded_vector in
  assert (value = "42") ;
  assert (
    Lazy_vector.LwtIntVector.to_string Fun.id vector
    = Lazy_vector.LwtIntVector.to_string Fun.id decoded_vector) ;
  return_unit

let test_chunked_byte_vector () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let vector =
    Chunked_byte_vector.Lwt.of_string
      (String.make 10_000 'a' ^ String.make 10_000 'b')
  in
  let*! value = Chunked_byte_vector.Lwt.load_byte vector 5L in
  assert (Char.chr value = 'a') ;
  let*! value = Chunked_byte_vector.Lwt.load_byte vector 10_005L in
  assert (Char.chr value = 'b') ;
  let*! decoded_vector = encode_decode chunked_byte_vector vector in
  let*! value = Chunked_byte_vector.Lwt.load_byte decoded_vector 5L in
  assert (Char.chr value = 'a') ;
  let*! value = Chunked_byte_vector.Lwt.load_byte decoded_vector 10_005L in
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
  return_unit

let test_value_option () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc = value_option [] Data_encoding.int31 in
  let* () = assert_round_trip enc (Some 1) Stdlib.( = ) in
  let* () = assert_round_trip enc None Stdlib.( = ) in
  return_unit

let test_value_default () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let*! tree = empty_tree () in
  let enc = value ~default:42 [] Data_encoding.int31 in
  assert_value tree enc 42

let test_optional () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let key = [] in
  let enc = optional key Data_encoding.int31 in
  let*! tree = empty_tree () in
  let*! tree = Tree_encoding.encode enc (Some 0) tree in
  let* () = assert_value tree enc (Some 0) in
  let*! tree = Tree_encoding.encode enc None tree in
  let* () = assert_missing_value tree key in
  return_unit

type cyclic = {name : string; self : unit -> cyclic}

let test_with_self_ref () =
  let open Tree_encoding in
  let open Lwt_result_syntax in
  let enc () =
    with_self_reference (fun cycle ->
        conv
          (fun name -> {name; self = (fun () -> Lazy.force cycle)})
          (fun {name; _} -> name)
          (value [] Data_encoding.string))
  in
  (* A cycle is a value with a (lazy) self-reference. *)
  let rec cycle = {name = "Cycle"; self = (fun () -> cycle)} in
  (* Encode using an encoder and an empty tree. *)
  let*! empty_tree = empty_tree () in
  let*! tree = Tree_encoding.encode (enc ()) cycle empty_tree in
  (* Decode using a new encoder value and the tree from above. *)
  let*! ({name; self} as cycle) = Tree_encoding.decode (enc ()) tree in
  assert (name = "Cycle") ;
  assert (cycle == self ()) ;
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
    tztest "Lazy vector" `Quick test_lazy_vector;
    tztest "Chunked byte vector" `Quick test_chunked_byte_vector;
    tztest "Tuples" `Quick test_tuples;
    tztest "Option" `Quick test_option;
    tztest "Value Option" `Quick test_value_option;
    tztest "Value ~default" `Quick test_value_default;
    tztest "Optional" `Quick test_optional;
    tztest "Self ref" `Quick test_with_self_ref;
  ]
