(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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
    Component:  Context Storage
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/main.exe \
                -- --file test_storage_functions.ml
    Subject:    Test storage functions.
 *)

open Protocol
open Storage_functors

let assert_length ~loc ctxt key expected =
  let open Lwt_result_syntax in
  let*! length = Raw_context.length ctxt key in
  let* () = Assert.equal_int ~loc length expected in
  let*! list = Raw_context.list ctxt key in
  let list_length = List.length list in
  Assert.equal_int ~loc length list_length

module Int32 = struct
  type t = int32

  let encoding = Data_encoding.int32

  module Index = struct
    type t = int

    let path_length = 1

    let to_path c l = string_of_int c :: l

    let of_path = function
      | [] | _ :: _ :: _ -> None
      | [c] -> int_of_string_opt c

    type 'a ipath = 'a * t

    let args =
      Storage_description.One
        {
          rpc_arg = Environment.RPC_arg.int;
          encoding = Data_encoding.int31;
          compare = Compare.Int.compare;
        }
  end
end

module String = struct
  type t = string

  let encoding = Data_encoding.string
end

module Root_raw_context =
  Make_subcontext (Registered) (Raw_context)
    (struct
      let name = ["test_storage_functors"]
    end)

module Indexed_context =
  Make_indexed_subcontext
    (Make_subcontext (Registered) (Root_raw_context)
       (struct
         let name = ["index"]
       end))
       (Int32.Index)

module Table =
  Make_carbonated_data_set_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["table"]
       end))
       (Int32.Index)

let wrap m = m >|= Environment.wrap_tzresult

(** Test:
     This test checks that it is possible to add values to a
     Carbonated_data_set_storage and iterate over them. *)
let test_fold_keys_unaccounted () =
  let open Lwt_result_syntax in
  let* ctxt = Context.default_raw_context () in
  let* ctxt, _ = wrap (Table.init ctxt 1) in
  let* ctxt, _ = wrap (Table.init ctxt 2) in
  let*! items =
    Table.fold_keys_unaccounted
      ctxt
      ~order:`Undefined
      ~f:(fun x acc -> Lwt.return @@ (x :: acc))
      ~init:[]
  in
  let items = List.sort Compare.Int.compare items in
  Assert.assert_equal_list
    ~loc:__LOC__
    Int.equal
    "Compare items"
    Format.pp_print_int
    [1; 2]
    items

(** Test that [length] returns the number of elements for a given path. *)
let test_length () =
  let open Lwt_result_syntax in
  let* ctxt = Context.default_raw_context () in
  (* Add a tree to the context:
     root:
       left:
         l1 : V1
         l2 : V2
         l3 : V3
       right:
         r1 : V4
         r2 : V5
       file : V6
  *)
  let*! tree =
    let*! tree_left =
      let tree = Raw_context.Tree.empty ctxt in
      let*! tree = Raw_context.Tree.add tree ["l1"] (Bytes.of_string "V1") in
      let*! tree = Raw_context.Tree.add tree ["l2"] (Bytes.of_string "V2") in
      Raw_context.Tree.add tree ["c"] (Bytes.of_string "V3")
    in
    let*! tree_right =
      let tree = Raw_context.Tree.empty ctxt in
      let*! tree = Raw_context.Tree.add tree ["r1"] (Bytes.of_string "V4") in
      Raw_context.Tree.add tree ["r2"] (Bytes.of_string "V5")
    in
    let tree = Raw_context.Tree.empty ctxt in
    let*! tree = Raw_context.Tree.add_tree tree ["left"] tree_left in
    let*! tree = Raw_context.Tree.add_tree tree ["right"] tree_right in
    Raw_context.Tree.add tree ["file"] (Bytes.of_string "V6")
  in
  let* ctxt = wrap @@ Raw_context.init_tree ctxt ["root"] tree in
  (* The root node contains 3 elements. *)
  let* () = assert_length ctxt ~loc:__LOC__ ["root"] 3 in
  (* The left branch contains 3 elements. *)
  let* () = assert_length ctxt ~loc:__LOC__ ["root"; "left"] 3 in
  (* The right branch contains 2 elements. *)
  let* () = assert_length ctxt ~loc:__LOC__ ["root"; "right"] 2 in
  (* Path [root/left/l1] is a leaf and thus returns length 0. *)
  let* () = assert_length ctxt ~loc:__LOC__ ["root"; "left"; "l1"] 0 in
  (* The length of a non-existing path also returns 0. *)
  assert_length ctxt ~loc:__LOC__ ["root"; "right"; "non_existing"] 0

let tests =
  [
    Tztest.tztest
      "fold_keys_unaccounted smoke test"
      `Quick
      test_fold_keys_unaccounted;
    Tztest.tztest "length test" `Quick test_length;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("storage tests", tests)]
  |> Lwt_main.run
