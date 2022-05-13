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
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/main.exe \
                -- test "storage tests"
    Subject:    Test storage functions.
 *)

open Protocol
open Storage_functors

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

let tests =
  [
    Tztest.tztest
      "fold_keys_unaccounted smoke test"
      `Quick
      test_fold_keys_unaccounted;
  ]
