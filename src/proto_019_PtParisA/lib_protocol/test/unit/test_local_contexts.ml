(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 proof ninja, Inc <contact@proof-ninja.co.jp            *)
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
    Component:    Local context storages by functors
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/unit/main.exe \
                  -- --file test_local_contexts.ml
    Dependencies: helpers/block.ml
    Subject:      Tests for local contexts

*)
open Protocol

open Storage_functors
module A = Alpha_context

let create () =
  let open Lwt_result_syntax in
  let account = Account.new_account () in
  let bootstrap_account = Account.make_bootstrap_account account in
  let* alpha_ctxt = Block.alpha_context [bootstrap_account] in
  return @@ A.Internal_for_tests.to_raw alpha_ctxt

(* /a/b/c *)
let dir1 = ["a"; "b"; "c"]

module Sub =
  Make_subcontext (Registered) (Raw_context)
    (struct
      let name = dir1
    end)

module Index : INDEX with type t = string = struct
  type t = string

  let path_length = 1

  let to_path x l = x :: l

  let of_path = function [x] -> Some x | _ -> None

  type 'a ipath = 'a * t

  let args =
    Storage_description.One
      {
        rpc_arg = Environment.RPC_arg.string;
        encoding = Data_encoding.string;
        compare;
      }
end

module Indexed_context = Make_indexed_subcontext (Sub) (Index)

module Value : Storage_sigs.VALUE with type t = bytes = struct
  type t = bytes

  let encoding = Data_encoding.bytes
end

module C =
  Indexed_context.Make_map
    (Registered)
    (struct
      let name = ["name"]
    end)
    (Value)

let eq_context ctxt1 ctxt2 =
  let open Lwt_result_wrap_syntax in
  let hash ctxt =
    let+@ root = Raw_context.get_tree ctxt [] in
    Raw_context.Tree.hash root
  in
  let* x = hash ctxt1 in
  let* y = hash ctxt2 in
  Assert.equal
    ~loc:__LOC__
    Context_hash.equal
    "check context"
    Context_hash.pp
    x
    y

let write_with_local ctxt local_dir f =
  let open Lwt_result_syntax in
  let+ ctxt, () =
    Indexed_context.with_local_context ctxt local_dir (fun local ->
        let+ local = f local in
        (local, ()))
  in
  ctxt

let test_local_remove_existing () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = create () in
  let subdir = "foo" in
  let value = Bytes.of_string "ABCDE" in
  (* init *)
  let*@ ctxt1 =
    write_with_local ctxt subdir (fun local -> C.Local.init local value)
  in
  let*@ ctxt2 = C.init ctxt subdir value in
  let* () = eq_context ctxt1 ctxt2 in
  let ctxt = ctxt2 in
  (* remove_existing *)
  let*@ ctxt1 = write_with_local ctxt subdir C.Local.remove_existing in
  let*@ ctxt2 = C.remove_existing ctxt subdir in
  eq_context ctxt1 ctxt2

let tests =
  [
    Tztest.tztest
      "Local.remove_existing: check whether local access has the same behavior"
      `Quick
      test_local_remove_existing;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("local contexts", tests)]
  |> Lwt_main.run
