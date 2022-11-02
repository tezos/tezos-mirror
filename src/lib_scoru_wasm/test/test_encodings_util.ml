(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary
include Tezos_tree_encoding

type Tezos_lazy_containers.Lazy_map.tree += Tree of Context.tree

module Tree = struct
  type t = Context.t

  type tree = Context.tree

  type key = Context.key

  type value = Context.value

  include Context.Tree

  let select = function
    | Tree t -> t
    | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

  let wrap t = Tree t
end

module Tree_encoding_runner = Tezos_tree_encoding.Runner.Make (Tree)

let empty_tree () =
  let open Lwt_syntax in
  let* index = Context.init "/tmp" in
  let empty_store = Context.empty index in
  return @@ Context.Tree.empty empty_store

let test_encode_decode enc value f =
  let open Lwt_result_syntax in
  let*! empty_tree = empty_tree () in
  let*! tree = Tree_encoding_runner.encode enc value empty_tree in
  let*! value' = Tree_encoding_runner.decode enc tree in
  f value'

let encode_decode enc value = test_encode_decode enc value Lwt.return

let qcheck ?count ?print gen f =
  let open Lwt_result_syntax in
  let test =
    QCheck2.Test.make ?count ?print gen (fun x ->
        Result.is_ok @@ Lwt_main.run (f x))
  in
  let res = QCheck_base_runner.run_tests ~verbose:true [test] in
  if res = 0 then return_unit else failwith "QCheck tests failed"

let make_test ?print encoding gen check () =
  qcheck ?print gen (fun value ->
      let open Lwt_result_syntax in
      let*! value' = encode_decode encoding value in
      let* res = check value value' in
      if res then return_unit else fail ())
