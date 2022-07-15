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

(** Testing
    -------
    Component:    Tree_encoding_decoding
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Parser encodings$"
    Subject:      Parser encoding tests for the tezos-scoru-wasm library
*)

open Tztest
open Lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary

module Tree :
  Tezos_context_sigs.Context.TREE
    with type t = Context.t
     and type tree = Context.tree
     and type key = string list
     and type value = bytes = struct
  type t = Context.t

  type tree = Context.tree

  type key = Context.key

  type value = Context.value

  include Context.Tree
end

type Lazy_containers.Lazy_map.tree += Tree of Tree.tree

module Tree_encoding = struct
  include Tree_encoding.Make (struct
    include Tree

    let select = function
      | Tree t -> t
      | _ -> raise Tree_encoding.Incorrect_tree_type

    let wrap t = Tree t
  end)

  include Lazy_map_encoding.Make (Instance.NameMap)
end

module Parser = Binary_parser_encodings.Make (Tree_encoding)

module Utils = struct
  module C = Chunked_byte_vector.Lwt

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

  let encode_decode enc value = test_encode_decode enc value Lwt.return

  let make_test encoding gen check () =
    Test_wasm_encoding.qcheck gen (fun value ->
        let open Lwt_result_syntax in
        let*! value' = encode_decode encoding value in
        let* res = check value value' in
        (* TODO: a better error reporting could be useful. *)
        if res then return_unit else fail ())
end

module Byte_vector = struct
  include Utils

  let gen_buffer =
    let open QCheck2.Gen in
    let* buffer = Ast_generators.data_label_gen in
    let* length = int64 in
    let+ offset = int64 in
    (buffer, offset, length)

  let gen =
    let open QCheck2.Gen in
    let start = return Decode.VKStart in
    let read =
      let+ buffer, offset, length = gen_buffer in
      Decode.VKRead (buffer, offset, length)
    in
    let stop =
      let+ vec = Ast_generators.data_label_gen in
      Decode.VKStop vec
    in
    oneof [start; read; stop]

  let check_buffer (buffer, offset, length) (buffer', offset', length') =
    let open Lwt_result_syntax in
    return
      (buffer = buffer' && Int64.equal offset offset'
     && Int64.equal length length')

  let check bv bv' =
    match (bv, bv') with
    | Decode.VKStart, Decode.VKStart -> Lwt.return_ok true
    | VKRead (buffer, offset, length), VKRead (buffer', offset', length') ->
        check_buffer (buffer, offset, length) (buffer', offset', length')
    | VKStop label, VKStop label' -> Lwt.return_ok (label = label')
    | _, _ -> Lwt.return_ok false

  let tests =
    tztest
      "Byte_vector"
      `Quick
      (make_test Parser.Byte_vector.encoding gen check)
end

let tests = [Byte_vector.tests]
