(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Protocol Library
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/pbt/main.exe \
                  -- --file test_sc_rollup_inbox.ml
    Subject:      Smart rollup inbox
*)

open Protocol
open Qcheck2_helpers

let gen_block_hash =
  let open QCheck2.Gen in
  let gen =
    let+ b = bytes_fixed_gen Block_hash.size in
    Block_hash.of_bytes_exn b
  in
  (* This is not beautiful, but there is currently no other way to
     remove the shrinker. *)
  make_primitive
    ~gen:(fun rand -> generate1 ~rand gen)
    ~shrink:(fun _ -> Seq.empty)

let gen_time =
  let open QCheck2.Gen in
  let+ s = int64 in
  Time.Protocol.of_seconds s

let gen_add_info_per_level =
  let open QCheck2.Gen in
  let* predecessor_timestamp = gen_time in
  let* predecessor = gen_block_hash in
  return (predecessor_timestamp, predecessor)

let test_add_info_per_level =
  QCheck2.Test.make
    ~count:10_000
    ~name:"test_add_info_per_level"
    gen_add_info_per_level
  @@ fun (predecessor_timestamp, predecessor) ->
  (* Test that we can indeed serialize the [Info_per_level] message for these
     inputs *)
  let _bytes =
    Sc_rollup_inbox_message_repr.info_per_level_serialized
      ~predecessor_timestamp
      ~predecessor
  in
  true

let tests = [test_add_info_per_level]

let () = Alcotest.run ~__FILE__ Protocol.name [("safety", qcheck_wrap tests)]
