(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Micheline binary encoding (decode depth guard)
    Invocation:   dune exec src/lib_micheline/test/test_encoding_depth.exe
    Subject:      Deeply-nested Micheline binary input must be rejected with a
                  clean decode error instead of overflowing the native stack
                  (L2-1767), while in-limit input is decoded unchanged.
*)

(* A test encoding whose primitives are single bytes, mirroring the protocol's
   [Script.expr_encoding] (which uses a uint8 primitive tag). *)
let enc =
  Micheline_encoding.canonical_encoding ~variant:"test" Data_encoding.uint8

(* Binary form of a Micheline expression made of [nodes] nested nodes:
   [nodes - 1] single-argument primitives (tag 5 = "Prim, 1 arg, no annots",
   followed by a 1-byte primitive) wrapping a leaf [Int 0] (tag 0 followed by
   the Zarith encoding of 0, a single 0x00 byte). Total size is [2 * nodes]
   bytes, and the nesting depth (number of recursive [Mu] unfoldings) is
   [nodes]. The bytes are built directly to avoid recursing in the binary
   writer, which has the same unbounded recursion as the reader. *)
let chain_bytes nodes =
  let b = Bytes.create (2 * nodes) in
  for i = 0 to nodes - 2 do
    Bytes.set_uint8 b (2 * i) 0x05 (* Prim, 1 arg, no annots *) ;
    Bytes.set_uint8 b ((2 * i) + 1) 0x00 (* primitive tag *)
  done ;
  Bytes.set_uint8 b (2 * (nodes - 1)) 0x00 (* Int *) ;
  Bytes.set_uint8 b ((2 * (nodes - 1)) + 1) 0x00 (* Zarith 0 *) ;
  b

let decodes nodes =
  match Data_encoding.Binary.of_bytes_opt enc (chain_bytes nodes) with
  | Some _ -> "decoded"
  | None -> "rejected"

(* Depth far beyond both the limit and any realistic stack: must return a clean
   [None] rather than raising [Stack_overflow] and crashing the process. *)
let%expect_test "deeply nested input is rejected without stack overflow" =
  Printf.printf "%s" (decodes 5_000_000) ;
  [%expect {| rejected |}]

(* Boundary: exactly at the limit decodes; one level over is rejected. *)
let%expect_test "depth boundary is deterministic" =
  Printf.printf "at_limit=%s over_limit=%s" (decodes 50_000) (decodes 50_001) ;
  [%expect {| at_limit=decoded over_limit=rejected |}]

(* In-limit values are decoded exactly as before the guard was added. *)
let%expect_test "shallow input round-trips unchanged" =
  let node = Data_encoding.Binary.of_bytes_opt enc (chain_bytes 10) in
  (match node with
  | None -> Printf.printf "unexpected rejection"
  | Some c ->
      let reencoded = Data_encoding.Binary.to_bytes_exn enc c in
      Printf.printf "roundtrip=%b" (Bytes.equal reencoded (chain_bytes 10))) ;
  [%expect {| roundtrip=true |}]
