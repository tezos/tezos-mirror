(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Tezos_wasmer

let hex_string =
  Alcotest.testable
    (fun fmt x -> Format.fprintf fmt "%a" Hex.pp (Hex.of_string x))
    String.equal

let page_size = 0x10000

let alloc ?initial ~pages () =
  let length = pages * page_size in
  let raw = Memory.Array.make ?initial Ctypes.uint8_t length in
  Memory.{raw; min = Unsigned.UInt32.of_int pages; max = None}

let alloc_random ~pages =
  let length = pages * page_size in
  let mem = alloc ~pages () in
  for i = 0 to length - 1 do
    Random.int 256 |> Unsigned.UInt8.of_int |> Memory.Array.set mem.raw i
  done ;
  mem

let test_get_string_behaves_like_get () =
  let mem = alloc_random ~pages:1 in

  let check ~offset ~length =
    let a =
      String.init length (fun i ->
          Memory.get mem (offset + i) |> Unsigned.UInt8.to_int |> Char.chr)
    in
    let b = Memory.get_string mem ~address:offset ~length in
    Alcotest.check hex_string "expected %L, got %R" a b
  in

  (* Reading the entire memory page should behave the same. *)
  check ~offset:0x0 ~length:page_size ;

  (* Reading parts of the memory should yield the same result. *)
  check ~offset:(page_size / 4) ~length:(page_size / 2) ;

  (* Reading each byte individually should be the same for both. *)
  for i = 0 to page_size - 1 do
    check ~offset:i ~length:1
  done ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ = Memory.get mem (-10) in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.get_string mem ~address:(-10) ~length:1 in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string) ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ = Memory.get mem (page_size + 1) in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.get_string mem ~address:(page_size + 1) ~length:1 in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string) ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ = Memory.get mem page_size in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.get_string mem ~address:page_size ~length:1 in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string) ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ =
        for i = 0 to 9 do
          ignore (Memory.get mem (page_size + i) : Unsigned.uint8)
        done
      in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.get_string mem ~address:page_size ~length:10 in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string)

let test_set_string_behaves_like_set () =
  let mem = alloc ~initial:(Unsigned.UInt8.of_int 0) ~pages:1 () in

  let data = String.init 256 (fun _ -> Random.int 256 |> Char.chr) in

  (* Both mechanisms should write the same data. *)
  let base_addr = page_size / 4 in
  for i = 0 to String.length data - 1 do
    String.get_uint8 data i |> Unsigned.UInt8.of_int
    |> Memory.set mem (base_addr + i)
  done ;
  Memory.set_string mem ~address:base_addr ~data ;
  let read_data =
    Memory.get_string mem ~address:base_addr ~length:(String.length data)
  in
  Alcotest.check hex_string "expected %L, got %R" data read_data ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ =
        for i = 0 to String.length data - 1 do
          String.get_uint8 data i |> Unsigned.UInt8.of_int
          |> Memory.set mem (-10 + i)
        done
      in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.set_string mem ~address:(-10) ~data in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string) ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ =
        for i = 0 to String.length data - 1 do
          String.get_uint8 data i |> Unsigned.UInt8.of_int
          |> Memory.set mem (page_size + 1 + i)
        done
      in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.set_string mem ~address:(page_size + 1) ~data in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string) ;

  (* Going out of bounds fails the same way. *)
  let exn_get =
    try
      let _ =
        for i = 0 to String.length data - 1 do
          String.get_uint8 data i |> Unsigned.UInt8.of_int
          |> Memory.set mem (page_size + i)
        done
      in
      assert false
    with exn -> exn
  in
  let exn_get_string =
    try
      let _ = Memory.set_string mem ~address:page_size ~data in
      assert false
    with exn -> exn
  in
  assert (exn_get = exn_get_string)

(* Modules are given in binary form on purpose: this exercises the Wasmer
   validator directly, rather than wat2wasm, whose support for a proposal is
   independent from the features the engine enables. *)

(* (module (global i32 (i32.const 3))) *)
let const_global_module =
  "\x00\x61\x73\x6d\x01\x00\x00\x00\x06\x06\x01\x7f\x00\x41\x03\x0b"

(* (module (global i32 (i32.add (i32.const 1) (i32.const 2)))) -- a global whose
   initialiser is an extended constant expression. *)
let extended_const_global_module =
  "\x00\x61\x73\x6d\x01\x00\x00\x00\x06\x09\x01\x7f\x00\x41\x01\x41\x02\x6a\x0b"

(* (module (memory 1) (func (memory.copy (i32.const 0) (i32.const 1)
                                         (i32.const 2)))) -- bulk memory.
   rustc emits memory.copy for wasm32-unknown-unknown by default, so every
   kernel contains this and it must compile. *)
let bulk_memory_module =
  "\x00\x61\x73\x6d\x01\x00\x00\x00\x01\x04\x01\x60\x00\x00\x03\x02\x01\x00\x05\x03\x01\x00\x01\x0a\x0e\x01\x0c\x00\x41\x00\x41\x01\x41\x02\xfc\x0a\x00\x00\x0b"

let compiles code =
  let config =
    let open Config in
    if is_compiler_available CRANELIFT then {compiler = CRANELIFT}
    else {compiler = SINGLEPASS}
  in
  let store = Store.create (Engine.create config) in
  match Module.create store Module.Binary code with
  | modul ->
      Module.delete modul ;
      true
  | exception _ -> false

(* The proposals Wasmer accepts must stay in sync with the ones the Octez
   WebAssembly interpreter implements: a module the fast execution accepts but
   the interpreter rejects makes the two disagree on whether a kernel is valid,
   and one it rejects but the interpreter accepts stops the node executing
   kernels at all.

   Both directions are pinned here because both have gone wrong. Wasmer's own
   defaults are not the right set -- [wasmer_features_new] enables proposals
   the interpreter lacks, and which ones changes between releases -- while the
   [Features] setters are order-sensitive, [reference_types true] enabling bulk
   memory and [bulk_memory false] disabling reference types. *)
let test_wasm_proposals () =
  let check msg expected code =
    Alcotest.(check bool) msg expected (compiles code)
  in
  check "a plain constant global initialiser compiles" true const_global_module ;
  (* Accepted: the interpreter implements bulk memory, and rustc emits it. *)
  check "a bulk memory instruction compiles" true bulk_memory_module ;
  (* Rejected: the interpreter does not implement extended const. *)
  check
    "an extended constant expression is rejected"
    false
    extended_const_global_module

let tests =
  [
    ( "Memory",
      [
        ("get_string behaves like get", `Quick, test_get_string_behaves_like_get);
        ("set_string behaves like set", `Quick, test_set_string_behaves_like_set);
      ] );
    ("Config", [("WebAssembly proposals", `Quick, test_wasm_proposals)]);
  ]

let () = Alcotest.run ~__FILE__ "Wasmer" tests
