(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
    Component:    Lib_scoru_wasm_fast
    Invocation:   dune exec src/lib_scoru_wasm/fast/test/main.exe -- --file test_memory_access.ml
    Subject:      Tests for the tezos-scoru-wasm library
*)

module Wasmer = Tezos_wasmer
module Preimage_map = Map.Make (String)
module Memory_access_fast = Tezos_scoru_wasm_fast.Memory_access.Wasmer
module Memory_access_slow =
  Tezos_scoru_wasm.Host_funcs.Memory_access_interpreter

let are_equivalent initial_content f_ref f_wasmer =
  let open Lwt.Syntax in
  let result_ref =
    Lwt_main.run
    @@ Lwt.catch
         (fun () ->
           let ref_mem = Partial_memory.of_list initial_content in
           let* ret = f_ref ref_mem in
           let+ final_content = Partial_memory.to_list ref_mem in
           Result.ok @@ (ret, final_content))
         (fun exn ->
           Lwt.return @@ Result.error
           @@ Memory_access_slow.exn_to_error
                ~default:
                  Tezos_scoru_wasm.Host_funcs.Error.Generic_invalid_access
                exn)
  in

  let result_wasmer =
    let open Tezos_wasmer.Memory in
    Lwt_main.run
    @@ Lwt.catch
         (fun () ->
           let wasmer_mem = Internal_for_tests.of_list initial_content in
           let* ret = f_wasmer wasmer_mem in
           let final_content = Internal_for_tests.to_list wasmer_mem in
           Lwt.return @@ Result.ok @@ (ret, final_content))
         (fun exn ->
           Lwt.return @@ Result.error
           @@ Memory_access_fast.exn_to_error
                ~default:
                  Tezos_scoru_wasm.Host_funcs.Error.Generic_invalid_access
                exn)
  in

  let mem_equal = List.equal Unsigned.UInt8.equal in

  Result.equal
    ~ok:(fun (ret_ref, content_ref) (ret_wasmer, content_wasmer) ->
      ret_ref = ret_wasmer && mem_equal content_ref content_wasmer)
    ~error:Stdlib.( = )
    result_ref
    result_wasmer

let test_store_bytes =
  let open Gen in
  let open QCheck2.Gen in
  QCheck2.Test.make
    ~count:50
    ~name:"store_bytes behaves the same on both memory implementations"
    (triple mem_content int32 string)
    (fun (memory, address, data) ->
      are_equivalent
        memory
        (fun mem -> Memory_access_slow.store_bytes mem address data)
        (fun mem -> Memory_access_fast.store_bytes mem address data))

let test_load_bytes =
  let open Gen in
  let open QCheck2.Gen in
  QCheck2.Test.make
    ~count:50
    ~name:"load_bytes behaves the same on both memory implementations"
    (triple mem_content int32 int)
    (fun (memory, address, size) ->
      are_equivalent
        memory
        (fun mem -> Memory_access_slow.load_bytes mem address size)
        (fun mem -> Memory_access_fast.load_bytes mem address size))

let test_store_num =
  let open Gen in
  let open QCheck2.Gen in
  QCheck2.Test.make
    ~count:50
    ~name:"store_num behaves the same on both memory implementations"
    (quad mem_content int32 int32 num)
    (fun (memory, address, offset, num) ->
      are_equivalent
        memory
        (fun mem -> Memory_access_slow.store_num mem address offset num)
        (fun mem -> Memory_access_fast.store_num mem address offset num))

let tests : unit Alcotest_lwt.test_case list =
  List.map
    Qcheck_helpers.to_alcotest_lwt
    [test_store_bytes; test_load_bytes; test_store_num]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru-wasm-fast"
    [("Memory access", tests)]
  |> Lwt_main.run
