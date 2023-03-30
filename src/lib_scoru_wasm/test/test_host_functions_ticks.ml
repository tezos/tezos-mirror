(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Test
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Host functions ticks$"
    Subject:      Tickification of host functions tests for the \
                  tezos-scoru-wasm library
*)

open Tezos_scoru_wasm
open Wasm_utils
open Tztest_helper

(* This function return a function to modifiy `write_debug` in the registry, and
   a function to revert the change. Due to how the registry works right now (a
   mutable global), this in necessary otherwise the next tests might fail. *)
let register_new_write_debug ~version added_ticks =
  let open Lwt_syntax in
  let name =
    match Host_funcs.Internal_for_tests.write_debug with
    | Tezos_webassembly_interpreter.Func.HostFunc (_, name) -> name
    | _ -> Stdlib.failwith "write_debug is not an host function"
  in
  let current_write_debug =
    Tezos_webassembly_interpreter.Host_funcs.lookup
      ~global_name:name
      (Host_funcs.registry ~version ~write_debug:Noop)
  in
  let alternative_write_debug =
    match current_write_debug with
    | Tezos_webassembly_interpreter.Host_funcs.Host_func f ->
        let write_debug input output durable mem args =
          let* durable, res, ticks = f input output durable mem args in
          return (durable, res, Z.add ticks (Z.of_int added_ticks))
        in
        Tezos_webassembly_interpreter.Host_funcs.Host_func write_debug
    | Reveal_func _ -> Stdlib.failwith "write_debug is not a reveal function"
  in
  let register impl () =
    Tezos_webassembly_interpreter.Host_funcs.register
      ~global_name:name
      impl
      (Host_funcs.registry ~version ~write_debug:Noop)
  in
  (register alternative_write_debug, register current_write_debug)

let test_tickified_host_function ~version () =
  let open Lwt_syntax in
  let run_until_result () =
    let module_ =
      {|
(module
 (import "smart_rollup_core" "write_debug"
         (func $write_debug (param i32 i32)))
 ;; Durable keys
 (data (i32.const 100) "hello")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (local $hello_address i32)
       (local $hello_length i32)

       (local.set $hello_address (i32.const 100))
       (local.set $hello_length (i32.const 5))

       (call $write_debug (local.get $hello_address) (local.get $hello_length))
       (nop)
       )
 )
|}
    in
    let* tree = initial_tree ~version ~from_binary:false module_ in
    (* Feeding it with one input *)
    let* tree = set_empty_inbox_step 0l tree in
    (* running until waiting for input *)
    let* _tree, elapsed_ticks = eval_to_result tree in
    return elapsed_ticks
  in
  let* elapsed_tick_before = run_until_result () in
  let update_registry, revert_registry =
    register_new_write_debug ~version 100
  in
  update_registry () ;
  let* elapsed_tick_with_alt_write_debug = run_until_result () in
  assert (Int64.add elapsed_tick_before 100L = elapsed_tick_with_alt_write_debug) ;
  revert_registry () ;
  return_ok_unit

let tests =
  tztests_with_pvm
    ~versions:[V0; V1]
    [("Test tickified host function", `Quick, test_tickified_host_function)]
