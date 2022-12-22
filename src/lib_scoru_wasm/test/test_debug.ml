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
                    -- test "^Debug$"
    Subject:      Debug facilities tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

let write_debug ~debug ~init ~values memories =
  let input = Input_buffer.alloc () in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memories =
    List.fold_left
      (fun memories memory -> Lazy_vector.Int32Vector.cons memory memories)
      module_inst.memories
      memories
  in
  let module_inst = {module_inst with memories} in

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;
  Eval.invoke
    ~module_reg
    ~caller:module_key
    (if debug then
     Host_funcs.all_debug
       ~write_debug:(Printer (fun str -> Lwt_io.printf "%s" str))
    else Host_funcs.all)
    ~input
    ~init
    Host_funcs.Internal_for_tests.write_debug
    values

let test_write_debug_ok () =
  let open Lwt_syntax in
  let memories_ok =
    [Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l})]
  in
  let values = Values.[Num (I32 4l); Num (I32 10l)] in
  let* _, result_noop =
    write_debug ~debug:false ~init:false ~values memories_ok
  in

  let* _, result_alternative =
    write_debug ~debug:true ~init:false ~values memories_ok
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let test_write_debug_init () =
  let open Lwt_syntax in
  let memories =
    [Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l})]
  in
  let values = Values.[Num (I32 4l); Num (I32 10l)] in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ = write_debug ~debug:false ~init:true ~values memories in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ = write_debug ~debug:true ~init:true ~values memories in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  return_ok_unit

let test_write_debug_too_many_memories () =
  let open Lwt_syntax in
  let memories_two =
    [
      Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l});
      Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l});
    ]
  in
  let values = Values.[Num (I32 4l); Num (I32 10l)] in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ = write_debug ~debug:false ~init:false ~values memories_two in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ = write_debug ~debug:false ~init:false ~values memories_two in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  return_ok_unit

let test_write_debug_invalid_length () =
  let open Lwt_syntax in
  let memory = Memory.alloc (MemoryType Types.{min = 0l; max = Some 1l}) in
  let offset = 0l in
  let length = 10_000l in
  assert (Memory.bound memory < Int64.of_int32 (Int32.add offset length)) ;

  let values = Values.[Num (I32 offset); Num (I32 length)] in
  let* _, result_noop = write_debug ~debug:false ~init:false ~values [memory] in

  let* _, result_alternative =
    write_debug ~debug:true ~init:false ~values [memory]
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let test_write_debug_invalid_offset () =
  let open Lwt_syntax in
  let memory = Memory.alloc (MemoryType Types.{min = 0l; max = Some 1l}) in
  let offset = 10_000l in
  assert (Memory.bound memory < Int64.of_int32 offset) ;
  let values = Values.[Num (I32 offset); Num (I32 10l)] in
  let* _, result_noop = write_debug ~debug:false ~init:false ~values [memory] in
  let* _, result_alternative =
    write_debug ~debug:true ~init:false ~values [memory]
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let tests =
  [
    tztest "debug on correct inputs and memory" `Quick test_write_debug_ok;
    tztest
      "debug on more than one memory"
      `Quick
      test_write_debug_too_many_memories;
    tztest "debug during init" `Quick test_write_debug_init;
    tztest
      "debug with inputs outside of the memory"
      `Quick
      test_write_debug_invalid_length;
    tztest
      "debug with inputs outside of the memory"
      `Quick
      test_write_debug_invalid_offset;
  ]
