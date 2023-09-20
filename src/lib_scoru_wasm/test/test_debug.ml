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
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_debug.ml
    Subject:      Debug facilities tests for the tezos-scoru-wasm library
*)

open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

let write_debug ~version ~debug ~init ~values memories =
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
    (Host_funcs.registry
       ~version
       ~write_debug:
         (if debug then
          Printer (fun str -> Lwt.return @@ Format.printf "%s" str)
         else Noop))
    ~input
    ~init
    Host_funcs.Internal_for_tests.write_debug
    values

let test_write_debug_ok ~version () =
  let open Lwt_syntax in
  let memories_ok =
    [Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l})]
  in
  let values = Values.[Num (I32 4l); Num (I32 10l)] in
  let* _, result_noop =
    write_debug ~version ~debug:false ~init:false ~values memories_ok
  in

  let* _, result_alternative =
    write_debug ~version ~debug:true ~init:false ~values memories_ok
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let test_write_debug_init ~version () =
  let open Lwt_syntax in
  let memories =
    [Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l})]
  in
  let values = Values.[Num (I32 4l); Num (I32 10l)] in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ =
          write_debug ~version ~debug:false ~init:true ~values memories
        in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ =
          write_debug ~version ~debug:true ~init:true ~values memories
        in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  return_ok_unit

let test_write_debug_too_many_memories ~version () =
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
        let* _, _ =
          write_debug ~version ~debug:false ~init:false ~values memories_two
        in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  let* () =
    Lwt.catch
      (fun () ->
        let* _, _ =
          write_debug ~version ~debug:false ~init:false ~values memories_two
        in
        assert false)
      (function Eval.Crash _ -> Lwt.return_unit | _ -> assert false)
  in
  return_ok_unit

let test_write_debug_invalid_length ~version () =
  let open Lwt_syntax in
  let memory = Memory.alloc (MemoryType Types.{min = 1l; max = Some 1l}) in
  let offset = 0l in
  let length = 100_000l in
  assert (Memory.bound memory < Int64.of_int32 (Int32.add offset length)) ;

  let values = Values.[Num (I32 offset); Num (I32 length)] in
  let* _, result_noop =
    write_debug ~version ~debug:false ~init:false ~values [memory]
  in

  let* _, result_alternative =
    write_debug ~version ~debug:true ~init:false ~values [memory]
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let test_write_debug_invalid_offset ~version () =
  let open Lwt_syntax in
  let memory = Memory.alloc (MemoryType Types.{min = 1l; max = Some 1l}) in
  let offset = 100_000l in
  assert (Memory.bound memory < Int64.of_int32 offset) ;
  let values = Values.[Num (I32 offset); Num (I32 10l)] in
  let* _, result_noop =
    write_debug ~version ~debug:false ~init:false ~values [memory]
  in
  let* _, result_alternative =
    write_debug ~version ~debug:true ~init:false ~values [memory]
  in
  assert (result_noop = result_alternative) ;
  return_ok_unit

let test_read_mem_outside_of_bounds ~version:_ () =
  let open Lwt_syntax in
  let check_read_mem_empty memory =
    let bound = Memory.bound memory |> I64.to_int_u |> I32.of_int_u in
    let offset_outside_bound = Int32.add bound 10l in
    let offset_in_bound = Int32.sub bound 10l in
    (* The correct offset is always set 10 bytes before the upper bound of the
       memory. Reading the 5 bytes should succeed, 50 will fail and -100_000 too
       with a potential overflow. *)
    let length_ok = 5l in
    let length_outside = 50l in
    let length_32bits = -100_000l in

    (* Should fill the end of the memory. *)
    let in_memory = String.init 10 (fun i -> Char.chr i) in
    let* () = Memory.store_bytes memory offset_in_bound in_memory in
    (* Reading in the bounds of the memory, should work. *)
    let* res =
      Host_funcs.Aux.read_mem_for_debug
        ~memory
        ~src:offset_in_bound
        ~num_bytes:length_ok
    in
    assert (res = String.sub in_memory 0 (Int32.to_int length_ok)) ;

    (* The offset is not in the bounds of the memory, it should return an error
       code. *)
    let* res =
      Host_funcs.Aux.read_mem_for_debug
        ~memory
        ~src:offset_outside_bound
        ~num_bytes:length_ok
    in
    assert (String.starts_with ~prefix:"Error code:" res) ;

    (* The offset is in the bounds of the memory but the length would make it
       read outside of the memory, it should return an error code. *)
    let* res =
      Host_funcs.Aux.read_mem_for_debug
        ~memory
        ~src:offset_in_bound
        ~num_bytes:length_outside
    in
    (* The truncated result is exactly the message put at the end of the memory,
       hence not of size `length_outside`. *)
    assert (String.starts_with ~prefix:"Error code:" res) ;

    (* The offset is in the bounds of the memory but the length would make it
       read outside of the memory and potentially overflow, it should return a
       an error code. *)
    let+ res =
      Host_funcs.Aux.read_mem_for_debug
        ~memory
        ~src:offset_in_bound
        ~num_bytes:length_32bits
    in
    (* The truncated result is exactly the message put at the end of the memory,
       hence not of size `length_outside`. *)
    assert (String.starts_with ~prefix:"Error code:" res)
  in

  let small_memory =
    Memory.alloc (MemoryType Types.{min = 1l; max = Some 1l})
  in
  let* () = check_read_mem_empty small_memory in

  (* Invalid addresses are in [-65536l; -1l] *)
  let memory_almost_4gb =
    Memory.alloc (MemoryType Types.{min = 0x0ffffl; max = None})
  in
  let* () = check_read_mem_empty memory_almost_4gb in
  return_ok_unit

let tests =
  Tztest_helper.tztests_with_all_pvms
    [
      ("debug on correct inputs and memory", `Quick, test_write_debug_ok);
      ( "debug on more than one memory",
        `Quick,
        test_write_debug_too_many_memories );
      ("debug during init", `Quick, test_write_debug_init);
      ( "debug with inputs outside of the memory (invalid length)",
        `Quick,
        test_write_debug_invalid_length );
      ( "debug with inputs outside of the memory (invalid offset)",
        `Quick,
        test_write_debug_invalid_offset );
      ( "Check reading at invalid position of the memory doesn't fail and \
         truncates if possible",
        `Quick,
        test_read_mem_outside_of_bounds );
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Debug", tests)]
  |> Lwt_main.run
