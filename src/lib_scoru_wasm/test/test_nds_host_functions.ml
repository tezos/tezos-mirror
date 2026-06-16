(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm NDS host functions
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_nds_host_functions.ml
    Subject:      Unit tests for the 12 NDS host functions (V6+),
                  exercised end-to-end through Eval.invoke with
                  the in-memory NDS backend.
*)

open Tztest
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils

(* -------------------------------------------------------------------- *)
(* Shared fixtures                                                      *)
(* -------------------------------------------------------------------- *)

(** Create a fresh in-memory NDS handle with one database (index 0). *)
let make_nds_handle () =
  let registry = Octez_riscv_nds_memory.Normal.Registry.create () in
  (match Octez_riscv_nds_memory.Normal.Registry.resize registry 1L with
  | Ok () -> ()
  | Error _ -> Stdlib.failwith "make_nds_handle: resize to 1 failed") ;
  Octez_riscv_nds_common.Nds.wrap
    (module Octez_riscv_nds_memory.Normal)
    registry

(** Build a module instance with WASM linear memory pre-loaded with
    the given strings starting at [src], using a V6 host function
    registry. Returns the module_reg, module_key, host function
    registry, and the NDS handle. *)
let make_nds_module_inst ?(version = Wasm_pvm_state.V6)
    ?(nds_host_functions_enabled = true) strings src =
  let module_reg, module_key, _registry =
    make_module_inst ~version strings src
  in
  (* Rebuild the registry with the requested feature-flag state so
     individual tests can exercise the no-op path. *)
  let host_funcs_registry =
    Host_funcs.registry ~version ~nds_host_functions_enabled ~write_debug:Noop
  in
  let nds = make_nds_handle () in
  (module_reg, module_key, host_funcs_registry, nds)

(** Invoke an NDS host function through [Eval.invoke] with Dual
    storage. Returns the result values list. *)
let invoke_nds ~module_reg ~module_key ~host_funcs_registry ~nds func_inst
    values =
  let open Lwt.Syntax in
  let* durable = make_durable [] in
  let storage = Eval_storage.dual durable nds in
  let+ _storage, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~storage
      host_funcs_registry
      func_inst
      values
  in
  result

(** Populate an NDS database with key-value pairs via direct API
    calls. *)
let populate_nds nds ~db_index pairs =
  List.iter
    (fun (key, value) ->
      match Octez_riscv_nds_common.Nds.set nds ~db_index ~key ~value with
      | Ok () -> ()
      | Error _ -> Stdlib.failwith "populate_nds: set failed")
    pairs

(** Store a string into WASM memory at the given offset and return
    its length as i32. *)
let store_string_in_memory ~module_reg offset s =
  let open Lwt.Syntax in
  let* memory = retrieve_memory module_reg in
  let+ () = Memory.store_bytes memory offset s in
  Int32.of_int (String.length s)

(** Read bytes from WASM memory at [offset] for [len] bytes. *)
let read_memory ~module_reg offset len =
  let open Lwt.Syntax in
  let* memory = retrieve_memory module_reg in
  Memory.load_bytes memory offset len

(* -------------------------------------------------------------------- *)
(* P1 — CRUD happy paths                                                *)
(* -------------------------------------------------------------------- *)

let test_nds_store_set_and_read () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "mykey" in
  let value = "hello_world" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  (* Write value into memory at offset 200 *)
  let value_offset = 200l in
  let* _len = store_string_in_memory ~module_reg value_offset value in
  let num_bytes = Int32.of_int (String.length value) in
  (* nds_store_set(db_index, key_offset, key_len, src, num_bytes) *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 value_offset);
          Num (I32 num_bytes);
        ]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Now read it back *)
  let dst = 300l in
  let max_bytes = Int32.of_int (String.length value) in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_read
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 0L);
          Num (I32 dst);
          Num (I32 max_bytes);
        ]
  in
  assert (result = Values.[Num (I32 (Int32.of_int (String.length value)))]) ;
  let* read_back = read_memory ~module_reg dst (String.length value) in
  assert (String.equal read_back value) ;
  Lwt.return_ok ()

let test_nds_store_write_and_read () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "wkey" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  (* First set a base value *)
  let base_value = "AAAA" in
  let src = 200l in
  let* _len = store_string_in_memory ~module_reg src base_value in
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 src);
          Num (I32 (Int32.of_int (String.length base_value)));
        ]
  in
  (* Write "BB" at offset 1 -> should yield "ABBA" *)
  let patch = "BB" in
  let patch_src = 250l in
  let* _len = store_string_in_memory ~module_reg patch_src patch in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_write
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 1L);
          Num (I32 patch_src);
          Num (I32 (Int32.of_int (String.length patch)));
        ]
  in
  (* nds_store_write returns the number of bytes written as i32 *)
  assert (result = Values.[Num (I32 2l)]) ;
  (* Read back and verify *)
  let dst = 300l in
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_read
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 0L);
          Num (I32 dst);
          Num (I32 4l);
        ]
  in
  let* read_back = read_memory ~module_reg dst 4 in
  assert (String.equal read_back "ABBA") ;
  Lwt.return_ok ()

let test_nds_store_exists_present () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "exists_key" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "val")] ;
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 db_index); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 1l)]) ;
  Lwt.return_ok ()

let test_nds_store_exists_absent () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "no_such_key" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 db_index); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  Lwt.return_ok ()

let test_nds_store_delete () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "del_key" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "value")] ;
  (* Delete *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_delete
      Values.[Num (I64 db_index); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Verify absent *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 db_index); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  Lwt.return_ok ()

let test_nds_store_value_size () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "size_key" in
  let value = "twelve_chars" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string value)] ;
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_value_size
      Values.[Num (I64 db_index); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 (Int32.of_int (String.length value)))]) ;
  Lwt.return_ok ()

let test_nds_store_write_extends () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "ext_key" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  (* Set initial 2-byte value *)
  let init_value = "AB" in
  let src = 200l in
  let* _len = store_string_in_memory ~module_reg src init_value in
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 src);
          Num (I32 (Int32.of_int (String.length init_value)));
        ]
  in
  (* Write "CD" at offset 2 -> extends to "ABCD" (4 bytes) *)
  let ext = "CD" in
  let ext_src = 250l in
  let* _len = store_string_in_memory ~module_reg ext_src ext in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_write
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 2L);
          Num (I32 ext_src);
          Num (I32 (Int32.of_int (String.length ext)));
        ]
  in
  (* Bytes written should be 2 (length of "CD") *)
  assert (result = Values.[Num (I32 2l)]) ;
  (* Read back full value *)
  let dst = 300l in
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_read
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 0L);
          Num (I32 dst);
          Num (I32 4l);
        ]
  in
  let* read_back = read_memory ~module_reg dst 4 in
  assert (String.equal read_back "ABCD") ;
  Lwt.return_ok ()

(* -------------------------------------------------------------------- *)
(* P2 — Registry operations                                             *)
(* -------------------------------------------------------------------- *)

let test_nds_registry_resize_grow () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  (* Default registry has 1 database (index 0). Grow to 2. *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 2L)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Verify database 1 is accessible by writing to it *)
  let key = "k" in
  let key_offset = 100l in
  let* key_len = store_string_in_memory ~module_reg key_offset key in
  let value = "v" in
  let src = 200l in
  let* _len = store_string_in_memory ~module_reg src value in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 1L);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 src);
          Num (I32 (Int32.of_int (String.length value)));
        ]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  Lwt.return_ok ()

let test_nds_registry_resize_shrink () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  (* Grow to 2, then shrink back to 1 *)
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 2L)]
  in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 1L)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Database 1 should now be out of bounds *)
  let key = "k" in
  let key_offset = 100l in
  let* key_len = store_string_in_memory ~module_reg key_offset key in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 1L); Num (I32 key_offset); Num (I32 key_len)]
  in
  (* -14 = Nds_database_out_of_bounds *)
  assert (result = Values.[Num (I32 (-14l))]) ;
  Lwt.return_ok ()

let test_nds_registry_copy () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "cpkey" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "cpval")] ;
  (* Grow to 2 databases *)
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 2L)]
  in
  (* Copy db 0 -> db 1 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_copy
      Values.[Num (I64 0L); Num (I64 1L)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Key should exist in db 1 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 1L); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 1l)]) ;
  Lwt.return_ok ()

let test_nds_registry_move () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "mvkey" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "mvval")] ;
  (* Grow to 2 databases *)
  let* _result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 2L)]
  in
  (* Move db 0 -> db 1 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_move
      Values.[Num (I64 0L); Num (I64 1L)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Key should NOT exist in db 0 (source cleared) *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 0L); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Key should exist in db 1 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 1L); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 1l)]) ;
  Lwt.return_ok ()

let test_nds_registry_clear () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "clrkey" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "clrval")] ;
  (* Clear db 0 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_clear
      Values.[Num (I64 0L)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  (* Key should no longer exist *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 0L); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  Lwt.return_ok ()

let test_nds_database_get_hash () =
  let open Lwt.Syntax in
  let dst = 500l in
  let max_bytes = 32l in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_database_get_hash
      Values.[Num (I64 0L); Num (I32 dst); Num (I32 max_bytes)]
  in
  assert (result = Values.[Num (I32 32l)]) ;
  let* hash_bytes = read_memory ~module_reg dst 32 in
  let all_zero =
    String.to_seq hash_bytes |> Seq.for_all (fun c -> Char.equal c '\000')
  in
  assert (not all_zero) ;
  Lwt.return_ok ()

(* -------------------------------------------------------------------- *)
(* P3 — Error paths                                                     *)
(* -------------------------------------------------------------------- *)

let test_nds_store_read_missing_key () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "missing" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_read
      Values.
        [
          Num (I64 0L);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 0L);
          Num (I32 300l);
          Num (I32 10l);
        ]
  in
  (* -3 = Store_not_a_value = Key_not_found *)
  assert (result = Values.[Num (I32 (-3l))]) ;
  Lwt.return_ok ()

let test_nds_store_write_bad_offset () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "offkey" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let db_index = 0L in
  (* Set a 3-byte value *)
  populate_nds nds ~db_index [(Bytes.of_string key, Bytes.of_string "abc")] ;
  (* Write at offset 100 (past end) *)
  let data = "X" in
  let src = 200l in
  let* _len = store_string_in_memory ~module_reg src data in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_write
      Values.
        [
          Num (I64 db_index);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 100L);
          Num (I32 src);
          Num (I32 1l);
        ]
  in
  (* -4 = Store_invalid_access = Offset_too_large *)
  assert (result = Values.[Num (I32 (-4l))]) ;
  Lwt.return_ok ()

let test_nds_store_value_size_missing () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "no_size" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_value_size
      Values.[Num (I64 0L); Num (I32 key_offset); Num (I32 key_len)]
  in
  (* -3 = Store_not_a_value = Key_not_found *)
  assert (result = Values.[Num (I32 (-3l))]) ;
  Lwt.return_ok ()

let test_nds_store_exists_bad_db_index () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "k" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  (* db_index 99 does not exist *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_exists
      Values.[Num (I64 99L); Num (I32 key_offset); Num (I32 key_len)]
  in
  (* -14 = Nds_database_out_of_bounds *)
  assert (result = Values.[Num (I32 (-14l))]) ;
  Lwt.return_ok ()

let test_nds_registry_resize_too_large () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  (* Attempt to resize from 1 to 100 (delta > 1 is not allowed) *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_resize
      Values.[Num (I64 100L)]
  in
  (* -15 = Nds_resize_too_large *)
  assert (result = Values.[Num (I32 (-15l))]) ;
  Lwt.return_ok ()

let test_nds_registry_copy_oob () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  (* Copy from db 0 to db 99 (out of bounds) *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_copy
      Values.[Num (I64 0L); Num (I64 99L)]
  in
  (* -14 = Nds_database_out_of_bounds *)
  assert (result = Values.[Num (I32 (-14l))]) ;
  Lwt.return_ok ()

let test_nds_registry_clear_oob () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  (* Clear db 99 (out of bounds) *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_registry_clear
      Values.[Num (I64 99L)]
  in
  (* -14 = Nds_database_out_of_bounds *)
  assert (result = Values.[Num (I32 (-14l))]) ;
  Lwt.return_ok ()

let test_nds_database_get_hash_small_buffer () =
  let open Lwt.Syntax in
  let dst = 500l in
  let max_bytes = 16l in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_database_get_hash
      Values.[Num (I64 0L); Num (I32 dst); Num (I32 max_bytes)]
  in
  assert (result = Values.[Num (I32 max_bytes)]) ;
  let* prefix = read_memory ~module_reg dst 16 in
  let all_zero =
    String.to_seq prefix |> Seq.for_all (fun c -> Char.equal c '\000')
  in
  assert (not all_zero) ;
  Lwt.return_ok ()

let test_nds_store_delete_missing_key () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "phantom" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  (* Delete a key that does not exist — should be a no-op returning 0 *)
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_delete
      Values.[Num (I64 0L); Num (I32 key_offset); Num (I32 key_len)]
  in
  assert (result = Values.[Num (I32 0l)]) ;
  Lwt.return_ok ()

let test_nds_database_get_hash_oob_dst () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [] 100l
  in
  let dst = 1_310_710l in
  let max_bytes = 64l in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_database_get_hash
      Values.[Num (I64 0L); Num (I32 dst); Num (I32 max_bytes)]
  in
  assert (result = Values.[Num (I32 (-6l))]) ;
  Lwt.return_ok ()

let test_nds_store_read_oob_dst () =
  let open Lwt.Syntax in
  let key_offset = 100l in
  let key = "k" in
  let value = "hello" in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [key] key_offset
  in
  let key_len = Int32.of_int (String.length key) in
  let value_offset = 200l in
  let* _ = store_string_in_memory ~module_reg value_offset value in
  let num_bytes = Int32.of_int (String.length value) in
  (* First populate with a known value *)
  let* _ =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 0L);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 value_offset);
          Num (I32 num_bytes);
        ]
  in
  (* Now read with dst close enough to memory end that the 5-byte read
     would overflow. *)
  let dst = 1_310_717l in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_read
      Values.
        [
          Num (I64 0L);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I64 0L);
          Num (I32 dst);
          Num (I32 num_bytes);
        ]
  in
  assert (result = Values.[Num (I32 (-6l))]) ;
  Lwt.return_ok ()

let test_nds_store_set_key_too_long () =
  let open Lwt.Syntax in
  (* The Rust NDS backend rejects oversized keys with [Key_too_long],
     mapped to [Store_key_too_large] (-1) by the host function layer.
     Use a key smaller than [input_output_max_size = 4096] so the
     memory load succeeds and we exercise the NDS-side check. *)
  let key_offset = 100l in
  let oversized_key = String.make 4000 'k' in
  let module_reg, module_key, host_funcs_registry, nds =
    make_nds_module_inst [oversized_key] key_offset
  in
  let key_len = Int32.of_int (String.length oversized_key) in
  let value_offset = Int32.add key_offset key_len in
  let value = "v" in
  let* _ = store_string_in_memory ~module_reg value_offset value in
  let num_bytes = 1l in
  let* result =
    invoke_nds
      ~module_reg
      ~module_key
      ~host_funcs_registry
      ~nds
      Host_funcs.Internal_for_tests.nds_store_set
      Values.
        [
          Num (I64 0L);
          Num (I32 key_offset);
          Num (I32 key_len);
          Num (I32 value_offset);
          Num (I32 num_bytes);
        ]
  in
  (* -1 = Store_key_too_large = Key_too_long *)
  assert (result = Values.[Num (I32 (-1l))]) ;
  Lwt.return_ok ()

(* -------------------------------------------------------------------- *)
(* P4 — Version gating & dispatch                                       *)
(* -------------------------------------------------------------------- *)

let nds_function_names =
  [
    "nds_registry_resize";
    "nds_registry_copy";
    "nds_registry_move";
    "nds_registry_clear";
    "nds_store_exists";
    "nds_store_read";
    "nds_store_write";
    "nds_store_set";
    "nds_store_delete";
    "nds_store_value_size";
    "nds_database_get_hash";
  ]

let test_nds_functions_not_available_before_v6 () =
  let pre_v6_versions = Wasm_pvm_state.[V0; V1; V2; V3; V4; V5] in
  List.iter
    (fun version ->
      List.iter
        (fun name ->
          let result = Host_funcs.lookup_opt ~version name in
          assert (Option.is_none result))
        nds_function_names)
    pre_v6_versions ;
  Lwt.return_ok ()

let test_nds_functions_available_v6 () =
  let v6_plus_versions = Wasm_pvm_state.[V6; VExperimental] in
  List.iter
    (fun version ->
      List.iter
        (fun name ->
          let result = Host_funcs.lookup_opt ~version name in
          assert (Option.is_some result))
        nds_function_names)
    v6_plus_versions ;
  Lwt.return_ok ()

let test_nds_functions_return_disabled_when_feature_flag_off () =
  (* On V6+, NDS host functions always resolve at link time
     regardless of the feature flag — the flag is enforced at
     runtime. With the flag off, every [nds_*] call returns the
     [Nds_not_enabled] error code without touching the NDS state.

     This is verified end-to-end by populating the NDS with a
     known key/value, then invoking [nds_store_value_size] under
     both flag states: with the flag on the call returns the real
     size; with the flag off it returns the [Nds_not_enabled]
     error code so the kernel can distinguish "disabled" from a
     successful zero-byte result. *)
  let open Lwt.Syntax in
  let key = "marker" in
  let value = Bytes.of_string "hello-world" in
  let key_offset = 100l in
  let key_len = Int32.of_int (String.length key) in
  let inputs = Values.[Num (I64 0L); Num (I32 key_offset); Num (I32 key_len)] in
  (* Flag on: real size is returned. *)
  let module_reg_on, module_key_on, registry_on, nds_on =
    make_nds_module_inst ~nds_host_functions_enabled:true [key] key_offset
  in
  populate_nds nds_on ~db_index:0L [(Bytes.of_string key, value)] ;
  let* result_on =
    invoke_nds
      ~module_reg:module_reg_on
      ~module_key:module_key_on
      ~host_funcs_registry:registry_on
      ~nds:nds_on
      Host_funcs.Internal_for_tests.nds_store_value_size
      inputs
  in
  assert (result_on = Values.[Num (I32 (Int32.of_int (Bytes.length value)))]) ;
  (* Flag off: same input, returns [Nds_not_enabled]. *)
  let module_reg_off, module_key_off, registry_off, nds_off =
    make_nds_module_inst ~nds_host_functions_enabled:false [key] key_offset
  in
  populate_nds nds_off ~db_index:0L [(Bytes.of_string key, value)] ;
  let* result_off =
    invoke_nds
      ~module_reg:module_reg_off
      ~module_key:module_key_off
      ~host_funcs_registry:registry_off
      ~nds:nds_off
      Host_funcs.Internal_for_tests.nds_store_value_size
      inputs
  in
  let expected = Host_funcs.Error.code Host_funcs.Error.Nds_not_enabled in
  assert (result_off = Values.[Num (I32 expected)]) ;
  Lwt.return_ok ()

let test_nds_dispatch_without_dual_storage_crashes () =
  let open Lwt.Syntax in
  let module_reg, module_key, host_funcs_registry, _nds =
    make_nds_module_inst [] 100l
  in
  let* durable = make_durable [] in
  let storage = Eval_storage.durable_only durable in
  let* raised =
    Lwt.catch
      (fun () ->
        let+ _storage, _result =
          Eval.invoke
            ~module_reg
            ~caller:module_key
            ~storage
            host_funcs_registry
            Host_funcs.Internal_for_tests.nds_registry_resize
            Values.[Num (I64 2L)]
        in
        false)
      (function
        | Eval.Nds_host_func_without_nds_storage -> Lwt.return true
        | _ -> Lwt.return false)
  in
  assert raised ;
  Lwt.return_ok ()

(* -------------------------------------------------------------------- *)
(* P5 — QCheck property tests                                           *)
(* -------------------------------------------------------------------- *)

let test_nds_store_set_read_roundtrip () =
  let open Lwt.Syntax in
  let test_one (key_s, value_s) =
    let key_offset = 100l in
    let module_reg, module_key, host_funcs_registry, nds =
      make_nds_module_inst [key_s] key_offset
    in
    let key_len = Int32.of_int (String.length key_s) in
    let db_index = 0L in
    let value_offset = 200l in
    let* _len = store_string_in_memory ~module_reg value_offset value_s in
    let num_bytes = Int32.of_int (String.length value_s) in
    (* Set *)
    let* result =
      invoke_nds
        ~module_reg
        ~module_key
        ~host_funcs_registry
        ~nds
        Host_funcs.Internal_for_tests.nds_store_set
        Values.
          [
            Num (I64 db_index);
            Num (I32 key_offset);
            Num (I32 key_len);
            Num (I32 value_offset);
            Num (I32 num_bytes);
          ]
    in
    assert (result = Values.[Num (I32 0l)]) ;
    (* Read back *)
    let dst = 400l in
    let max_bytes = num_bytes in
    let* result =
      invoke_nds
        ~module_reg
        ~module_key
        ~host_funcs_registry
        ~nds
        Host_funcs.Internal_for_tests.nds_store_read
        Values.
          [
            Num (I64 db_index);
            Num (I32 key_offset);
            Num (I32 key_len);
            Num (I64 0L);
            Num (I32 dst);
            Num (I32 max_bytes);
          ]
    in
    assert (result = Values.[Num (I32 num_bytes)]) ;
    let* read_back = read_memory ~module_reg dst (String.length value_s) in
    assert (String.equal read_back value_s) ;
    Lwt.return_unit
  in
  let gen =
    QCheck.Gen.(
      pair
        (string_size ~gen:printable (1 -- 64))
        (string_size ~gen:char (1 -- 256)))
  in
  (* Fixed seed for reproducibility — flip to Random.State.make_self_init ()
     locally to fuzz beyond the canonical samples. *)
  let rand = Random.State.make [|0|] in
  let samples = QCheck.Gen.generate ~rand ~n:20 gen in
  let* () = Lwt_list.iter_s test_one samples in
  Lwt.return_ok ()

let test_nds_store_write_read_roundtrip () =
  let open Lwt.Syntax in
  let test_one (key_s, value_s, offset) =
    let key_offset = 100l in
    let module_reg, module_key, host_funcs_registry, nds =
      make_nds_module_inst [key_s] key_offset
    in
    let key_len = Int32.of_int (String.length key_s) in
    let db_index = 0L in
    (* First create the key with enough initial bytes *)
    let init_len = offset + String.length value_s in
    let init_value = String.make init_len '\000' in
    let init_src = 200l in
    let* _len = store_string_in_memory ~module_reg init_src init_value in
    let* _result =
      invoke_nds
        ~module_reg
        ~module_key
        ~host_funcs_registry
        ~nds
        Host_funcs.Internal_for_tests.nds_store_set
        Values.
          [
            Num (I64 db_index);
            Num (I32 key_offset);
            Num (I32 key_len);
            Num (I32 init_src);
            Num (I32 (Int32.of_int init_len));
          ]
    in
    (* Write value_s at offset *)
    let write_src = 600l in
    let* _len = store_string_in_memory ~module_reg write_src value_s in
    let* _result =
      invoke_nds
        ~module_reg
        ~module_key
        ~host_funcs_registry
        ~nds
        Host_funcs.Internal_for_tests.nds_store_write
        Values.
          [
            Num (I64 db_index);
            Num (I32 key_offset);
            Num (I32 key_len);
            Num (I64 (Int64.of_int offset));
            Num (I32 write_src);
            Num (I32 (Int32.of_int (String.length value_s)));
          ]
    in
    (* Read back at the same offset *)
    let dst = 800l in
    let read_len = Int32.of_int (String.length value_s) in
    let* result =
      invoke_nds
        ~module_reg
        ~module_key
        ~host_funcs_registry
        ~nds
        Host_funcs.Internal_for_tests.nds_store_read
        Values.
          [
            Num (I64 db_index);
            Num (I32 key_offset);
            Num (I32 key_len);
            Num (I64 (Int64.of_int offset));
            Num (I32 dst);
            Num (I32 read_len);
          ]
    in
    assert (result = Values.[Num (I32 read_len)]) ;
    let* read_back = read_memory ~module_reg dst (String.length value_s) in
    assert (String.equal read_back value_s) ;
    Lwt.return_unit
  in
  let gen =
    QCheck.Gen.(
      map
        (fun (k, v, o) -> (k, v, o))
        (triple
           (string_size ~gen:printable (1 -- 64))
           (string_size ~gen:char (1 -- 128))
           (0 -- 64)))
  in
  (* Fixed seed for reproducibility — flip to Random.State.make_self_init ()
     locally to fuzz beyond the canonical samples. *)
  let rand = Random.State.make [|0|] in
  let samples = QCheck.Gen.generate ~rand ~n:20 gen in
  let* () = Lwt_list.iter_s test_one samples in
  Lwt.return_ok ()

(* -------------------------------------------------------------------- *)
(* Test registration                                                    *)
(* -------------------------------------------------------------------- *)

let tests =
  [
    (* P1 — CRUD *)
    tztest "nds_store_set and read roundtrip" `Quick test_nds_store_set_and_read;
    tztest "nds_store_write and read" `Quick test_nds_store_write_and_read;
    tztest "nds_store_exists present" `Quick test_nds_store_exists_present;
    tztest "nds_store_exists absent" `Quick test_nds_store_exists_absent;
    tztest "nds_store_delete" `Quick test_nds_store_delete;
    tztest "nds_store_value_size" `Quick test_nds_store_value_size;
    tztest "nds_store_write extends value" `Quick test_nds_store_write_extends;
    (* P2 — Registry *)
    tztest "nds_registry_resize grow" `Quick test_nds_registry_resize_grow;
    tztest "nds_registry_resize shrink" `Quick test_nds_registry_resize_shrink;
    tztest "nds_registry_copy" `Quick test_nds_registry_copy;
    tztest "nds_registry_move" `Quick test_nds_registry_move;
    tztest "nds_registry_clear" `Quick test_nds_registry_clear;
    tztest "nds_database_get_hash" `Quick test_nds_database_get_hash;
    (* P3 — Error paths *)
    tztest
      "nds_store_read missing key -> -3"
      `Quick
      test_nds_store_read_missing_key;
    tztest
      "nds_store_write bad offset -> -4"
      `Quick
      test_nds_store_write_bad_offset;
    tztest
      "nds_store_value_size missing -> -3"
      `Quick
      test_nds_store_value_size_missing;
    tztest
      "nds_store_exists bad db_index -> -14"
      `Quick
      test_nds_store_exists_bad_db_index;
    tztest
      "nds_registry_resize too large -> -15"
      `Quick
      test_nds_registry_resize_too_large;
    tztest "nds_registry_copy oob -> -14" `Quick test_nds_registry_copy_oob;
    tztest "nds_registry_clear oob -> -14" `Quick test_nds_registry_clear_oob;
    tztest
      "nds_database_get_hash small buffer -> truncated prefix"
      `Quick
      test_nds_database_get_hash_small_buffer;
    tztest
      "nds_store_delete missing key -> 0 (no-op)"
      `Quick
      test_nds_store_delete_missing_key;
    tztest
      "nds_database_get_hash dst near memory end -> -6 (OOB write)"
      `Quick
      test_nds_database_get_hash_oob_dst;
    tztest
      "nds_store_read dst near memory end -> -6 (OOB write)"
      `Quick
      test_nds_store_read_oob_dst;
    tztest
      "nds_store_set oversized key -> -1 (Key_too_long)"
      `Quick
      test_nds_store_set_key_too_long;
    (* P4 — Version gating & dispatch *)
    tztest
      "NDS functions not available before V6"
      `Quick
      test_nds_functions_not_available_before_v6;
    tztest
      "NDS functions available V6 and VExperimental"
      `Quick
      test_nds_functions_available_v6;
    tztest
      "NDS functions return Nds_not_enabled when feature flag is off"
      `Quick
      test_nds_functions_return_disabled_when_feature_flag_off;
    tztest
      "NDS dispatch without Dual storage crashes"
      `Quick
      test_nds_dispatch_without_dual_storage_crashes;
    (* P5 — QCheck properties *)
    tztest
      "nds_store_set/read roundtrip (QCheck)"
      `Quick
      test_nds_store_set_read_roundtrip;
    tztest
      "nds_store_write/read roundtrip (QCheck)"
      `Quick
      test_nds_store_write_read_roundtrip;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru wasm"
    [("NDS host functions", tests)]
  |> Lwt_main.run
