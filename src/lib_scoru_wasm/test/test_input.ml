(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
    Component:    Lib_scoru_wasm input
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Input$"
    Subject:      Input tests for the tezos-scoru-wasm library
*)

open Tztest
open Lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm

let write_input () =
  let open Lwt.Syntax in
  let input = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input
      {
        rtype = 1l;
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  let* () =
    Input_buffer.enqueue
      input
      {
        rtype = 1l;
        raw_level = 2l;
        message_counter = Z.of_int 3;
        payload = Bytes.of_string "hello";
      }
  in
  assert (Input_buffer.num_elements input = Z.of_int 2) ;
  let* () =
    Lwt.try_bind
      (fun () ->
        Input_buffer.enqueue
          input
          {
            rtype = 1l;
            raw_level = 2l;
            message_counter = Z.of_int 2;
            payload = Bytes.of_string "hello";
          })
      (fun _ -> assert false)
      (function
        | Input_buffer.Cannot_store_an_earlier_message -> Lwt.return ()
        | _ -> assert false)
  in
  Lwt.return Result.return_unit

let read_input () =
  let open Lwt.Syntax in
  let lim = Types.(MemoryType {min = 100l; max = Some 1000l}) in
  let memory = Memory.alloc lim in
  let input_buffer = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input_buffer
      {
        rtype = 1l;
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  assert (Input_buffer.num_elements input_buffer = Z.one) ;
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memories =
    Tezos_webassembly_interpreter.Instance.Vector.cons
      memory
      module_inst.memories
  in
  let module_inst = {module_inst with memories} in
  let* result =
    Host_funcs.Internal_for_tests.aux_write_input_in_memory
      ~input_buffer
      ~module_inst
      ~rtype_offset:0l
      ~level_offset:4l
      ~id_offset:10l
      ~dst:50l
      ~max_bytes:36000l
  in
  let* memory =
    Tezos_webassembly_interpreter.Instance.Vector.get 0l module_inst.memories
  in
  assert (Input_buffer.num_elements input_buffer = Z.zero) ;
  assert (result = 5) ;
  let* m = Memory.load_bytes memory 0l 1 in
  assert (m = "\001") ;
  let* m = Memory.load_bytes memory 4l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 10l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  Lwt.return @@ Result.return_unit

let test_host_fun () =
  let open Lwt.Syntax in
  let input = Input_buffer.alloc () in
  let* () =
    Input_buffer.enqueue
      input
      {
        rtype = 1l;
        raw_level = 2l;
        message_counter = Z.of_int 2;
        payload = Bytes.of_string "hello";
      }
  in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memories =
    Lazy_vector.LwtInt32Vector.cons
      (Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}))
      module_inst.memories
  in
  let module_inst = {module_inst with memories} in
  let values =
    Values.
      [
        Num (I32 0l); Num (I32 4l); Num (I32 10l); Num (I32 50l); Num (I32 3600l);
      ]
  in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let* result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      host_funcs_registry
      ~input
      Host_funcs.Internal_for_tests.read_input
      values
  in
  let* module_inst = Instance.resolve_module_ref module_reg module_key in
  let* memory = Lazy_vector.LwtInt32Vector.get 0l module_inst.memories in
  assert (Input_buffer.num_elements input = Z.zero) ;
  let* m = Memory.load_bytes memory 0l 1 in
  assert (m = "\001") ;
  let* m = Memory.load_bytes memory 4l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 10l 1 in
  assert (m = "\002") ;
  let* m = Memory.load_bytes memory 50l 5 in
  assert (m = "hello") ;
  assert (result = Values.[Num (I32 5l)]) ;
  Lwt.return @@ Result.return_unit

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary

let empty_tree () =
  let open Lwt_syntax in
  let* index = Context.init "/tmp" in
  let empty_store = Context.empty index in
  return @@ Context.Tree.empty empty_store

type Lazy_containers.Lazy_map.tree += Tree of Context.tree

module Tree : Tree_encoding.TREE with type tree = Context.tree = struct
  type tree = Context.tree

  include Context.Tree

  let select = function
    | Tree t -> t
    | _ -> raise Tree_encoding.Incorrect_tree_type

  let wrap t = Tree t
end

module Wasm = Wasm_pvm.Make (Tree)
module Tree_encoding = Tree_encoding.Make (Tree)

let current_tick_encoding =
  Tree_encoding.value ["wasm"; "current_tick"] Data_encoding.z

let status_encoding =
  Tree_encoding.value ["input"; "consuming"] Data_encoding.bool

let floppy_encoding =
  Tree_encoding.value
    ["gather-floppies"; "status"]
    Gather_floppies.internal_status_encoding

let level_encoding =
  Tree_encoding.value ["input"; "level"] Bounded.Int32.NonNegative.encoding

let id_encoding = Tree_encoding.value ["input"; "id"] Data_encoding.z

let inp_encoding = Tree_encoding.value ["input"; "0"; "1"] Data_encoding.string

let zero =
  WithExceptions.Option.get ~loc:__LOC__ (Bounded.Int32.NonNegative.of_int32 0l)

(** Artificial initialization. Under normal circumstances the changes in
    [current_tick], [gather_floppies] and [status] will be done by the other
    PVM operations. for example the [origination_kernel_loading_step] in
    Gather_floppies will initialize both the [current_tick] and the
    [gather_floppies] *)
let initialise_tree () =
  let open Lwt_syntax in
  let* empty_tree = empty_tree () in
  let boot_sector =
    Data_encoding.Binary.to_string_exn
      Gather_floppies.origination_message_encoding
      (Complete_kernel (Bytes.of_string "some boot sector"))
  in
  let* tree =
    Wasm.Internal_for_tests.initial_tree_from_boot_sector
      ~empty_tree
      boot_sector
  in

  let* tree = Tree_encoding.encode current_tick_encoding Z.zero tree in
  let* tree =
    Tree_encoding.encode
      floppy_encoding
      Gather_floppies.Not_gathering_floppies
      tree
  in
  let* tree = Tree_encoding.encode status_encoding true tree in
  Lwt.return tree

(** Artificial initialization of the raw_level and message id. Again, in practice
    these will normally be initialized  by the origination step and modified by
    subsequent read_input steps.*)
let add_level_id tree =
  let open Lwt_syntax in
  let* tree = Tree_encoding.encode level_encoding zero tree in
  let* tree = Tree_encoding.encode id_encoding Z.zero tree in
  Lwt.return tree

(** Simple test checking get_info after the initialization. Note that we also
    check that if the tree has no last_input_read set the response to [get_info]
    has [None] as [last_input_read] *)
let test_get_info () =
  let open Lwt_syntax in
  let* tree = initialise_tree () in
  let expected_info =
    let open Wasm_pvm_sig in
    let last_input_read = Some {inbox_level = zero; message_counter = Z.zero} in
    {current_tick = Z.one; last_input_read; input_request = Input_required}
  in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info.last_input_read = None) ;
  let* tree = add_level_id tree in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info = expected_info) ;
  Lwt_result_syntax.return_unit

(** Tests that, after set_input th resulting tree decodes to the correct values.
    In particular it does check that [get_info] produces the expected value. *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3524
   Enable/fix test.
   This test needs to be modifying in order to work with the WASM PVM.
*)
let _test_set_input () =
  let open Lwt_syntax in
  let* tree = initialise_tree () in
  let* tree = add_level_id tree in
  let* tree = Tree_encoding.encode status_encoding false tree in
  let* tree = Tree_encoding.encode status_encoding true tree in
  let* tree =
    Wasm.set_input_step
      {inbox_level = zero; message_counter = Z.of_int 1}
      "hello"
      tree
  in
  let* result_input = Tree_encoding.decode inp_encoding tree in
  let* waiting_for_input = Tree_encoding.decode status_encoding tree in
  let* current_tick = Tree_encoding.decode current_tick_encoding tree in
  let expected_info =
    let open Wasm_pvm_sig in
    let last_input_read = Some {inbox_level = zero; message_counter = Z.zero} in
    {
      current_tick = Z.(succ one);
      last_input_read;
      input_request = No_input_required;
    }
  in
  let* actual_info = Wasm.get_info tree in
  assert (actual_info = expected_info) ;
  assert (current_tick = Z.one) ;
  assert (not waiting_for_input) ;
  assert (result_input = "hello") ;
  Lwt_result_syntax.return_unit

let tests =
  [
    tztest "Write input" `Quick write_input;
    tztest "Read input" `Quick read_input;
    tztest "Host read input" `Quick test_host_fun;
    tztest "Get info" `Quick test_get_info;
  ]
